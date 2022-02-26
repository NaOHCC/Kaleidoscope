#include "KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::orc;
using namespace std;
//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
enum Token
{
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5,
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,
    tok_binary = -11,
    tok_unary = -12,
};

static string IdentifierStr; // token为tok_identifier时填充
static double NumVal;        // token为tok_number时填充

// 从stdin返回一个新token
static int gettok()
{
    static int LastChar = ' ';

    // 跳过空白
    while (isspace(LastChar))
        LastChar = getchar();

    // 识别关键字
    if (isalpha(LastChar))
    {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "then")
            return tok_then;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "for")
            return tok_for;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "binary")
            return tok_binary;
        if (IdentifierStr == "unary")
            return tok_unary;
        return tok_identifier;
    }
    // 识别数字
    if (isdigit(LastChar) || LastChar == '.')
    {
        string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getchar();

        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }
    // 处理注释
    if (LastChar == '#')
    {
        do
        {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF)
            return gettok();
    }

    if (LastChar == EOF)
        return tok_eof;
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

class ExprAST
{
public:
    virtual ~ExprAST() {}
    virtual Value *codegen() = 0;
};

// 数字表达式的AST类, 形如 "1.0"
class NumberExprAST : public ExprAST
{
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
    virtual Value *codegen() override;
};

// 变量引用的表达式AST类, 形如 "a"
class VariableExprAST : public ExprAST
{
    string Name;

public:
    VariableExprAST(const string &Name) : Name(Name) {}
    virtual Value *codegen() override;
    const string getName() const { return Name; }
};

// 二元运算符的AST类
class BinaryExprAST : public ExprAST
{
    char Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char op, unique_ptr<ExprAST> LHS, unique_ptr<ExprAST> RHS)
        : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    virtual Value *codegen() override;
};

// 函数调用表达式的AST类
class CallExprAST : public ExprAST
{
    string Callee; // 函数名
    vector<unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const string &Callee, vector<unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    virtual Value *codegen() override;
};

// 函数原型的AST类
// 捕获参数名称及是否是个运算符
// example: def binary> 10 (LHS RHS) RHS < LHS;
// def unary!(v) if v then 0 else 1;
class PrototypeAST
{
    string Name;
    vector<string> Args;
    bool IsOperator;
    unsigned Precedence; // 二元运算符的优先级

public:
    PrototypeAST(const string Name, vector<string> Args,
                 bool IsOperator = false, unsigned Prec = 0)
        : Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
          Precedence(Prec) {}
    const string getName() const { return Name; }
    Function *codegen();

    bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
    bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

    char getOperatorName() const
    {
        assert(isUnaryOp() || isBinaryOp());
        return Name[Name.size() - 1];
    }

    unsigned getBinaryPrecedence() const { return Precedence; }
};

// 函数的AST类, 表示定义本身
class FunctionAST
{
    unique_ptr<PrototypeAST> Proto;
    unique_ptr<ExprAST> Body;

public:
    FunctionAST(unique_ptr<PrototypeAST> Proto, unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    Function *codegen();
};

// IfExprAST - if/then/else 的类
class IfExprAST : public ExprAST
{
    unique_ptr<ExprAST> Cond, Then, Else;

public:
    IfExprAST(unique_ptr<ExprAST> Cond, unique_ptr<ExprAST> Then, unique_ptr<ExprAST> Else)
        : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    Value *codegen() override;
};

// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST
{
    string VarName;
    unique_ptr<ExprAST> Start, End, Step, Body;

public:
    ForExprAST(const string &VarName, unique_ptr<ExprAST> Start,
               unique_ptr<ExprAST> End, unique_ptr<ExprAST> Step,
               unique_ptr<ExprAST> Body)
        : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
          Step(std::move(Step)), Body(std::move(Body)) {}

    Value *codegen() override;
};

// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST
{
    char Opcode;
    unique_ptr<ExprAST> Operand;

public:
    UnaryExprAST(char Opcode, unique_ptr<ExprAST> Operand)
        : Opcode(Opcode), Operand(std::move(Operand)) {}

    Value *codegen() override;
};

static int CurTok; //缓冲
static int getNextToken()
{
    return CurTok = gettok();
}

unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "LogError: %s\n", Str);
    return nullptr;
}

// 原型错误
unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

// 解析表达式

// numberexpr ::= number
static unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextToken(); // 吃掉当前number
    return std::move(Result);
}
static unique_ptr<ExprAST> ParseExpression();

// 括号表达式
// parenexpr ::= '(' expression ')'
static unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // 吃掉 (
    auto V = ParseExpression();
    if (!V)
        return nullptr;
    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken();
    return V;
}

// identifierexpr
//   ::= identifier
//   ::= identifier '(' expression* ')'
static unique_ptr<ExprAST> ParseIdentifierExpr()
{
    string IdName = IdentifierStr;
    getNextToken();

    if (CurTok != '(') // 引用变量
        return make_unique<VariableExprAST>(IdName);

    // 函数调用
    getNextToken(); // ear (
    vector<unique_ptr<ExprAST>> Args;
    if (CurTok != ')')
    {
        while (1)
        {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')')
                break;

            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    getNextToken(); // eat )

    return make_unique<CallExprAST>(IdName, std::move(Args));
}

// ifexpr ::= 'if' expression 'then' expression 'else' expression
static unique_ptr<ExprAST> ParseIfExpr()
{
    getNextToken(); // eat the if.

    // 条件部分
    auto Cond = ParseExpression();
    if (!Cond)
        return nullptr;

    if (CurTok != tok_then)
        return LogError("expected then");
    getNextToken(); // eat the then

    auto Then = ParseExpression();
    if (!Then)
        return nullptr;

    if (CurTok != tok_else)
        return LogError("expected else");

    getNextToken();

    auto Else = ParseExpression();
    if (!Else)
        return nullptr;

    return make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                  std::move(Else));
}

// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static unique_ptr<ExprAST> ParseForExpr()
{
    getNextToken(); // eat the for
    if (CurTok != tok_identifier)
        return LogError("expected identifier after for");

    string IdName = IdentifierStr;
    getNextToken(); // eat identifier

    if (CurTok != '=')
        return LogError("expected '=' after for");
    getNextToken(); // eat '='

    auto Start = ParseExpression();
    if (!Start)
        return nullptr;
    if (CurTok != ',')
        return LogError("expected ',' after for start value");
    getNextToken();

    auto End = ParseExpression();
    if (!End)
        return nullptr;

    // step值可选
    unique_ptr<ExprAST> Step;
    if (CurTok == ',')
    {
        getNextToken();
        Step = ParseExpression();
        if (!Step)
            return nullptr;
    }

    if (CurTok != tok_in)
        return LogError("expected 'in' after for");
    getNextToken(); // eat 'in'

    auto Body = ParseExpression();
    if (!Body)
        return nullptr;

    return make_unique<ForExprAST>(IdName, std::move(Start),
                                   std::move(End), std::move(Step),
                                   std::move(Body));
}

// primary
//   ::= identifierexpr
//   ::= numberexpr
//   ::= parenexpr
static unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
    default:
        return LogError("unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    case tok_if:
        return ParseIfExpr();
    case tok_for:
        return ParseForExpr();
    }
}

// unary
//   ::= primary
//   ::= '!' unary
static unique_ptr<ExprAST> ParseUnary()
{
    // 如果当前token不是运算符(不是负数), 则它必须是primary expr
    if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
        return ParsePrimary();

    // 如果当前是一元运算符, 读取它
    int Opc = CurTok;
    getNextToken();
    if (auto Operand = ParseUnary())
        return make_unique<UnaryExprAST>(Opc, std::move(Operand));
    return nullptr;
}

// 存储运算符优先级
static map<char, int> BinopPrecedence;
// 获取当前token优先级
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;

    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

static unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, unique_ptr<ExprAST> LHS);

// expression
//   ::= primary binoprhs
//
unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParseUnary();
    if (!LHS)
        return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

// binoprhs
//   ::= ('+' primary)*
unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, unique_ptr<ExprAST> LHS)
{
    while (1)
    {
        int TokPrec = GetTokPrecedence();
        if (TokPrec < ExprPrec)
            return LHS;

        // 现在CurTok是binop
        int Binop = CurTok;
        getNextToken(); // eat binop

        // 解析运算符右侧的表达式
        auto RHS = ParseUnary();
        if (!RHS)
            return nullptr;
        int NextPrec = GetTokPrecedence();
        // TODO
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }
        //合并 LHS/RHS
        LHS = make_unique<BinaryExprAST>(Binop, std::move(LHS), std::move(RHS));
    }
}

// prototype
//   ::= id '(' id* ')'
//   ::= binary LETTER number? (id, id)
//   ::= unary LETTER (id)
static unique_ptr<PrototypeAST> ParsePrototype()
{
    string FnName;

    unsigned Kind = 0; // 0 = id, 1 = unary, 2 = binary
    unsigned BinaryPrecedence = 30;

    switch (CurTok)
    {
    default:
        return LogErrorP("Expected function name in prototype");
    case tok_identifier:
        FnName = IdentifierStr;
        Kind = 0;
        getNextToken();
        break;
    case tok_unary:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected unary operator");
        FnName = "unary";
        FnName += (char)CurTok;
        Kind = 1;
        getNextToken();
        break;
    case tok_binary:
        getNextToken();
        if (!isascii(CurTok))
            return LogErrorP("Expected binary operator");
        FnName = "binary";
        FnName += (char)CurTok;
        Kind = 2;
        getNextToken();

        // 如果定义了优先级
        if (CurTok == tok_number)
        {
            if (NumVal < 1 || NumVal > 100)
                return LogErrorP("Invalid precedence: must be 1..100");
            BinaryPrecedence = (unsigned)NumVal;
            getNextToken();
        }
        break;
    }

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    // 读取参数名
    vector<string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    getNextToken(); // eat )

    // 验证运算符的参数与运算符性质匹配
    if (Kind && ArgNames.size() != Kind)
        return LogErrorP("Invalid number of operands for operator");

    return make_unique<PrototypeAST>(FnName, std::move(ArgNames), Kind != 0, BinaryPrecedence);
}

// definition ::= 'def' prototype expression
static unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken(); // eat def.
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;
    if (auto E = ParseExpression())
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

// external ::= 'extern' prototype
static unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern.
    return ParsePrototype();
}

// toplevelexpr ::= expression
static unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // 创建匿名函数
        auto Proto = make_unique<PrototypeAST>("__anon_expr", vector<string>());
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
static unique_ptr<LLVMContext> TheContext;
static unique_ptr<Module> TheModule;
static unique_ptr<IRBuilder<>> Builder;
static map<string, AllocaInst *> NamedValues; // 当前作用域的符号表, 现在它保存变量的内存地址而不是值
static unique_ptr<legacy::FunctionPassManager> TheFPM;
static unique_ptr<KaleidoscopeJIT> TheJIT;
static map<string, unique_ptr<PrototypeAST>> FunctionProtos; // 保存函数的最新原型

static ExitOnError ExitOnErr;

// 代码生成时错误
Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

/**
 * @brief Get the Function object
 * 在TheModule中搜索现有的函数声明，如果没有找到，
 * 则退回到从FunctionProtos生成新的声明
 * @param Name 函数名
 * @return Function*
 */
Function *getFunction(string Name)
{
    // 首先查找该函数是否在当前模块中
    if (auto *F = TheModule->getFunction(Name))
        return F;

    // 如果没有, 检查是否可以从一些现有的原型代码生成声明
    auto FI = FunctionProtos.find(Name);
    if (FI != FunctionProtos.end())
        return FI->second->codegen();

    // 不存在原型, 返回nullptr
    return nullptr;
}

/**
 * @brief 在函数入口块中创建一条alloca指令, 这用于可变变量
 *
 * @param TheFunction
 * @param VarName
 * @return AllocaInst*
 */
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, StringRef VarName)
{
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), 0, VarName);
}

/**
 * @brief 创建并返回一个ConstantFP.
 * 数值常量由ConstantFP类表示，该类在内部保存APFloat中的数值
 * (APFloat可以保存任意精度的浮点常量
 *
 * @return Value* ConstantFP数值常量
 */
Value *NumberExprAST::codegen()
{
    return ConstantFP::get(*TheContext, APFloat(Val));
}

/**
 * @brief 检查map中是否有指定的名称(如果没有，
 * 则表示引用了一个未知变量)并返回该变量的值
 *
 * @return Value* 变量的值
 */
Value *VariableExprAST::codegen()
{
    // 在函数中查找变量
    Value *V = NamedValues[Name];
    if (!V)
        LogErrorV("Unknown variable name");
    // 从内存中加载变量值
    return Builder->CreateLoad(V, Name.c_str());
}

/**
 * @brief 递归生成运算符左侧的代码, 接着是右侧的, 最后计算二元表达式结果
 *
 * @return Value*
 */
Value *BinaryExprAST::codegen()
{
    // 特殊情况, 不将LHS作为表达式发射
    // 只允许x=expr, 而不是(x+1)=expr
    if (Op == '=')
    {
        // 要求LHS是一个id
        VariableExprAST *LHSE = dynamic_cast<VariableExprAST *>(LHS.get());
        if (!LHSE)
            return LogErrorV("destination of '=' must be a variable");
        // 生成右边IR
        Value *Val = RHS->codegen();
        if (!Val)
            return nullptr;

        // 查看变量
        Value *Variable = NamedValues[LHSE->getName()];
        if (!Variable)
            return LogErrorV("Unknown variable name");

        Builder->CreateStore(Val, Variable);
        return Val;
    }

    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // 将布尔值 0/1 转换为 0.0 或 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        break;
    }

    // 如果不是内置的运算符, 则它是用户自定义的. 生成调用代码
    Function *F = getFunction(string("binary") + Op);
    assert(F && "binary operator not found!");

    Value *Ops[2] = {L, R};
    return Builder->CreateCall(F, Ops, "binop");
}

/**
 * @brief 函数调用代码生成
 *
 * @return Value*
 */
Value *CallExprAST::codegen()
{
    // 在全局模块表中查找名称
    Function *CalleeF = getFunction(Callee);
    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // 检查参数数量
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

/**
 * @brief 生成函数声明的IR
 *
 * @return Function*
 */
Function *PrototypeAST::codegen()
{
    // 创建函数类型: double(double,double) etc.
    vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));

    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
    // 创建函数原型的IR
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // 为所有参数设置名称
    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);
    return F;
}

Function *FunctionAST::codegen()
{
    // 将原型的所有权转移到 FunctionProtos 映射
    // 但保留对它的引用以供下面使用
    auto &P = *Proto;
    FunctionProtos[Proto->getName()] = std::move(Proto);
    Function *TheFunction = getFunction(P.getName());
    if (!TheFunction)
        return nullptr;

    // 如果是运算符, 则添加它
    if (P.isBinaryOp())
        BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

    // 创建一个基本块以开始插入IR
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // 在 NamedValues 中记录函数参数
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
    {
        // 给当前参数分配alloca
        AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

        // 把初始值存入alloca
        Builder->CreateStore(&Arg, Alloca);

        // 把变量加入符号表
        NamedValues[string(Arg.getName())] = Alloca;
    }

    // 生成函数体
    if (Value *RetVal = Body->codegen())
    {
        // 结束函数
        Builder->CreateRet(RetVal);
        // 验证生成的代码
        verifyFunction(*TheFunction);

        // 优化函数
        TheFPM->run(*TheFunction);

        return TheFunction;
    }

    // 错误处理, 删除生成的函数
    TheFunction->eraseFromParent();
    return nullptr;
}

Value *IfExprAST::codegen()
{
    Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;

    // 通过比较不等于 0.0 将条件转换为布尔值
    CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    // if语句所在的函数
    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // 给then和else创建块, 在TheFunction函数末尾插入then块, 其它两个块尚未插入
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont"); // 汇合处

    // 创建一个进行分支选择的IR
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // 移动插入点到then块中, 重头开始插入
    // 发射then值
    Builder->SetInsertPoint(ThenBB);

    Value *ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    // then块结束, 创建无条件分支跳转, 合并块
    Builder->CreateBr(MergeBB);

    // then块中可能会更改Builder发射的块, 例如包含一个嵌套if表达式
    // 递归调用codegen()可能会改变当前block, 需要设置Phi节点的代码为最新值
    ThenBB = Builder->GetInsertBlock();

    // 发射else块
    TheFunction->getBasicBlockList().push_back(ElseBB); // 将else块添加到函数中
    Builder->SetInsertPoint(ElseBB);

    Value *ElseV = Else->codegen();
    if (!ElseV)
        return nullptr;

    Builder->CreateBr(MergeBB);
    // 递归调用codegen()可能会改变当前block, 需要设置Phi节点的代码为最新值
    ElseBB = Builder->GetInsertBlock();

    // 发射merge块
    TheFunction->getBasicBlockList().push_back(MergeBB); // 将merge块添加到函数中
    Builder->SetInsertPoint(MergeBB);
    PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    // 给phi节点设置block/value对
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

Value *ForExprAST::codegen()
{

    Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // 给entry块中的变量创建一个alloca
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

    // 发射start部分的IR(即初始化变量的表达式), 这个范围内没有变量
    Value *StartVal = Start->codegen();
    if (!StartVal)
        return nullptr;

    // 将变量值存储到alloca中
    Builder->CreateStore(StartVal, Alloca);

    // 为循环头的开始设置基本块, 在当前块后插入
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

    // 显式从当前块跳转到LoopBB块
    Builder->CreateBr(LoopBB);

    // 开始在LoopBB中插入
    Builder->SetInsertPoint(LoopBB);

    // 在循环中, 变量被定义为PHI节点, 如果隐藏了外部同名变量
    // 出循环后要恢复它的值, 所有现在保存它
    AllocaInst *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Alloca;

    // 发射循环体IR, 这与任何其他expr一样, 可以更改当前BB
    // 请注意, 我们忽略了主体计算的值, 但不允许出现错误
    if (!Body->codegen())
        return nullptr;

    // 发射step值
    Value *StepVal = nullptr;
    if (Step)
    {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    }
    else
    {
        // 使用默认步长
        StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
    }

    // 计算end条件
    Value *EndCond = End->codegen();
    if (!EndCond)
        return nullptr;

    // 重新载入变量, 递增, 写回变量. 这里处理了循环体改变变量的情况
    Value *CurVar = Builder->CreateLoad(Alloca);
    Value *NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
    Builder->CreateStore(NextVar, Alloca);

    // 通过比较将不等于0.0的条件转换为布尔值
    EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

    // 创建after loop块并插入
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

    // 将条件分支插入LoopEndBB末尾
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

    // 循环体结束, 以后其它代码都插入到AfterBB
    Builder->SetInsertPoint(AfterBB);

    // 恢复隐藏的变量
    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);

    // for循环的代码生成总是返回0.0
    return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

Value *UnaryExprAST::codegen()
{
    Value *OperandV = Operand->codegen();
    if (!OperandV)
        return nullptr;

    Function *F = getFunction(string("unary") + Opcode);
    if (!F)
        return LogErrorV("Unknown unary operator");

    return Builder->CreateCall(F, OperandV, "unop");
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager()
{
    // Open a new context and module.
    TheContext = make_unique<LLVMContext>();
    TheModule = make_unique<Module>("my cool jit", *TheContext);
    TheModule->setDataLayout(TheJIT->getDataLayout());

    Builder = make_unique<IRBuilder<>>(*TheContext);

    // 创建pass管理器
    TheFPM = make_unique<legacy::FunctionPassManager>(TheModule.get());

    // mem2reg pass
    TheFPM->add(createPromoteMemoryToRegisterPass());
    // 窥孔优化和bit-twiddling optzns
    TheFPM->add(createInstructionCombiningPass());
    // 重新关联表达式
    TheFPM->add(createReassociatePass());
    // 消除子表达式
    TheFPM->add(createGVNPass());
    // CFG优化
    TheFPM->add(createCFGSimplificationPass());

    TheFPM->doFinalization();
}

static void HandleDefinition()
{
    if (auto FnAST = ParseDefinition())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            // 将新函数定义传递到JIT, 并创建一个新模块
            ExitOnErr(TheJIT->addModule(ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
            InitializeModuleAndPassManager();
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern()
{
    if (auto ProtoAST = ParseExtern())
    {
        if (auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read extern: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
            // 添加新原型
            FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

/**
 * @brief 每个表达式都是一个Module, 即不以def, extern 开头的表达式
 *
 */
static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    // 两次遍历, 先生成AST, 再遍历生成IR
    if (auto FnAST = ParseTopLevelExpr())
    {
        // 两次遍历, 先生成AST, 再遍历生成IR
        if (FnAST->codegen())
        {
            // 创建一个 ResourceTracker 来跟踪分配给匿名表达式的JIT内存
            // 这样我们可以在执行后释放它。
            auto RT = TheJIT->getMainJITDylib().createResourceTracker();

            // 将IR添加到JIT中, 生成可执行代码
            auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
            ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
            // 开启新模块存放后续代码
            InitializeModuleAndPassManager();

            // 搜索JIT中的__anon_expr符号
            auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
            fprintf(stderr, "\n");
            double (*FP)() = (double (*)())(intptr_t)ExprSymbol.getAddress();
            fprintf(stderr, "\nEvaluated to %f\n", FP());

            // 删除JIT中的匿名模块
            ExitOnErr(RT->remove());
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X)
{
    fputc((char)X, stderr);
    return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X)
{
    fprintf(stderr, "%f\n", X);
    return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main()
{
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['='] = 2;
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());

    // Make the module, which holds all the code.
    InitializeModuleAndPassManager();

    // Run the main "interpreter loop" now.
    MainLoop();

    // Print out all of the generated code.
    TheModule->print(errs(), nullptr);

    return 0;
}

//===============================================================================
//===============================================================================
//===============================================================================
//===============================================================================

// //===----------------------------------------------------------------------===//
// // Top-Level parsing
// //===----------------------------------------------------------------------===//

// static void HandleDefinition()
// {
//     if (ParseDefinition())
//     {
//         fprintf(stderr, "Parsed a function definition.\n");
//     }
//     else
//     {
//         // Skip token for error recovery.
//         getNextToken();
//     }
// }

// static void HandleExtern()
// {
//     if (ParseExtern())
//     {
//         fprintf(stderr, "Parsed an extern\n");
//     }
//     else
//     {
//         // Skip token for error recovery.
//         getNextToken();
//     }
// }

// // 将顶层表达式计算为匿名函数
// static void HandleTopLevelExpression()
// {
//     // Evaluate a top-level expression into an anonymous function.
//     if (ParseTopLevelExpr())
//     {
//         fprintf(stderr, "Parsed a top-level expr\n");
//     }
//     else
//     {
//         // Skip token for error recovery.
//         getNextToken();
//     }
// }

// // top ::= definition | external | expression | ';'
// static void MainLoop()
// {
//     while (true)
//     {
//         fprintf(stderr, "ready> ");
//         switch (CurTok)
//         {
//         case tok_eof:
//             return;
//         case ';': // ignore top-level semicolons.
//             getNextToken();
//             break;
//         case tok_def:
//             HandleDefinition();
//             break;
//         case tok_extern:
//             HandleExtern();
//             break;
//         default:
//             HandleTopLevelExpression();
//             break;
//         }
//     }
// }

// //===----------------------------------------------------------------------===//
// // Main driver code.
// //===----------------------------------------------------------------------===//

// int main()
// {
//     // Install standard binary operators.
//     // 1 is lowest precedence.
//     BinopPrecedence['<'] = 10;
//     BinopPrecedence['+'] = 20;
//     BinopPrecedence['-'] = 20;
//     BinopPrecedence['*'] = 40; // highest.

//     // Prime the first token.
//     fprintf(stderr, "ready> ");
//     getNextToken();

//     // Run the main "interpreter loop" now.
//     MainLoop();

//     return 0;
// }