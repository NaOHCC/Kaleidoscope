#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
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
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
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
class PrototypeAST
{
    string Name;
    vector<string> Args;

public:
    PrototypeAST(const string Name, vector<string> Args)
        : Name(Name), Args(std::move(Args)) {}
    const string getName() const { return Name; }
    Function *codegen();
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
    }
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
    auto LHS = ParsePrimary();
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
        auto RHS = ParsePrimary();
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
static unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    // 读取参数名
    vector<string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    getNextToken(); // eat )

    return make_unique<PrototypeAST>(FnName, std::move(ArgNames));
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
        auto Proto = make_unique<PrototypeAST>("", vector<string>());
        return make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static unique_ptr<Module> TheModule;
static map<string, Value *> NamedValues; // 当前作用域的符号表
static unique_ptr<legacy::FunctionPassManager> TheFPM;

// 代码生成时错误
Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
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
    return ConstantFP::get(TheContext, APFloat(Val));
}

/**
 * @brief 检查map中是否有指定的名称(如果没有，则表示引用了一个未知变量)并返回该变量的值
 *
 * @return Value* 变量的值
 */
Value *VariableExprAST::codegen()
{
    // 在函数中查找变量
    Value *V = NamedValues[Name];
    if (!V)
        LogErrorV("Unknown variable name");
    return V;
}

/**
 * @brief 递归生成运算符左侧的代码, 接着是右侧的, 最后计算二元表达式结果
 *
 * @return Value*
 */
Value *BinaryExprAST::codegen()
{
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        return Builder.CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder.CreateFSub(L, R, "subtmp");
    case '*':
        return Builder.CreateFMul(L, R, "multmp");
    case '<':
        L = Builder.CreateFCmpULT(L, R, "cmptmp");
        // 将布尔值 0/1 转换为 0.0 或 1.0
        return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
}

/**
 * @brief 函数调用代码生成
 *
 * @return Value*
 */
Value *CallExprAST::codegen()
{
    // 在全局模块表中查找名称
    Function *CalleeF = TheModule->getFunction(Callee);
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

    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

/**
 * @brief 生成函数声明的IR
 *
 * @return Function*
 */
Function *PrototypeAST::codegen()
{
    // 创建函数类型: double(double,double) etc.
    vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));

    FunctionType *FT = FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
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
    // 从之前 extern 声明中检查现有函数
    Function *TheFunction = TheModule->getFunction(Proto->getName());

    // 如果没有, 则生成一个函数原型
    if (!TheFunction)
        TheFunction = Proto->codegen();

    if (!TheFunction)
        return nullptr;

    if (!TheFunction->empty())
        return (Function *)LogErrorV("Function cannot be redefined.");

    // 创建一个基本块以开始插入IR
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    // 在 NamedValues 中记录函数参数
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
        NamedValues[string(Arg.getName())] = &Arg;

    // 生成函数体
    if (Value *RetVal = Body->codegen())
    {
        // 结束函数
        Builder.CreateRet(RetVal);
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

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager()
{
    // Open a new context and module.
    TheModule = std::make_unique<Module>("my cool jit", TheContext);

    // 创建pass管理器
    TheFPM = make_unique<legacy::FunctionPassManager>(TheModule.get());

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
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr())
    {
        // 两次遍历, 先生成AST, 再遍历生成IR
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read top-level expression:");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            // Remove the anonymous expression.
            FnIR->eraseFromParent();
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
// Main driver code.
//===----------------------------------------------------------------------===//

int main()
{
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

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