## Grammar
```c++
// top ::= definition | external | expression | ';'

// toplevelexpr ::= expression # toplevelexpr = expression
// definition ::= 'def' prototype expression
// external ::= 'extern' prototype
// prototype ::= id '(' id* ')' | (binary LETTER number? (id, id))

// expression ::= primary binoprhs
// binoprhs ::= ('+' primary)*
// primary ::= identifierexpr | numberexpr | parenexpr
// identifierexpr ::= identifier | ( identifier '(' expression* ')' )
// parenexpr ::= '(' expression ')'
// numberexpr ::= number
// ifexpr ::= 'if' expression 'then' expression 'else' expression

// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
```

## JIT
函数定义和调用也可以工作，但最后一行出现了非常错误的情况。函数调用看起来有效，但是出现报错，发生了什么事？正如您可能从API中猜到的那样，Module是JIT的分配单元，而testfunc是包含匿名表达式的同一模块的一部分。当我们从JIT中删除该模块以释放用于匿名表达式的内存时，我们同时删除了testfunc的定义。然后，当我们试图第二次调用testfunc时，JIT再也找不到它了。
解决此问题的最简单方法是将匿名表达式放在与剩余函数定义的不同的模块中。JIT将愉快地跨模块边界解决函数调用，只要每个被调用的函数都有一个原型，并且在调用之前被添加到JIT中。通过将匿名表达式放在不同的模块中，我们可以删除它，而不会影响剩余的函数。

哇，JIT怎么知道SIN和COS的？答案出奇的简单：KaleidoscopeJIT有一个简单明了的符号解析规则，它用来查找任何给定模块中没有的符号：首先，它搜索已经添加到JIT的所有模块(从最新到最旧)，以找到最新的定义。如果在JIT中找不到定义，它将退回到在Kaleidoscope进程本身上调用“dlsym(”sin“)”。因为“sin”是在JIT的地址空间中定义的，所以它只是给模块中的调用打了补丁，直接调用sin的libm版本。但在某些情况下，这甚至会更进一步：因为sin和cos是标准数学函数的名称，所以当使用常量调用函数时，Constant folder将直接计算函数调用的正确结果，就像上面的“sin(1.0)”一样。
在未来，我们将看到调整此符号解析规则能够被用来启用各种有用的功能，从安全性(限制可用于JIT代码的符号集)到基于符号名称的动态代码生成，甚至惰性编译（lazy compilation）。