## Grammar
```c++
// top ::= definition | external | expression | ';'

// toplevelexpr ::= expression # toplevelexpr = expression
// definition ::= 'def' prototype expression
// external ::= 'extern' prototype
// prototype ::= id '(' id* ')'

// expression ::= primary binoprhs
// binoprhs ::= ('+' primary)*
// primary ::= identifierexpr | numberexpr | parenexpr
// identifierexpr ::= identifier | ( identifier '(' expression* ')' )
// parenexpr ::= '(' expression ')'
// numberexpr ::= number

```