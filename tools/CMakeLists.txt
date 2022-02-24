set(LLVM_LINK_COMPONENTS 
  AggressiveInstCombine Analysis AsmParser
  BitWriter CodeGen Core Coroutines IPO IRReader
  InstCombine Instrumentation MC ObjCARCOpts Remarks
  ScalarOpts Support Target TransformUtils Vectorize
  Passes)

add_kaleidoscope_tool(kaleidoscope toy.cpp)