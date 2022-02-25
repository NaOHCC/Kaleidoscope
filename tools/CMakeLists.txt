set(LLVM_LINK_COMPONENTS 
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  ScalarOpts
  Support
  native
)
set(CMAKE_EXE_LINKER_FLAGS "-Xlinker -export-dynamic")
add_kaleidoscope_tool(kaleidoscope toy.cpp)