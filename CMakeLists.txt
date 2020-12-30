cmake_minimum_required(VERSION 3.14)
project(V3MathExpressionGenerator LANGUAGES C)

include_directories(extern)
include_directories(src)

add_executable(V3MathExpressionGenerator
    src/main.c
    src/lang/AST/AST_Constructor.c
    extern/mpc/mpc.c
    extern/Array/src/array.c
)