cmake_minimum_required(VERSION 3.14)
project(V3MathExpressionGenerator LANGUAGES C)

include_directories(extern)
include_directories(extern/Array/src)
include_directories(src)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/grammar.mpc DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable(V3MathExpressionGenerator
    src/main.c
    src/lang/lang.c
    src/lang/transpile.c
    src/lang/types.c
    extern/mpc/mpc.c
    extern/Array/src/array.c
)