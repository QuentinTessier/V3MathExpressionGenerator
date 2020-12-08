cmake_minimum_required(VERSION 3.14)
project(V3MathExpressionGenerator LANGUAGES C)

include_directories(extern)

add_executable(V3MathExpressionGenerator
    src/main.c
    extern/mpc/mpc.c
)

target_include_directories(V3MathExpressionGenerator PRIVATE ./src)