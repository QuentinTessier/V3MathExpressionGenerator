#pragma once

#include <string.h>
#include <stdlib.h>
#include "Node.h"

enum BinaryOperationTypes {
    BinaryOperation_Mul,
    BinaryOperation_Div,
    BinaryOperation_Mod,

    BinaryOperation_Add,
    BinaryOperation_Sub
};

typedef struct binary_operation {
    Node base;
    enum BinaryOperationTypes op;
    Node *LHS, *RHS;
} NodeBinaryOperation;

static inline Node *new_BinaryOperation(const char *data)
{
    NodeBinaryOperation *n = calloc(1, sizeof(NodeBinaryOperation));

    if (n == 0)
        return 0;
    n->base.type = BinaryOperation;
    if (strcmp(data, "*") == 0)
        n->op = BinaryOperation_Mul;
    else if (strcmp(data, "/") == 0)
        n->op = BinaryOperation_Div;
    else if (strcmp(data, "%") == 0)
        n->op = BinaryOperation_Mod;
    else if (strcmp(data, "+") == 0)
        n->op = BinaryOperation_Add;
    else if (strcmp(data, "-") == 0)
        n->op = BinaryOperation_Sub;
    return &n->base;
}

static inline void print_BinaryOperation(NodeBinaryOperation *n)
{
    static const char symbols[] = {
        '*', '/', '%', '+', '-'
    };
    printf("%c", symbols[n->op]);
}