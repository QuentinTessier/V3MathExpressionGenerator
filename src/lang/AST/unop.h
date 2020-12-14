#pragma once

#include <string.h>
#include <stdlib.h>
#include "Node.h"

enum UnaryOperatorTypes {
    UnaryOperator_Neg
};

typedef struct unary_operator {
    Node base;
    enum UnaryOperatorTypes op;
    Node *child;
} NodeUnaryOperator;

static inline Node *new_UnaryOperator(char const *data)
{
    NodeUnaryOperator *n = calloc(1, sizeof(NodeUnaryOperator));

    if (n == 0)
        return 0;
    n->base.type = UnaryOperation;
    if (strcmp(data, "-") == 0)
        n->op = UnaryOperator_Neg;
    return &n->base;
}

static inline void print_UnaryOperator(NodeUnaryOperator *n)
{
    static const char symbols[] = { '-' };
    printf("(%c)", symbols[n->op]);
}