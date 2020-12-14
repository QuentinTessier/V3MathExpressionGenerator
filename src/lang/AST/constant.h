#pragma once

#include <stdlib.h>
#include "Node.h"

typedef struct decimal_const {
    Node base;
    unsigned int value;
} NodeDecimalConstant;

typedef struct double_const {
    Node base;
    double value;
} NodeDoubleConstant;

static inline Node *new_DecimalConstant(char const *data)
{
    NodeDecimalConstant *n = calloc(1, sizeof(NodeDecimalConstant));
    n->base.type = DecimalConstant;
    n->value = atoi(data);

    return &n->base;
}

static inline Node *new_DoubleConstant(char const *data)
{
    NodeDecimalConstant *n = calloc(1, sizeof(NodeDoubleConstant));
    n->base.type = DoubleConstant;
    n->value = atof(data);

    return &n->base;
}

static inline void print_DecimalConstant(NodeDecimalConstant *n)
{
    printf("%d", n->value);
}

static inline void print_DoubleConstant(NodeDoubleConstant *n)
{
    printf("%.2f", n->value);
}