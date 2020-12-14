#pragma once

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Node.h"

typedef struct variable {
    Node base;
    char *name;
} NodeVariable;

static inline Node *new_Variable(char const *data)
{
    NodeVariable *n = calloc(1, sizeof(NodeVariable));

    if (n == 0)
        return 0;
    n->base.type = Variable;
    n->name = strdup(data);
    return &n->base;
}

static inline void print_Variable(NodeVariable *n)
{
    printf("%s", n->name);
}