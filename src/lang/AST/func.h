#pragma once

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Node.h"

typedef struct function_call {
    Node base;
    char *name;
    Node **args;
} NodeFunctionCall;

static inline Node *new_FunctionCall(const char *name, int arg_count)
{
    NodeFunctionCall *n = calloc(1, sizeof(NodeFunctionCall));

    if (n == 0)
        return 0;
    n->base.type = FunctionCall;
    n->name = strdup(name);
    n->args = calloc(arg_count + 1, sizeof(Node *));
    return &n->base;
}

static inline void print_FunctionCall(NodeFunctionCall *n)
{
    printf("%s", n->name);
}