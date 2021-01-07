#pragma once

#include <stdlib.h>
#include <string.h>
#include "Nodes/Integer.h"

typedef enum ASTNodeType {
    ASTN_Integer
} ASTNodeType;

typedef struct ASTNode {
    ASTNodeType type;

    union {
        ASTNodeIntegerData integer;
    };
} ASTNode;

static inline ASTNode *new_ASTNode(ASTNodeType type, void *data, size_t n)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        if (data)
            memcpy(&a->integer, data, n);
    }
    return a;
}

static inline ASTNode *new_ASTNodeInteger(ASTNodeType type, int32_t value)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        a->integer.value = value;
    }
    return a;
}