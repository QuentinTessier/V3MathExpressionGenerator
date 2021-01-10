#pragma once

#include <stdlib.h>
#include <string.h>
#include "Nodes/Integer.h"
#include "Nodes/Floating.h"
#include "Nodes/Variable.h"
#include "Nodes/BinaryOperator.h"
#include "Nodes/UnaryOperator.h"

typedef enum ASTNodeType {
    ASTN_Integer,
    ASTN_Floating,
    ASTN_Variable,
    ASTN_BinaryOperator,
    ASTN_UnaryOperator
} ASTNodeType;

typedef struct ASTNode {
    ASTNodeType type;

    union {
        ASTNodeIntegerData integer;
        ASTNodeFloatingData floating;
        ASTNodeVariableData variable;
        ASTNodeBinaryOperatorData binop;
        ASTNodeUnaryOperatorData unop;
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

static inline ASTNode *new_ASTNodeFloating(ASTNodeType type, float value)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        a->floating.value = value;
    }
    return a;
}

static inline ASTNode *new_ASTNodeVariable(ASTNodeType type, char const *name)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        a->variable.name = strdup(name);
        a->variable.LHS = 0;
    }
    return a;
}

static inline ASTNode *new_ASTNodeBinaryOperator(ASTNodeType type, enum BinaryOperatorType otype)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        a->binop.type = otype;
        a->binop.LHS = a->binop.RHS = 0;
    }
    return a;
}

static inline ASTNode *new_ASTNodeUnaryOperator(ASTNodeType type, enum UnaryOperatorType otype)
{
    ASTNode *a = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (a) {
        a->type = type;
        a->binop.type = otype;
        a->binop.LHS = 0;
    }
    return a;
}