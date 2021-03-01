#pragma once

#include <string.h>

typedef struct ASTNode ASTNode;

enum BinaryOperatorType {
    BinaryOperatorError = -1,
    BinaryOperatorAdd,
    BinaryOperatorSub,
    BinaryOperatorDiv,
    BinaryOperatorMul,
    BinaryOperatorMod,
};

typedef struct ASTNodeBinaryOperatorData {
    enum BinaryOperatorType type;
    ASTNode *LHS, *RHS;
} ASTNodeBinaryOperatorData;

static inline enum BinaryOperatorType ConvertContentToBinaryOperatorType(char const *symbol)
{
    if (strcmp(symbol, "+") == 0)
        return BinaryOperatorAdd;
    else if (strcmp(symbol, "-") == 0)
        return BinaryOperatorSub;
    else if (strcmp(symbol, "*") == 0)
        return BinaryOperatorMul;
    else if (strcmp(symbol, "/") == 0)
        return BinaryOperatorDiv;
    else if (strcmp(symbol, "%") == 0)
        return BinaryOperatorMod;
    else
        return BinaryOperatorError;
}

static inline char ConvertBinaryOperatorTypeToChar(enum BinaryOperatorType o)
{
    const char c[6] = "?+-/*%";
    int index = o + 1;

    return c[index];
}