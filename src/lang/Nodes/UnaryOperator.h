#pragma once

#include <string.h>

typedef struct ASTNode ASTNode;

enum UnaryOperatorType {
    UnaryOperatorError = -1,
    UnaryOperatorNeg
};

typedef struct ASTNodeUnaryOperatorData {
    enum UnaryOperatorType type;
    ASTNode *LHS;
} ASTNodeUnaryOperatorData;

static inline enum UnaryOperatorType ConvertContentToUnaryOperatorType(char const *symbol)
{
    if (strcmp(symbol, "-") == 0)
        return UnaryOperatorNeg;
    else
        return UnaryOperatorError;
}