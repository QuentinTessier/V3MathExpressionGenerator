#pragma once

typedef struct ASTNode ASTNode;

typedef struct ASTNodeVariableData {
    char *name;
    ASTNode *LHS;
} ASTNodeVariableData;