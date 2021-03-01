#pragma once

typedef struct ASTNode ASTNode;

typedef struct ASTNodeVariableData {
    char *name;
    ASTNode *LHS;
} ASTNodeVariableData;

/**
 *
 * a.x => Node [a] : f32
 *           |
 *        Node [x] : f32
 *
 */