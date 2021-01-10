#include <stdio.h>
#include "AST.h"

void TranspileASTInternal(ASTNode *root)
{
    if (root == 0)
        return;
    else if (root->type == ASTN_Integer)
        printf("%d ", root->integer.value);
    else if (root->type == ASTN_UnaryOperator) {
        printf("( -");
        TranspileASTInternal(root->unop.LHS);
        printf(") ");
    } else if (root->type == ASTN_BinaryOperator) {
        printf("( ");
        TranspileASTInternal(root->binop.LHS);
        printf("%c ", ConvertBinaryOperatorTypeToChar(root->binop.type));
        TranspileASTInternal(root->binop.RHS);
        printf(") ");
    }
}

void TranspileAST(ASTNode *root, const char *name)
{
    printf("#define %s() ", name);
    TranspileASTInternal(root);
    printf("\n");
}