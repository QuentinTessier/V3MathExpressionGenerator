#include <stdio.h>
#include "AST.h"

void TranspileASTVariableInternal(ASTNode *root, ASTNode **save, size_t *offset)
{
    if (root->type == ASTN_Variable) {
        for (size_t i = 0; i < *offset; ++i) {
            if (strcmp(save[i]->variable.name, root->variable.name) == 0)
                return;
        }
        save[*offset] = root;
        *offset += 1;
    } else if (root->type == ASTN_BinaryOperator) {
        TranspileASTVariableInternal(root->binop.LHS, save, offset);
        TranspileASTVariableInternal(root->binop.RHS, save, offset);
    } else if (root->type == ASTN_UnaryOperator) {
        TranspileASTVariableInternal(root->unop.LHS, save, offset);
    }
}

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
    } else if (root->type == ASTN_Variable) {
        printf("%s", root->variable.name);
        if (root->variable.LHS) {
            printf(".");
            TranspileASTInternal(root->variable.LHS);
        } else
            printf(" ");
    }
}

void TranspileAST(ASTNode *root, const char *name)
{
    ASTNode *buffer[256] = {0};
    size_t offset = 0;
    printf("#define %s(", name);
    TranspileASTVariableInternal(root, buffer, &offset);
    for (size_t i = 0; i < offset; ++i) {
        printf("%s", buffer[i]->variable.name);
        if (i < offset - 1)
            printf(", ");
    }
    printf(")");
    TranspileASTInternal(root);
    printf("\n");
}