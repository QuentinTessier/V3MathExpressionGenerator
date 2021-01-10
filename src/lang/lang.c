#include <string.h>
#include <stdlib.h>
#include "lang.h"

int BuildLiteralASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot)
{
    size_t bsize = strlen(mroot->children[0]->contents) + strlen(mroot->children[2]->contents) + 2;
    char *buffer = calloc(bsize, sizeof(char));

    strcat(buffer, mroot->children[0]->contents);
    strcat(buffer, ".");
    strcat(buffer, mroot->children[2]->contents);
    *aroot = new_ASTNodeFloating(ASTN_Floating, atof(buffer));
    free(buffer);
    return (*aroot) ? 1 : 0;
}

int BuildVariableASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot)
{
    if (mroot->children_num > 0) {
        *aroot = new_ASTNodeVariable(ASTN_Variable, mroot->children[0]->contents);
        ASTNode *tmp = *aroot;
        for (int i = 2; i < mroot->children_num; i += 2) {
            tmp->variable.LHS = new_ASTNodeVariable(ASTN_Variable, mroot->children[i]->contents);
            tmp = tmp->variable.LHS;
        }
    } else {
        *aroot = new_ASTNodeVariable(ASTN_Variable, mroot->contents);
    }
    return 1;
}

int BuildPrimaryASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot)
{
    if (strstr(mroot->tag, "unary|>") != 0) {
        *aroot = new_ASTNodeUnaryOperator(ASTN_UnaryOperator, ConvertContentToUnaryOperatorType(mroot->contents));
        return BuildExpressionASTFromMPC(mroot->children[1], &(*aroot)->unop.LHS);
    } else if (strstr(mroot->tag, "parens") != 0) {
        return BuildExpressionASTFromMPC(mroot->children[1], aroot);
    } else if (strstr(mroot->tag, "integer") != 0) {
        *aroot = new_ASTNodeInteger(ASTN_Integer, atoi(mroot->contents));
        return (*aroot != 0) ? 1 : 0;
    } else if (strstr(mroot->tag, "literal|>") != 0) {
        return BuildLiteralASTFromMPC(mroot, aroot);
    } else if (strstr(mroot->tag, "variable") != 0) {
        return BuildVariableASTFromMPC(mroot, aroot);
    }
    return 0;
}

int BuildTermASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot)
{
    int index = mpc_ast_get_index_lb(mroot, "binop_term|regex", 0);

    if (index == -1)
        return BuildPrimaryASTFromMPC(mroot, aroot);
    int first_pass = 1;
    mpc_ast_t *child = mpc_ast_get_child_lb(mroot, "binop_term|regex", 0);
    printf("%s\n", child->tag);
    *aroot = new_ASTNodeBinaryOperator(ASTN_BinaryOperator, ConvertContentToBinaryOperatorType(child->contents));
    if (*aroot == 0)
        return 0;
    ASTNode *temp = *aroot;
    while (index != -1) {
        int save = index;
        if (first_pass) {
            if (!BuildPrimaryASTFromMPC(mroot->children[save - 1], &temp->binop.LHS))
                return 0;
            first_pass = 0;
        }
        index = mpc_ast_get_index_lb(mroot, "binop_term|regex", index + 1);
        if (index == -1) {
            if (!BuildTermASTFromMPC(mroot->children[save + 1], &temp->binop.RHS))
                return 0;
        } else {
            temp->binop.RHS = new_ASTNodeBinaryOperator(ASTN_BinaryOperator, ConvertContentToUnaryOperatorType(mroot->children[index]->contents));
            temp = temp->binop.RHS;
            if (!BuildTermASTFromMPC(mroot->children[index - 1], &temp->binop.RHS))
                return 0;
        }
    }
    return 1;
}

int BuildExpressionASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot)
{
    int index = mpc_ast_get_index_lb(mroot, "binop_exp|regex", 0);

    if (index == -1)
        return BuildTermASTFromMPC((mroot->children_num == 1 ? mroot->children[0] : mroot), aroot);
    int first_pass = 1;
    mpc_ast_t *child = mpc_ast_get_child_lb(mroot, "binop_exp|regex", 0);
    *aroot = new_ASTNodeBinaryOperator(ASTN_BinaryOperator, ConvertContentToBinaryOperatorType(child->contents));
    if (*aroot == 0)
        return 0;
    ASTNode *temp = *aroot;
    while (index != -1) {
        int save = index;
        if (first_pass) {
            if (!BuildTermASTFromMPC(mroot->children[save - 1], &temp->binop.LHS))
                return 0;
            first_pass = 0;
        }
        index = mpc_ast_get_index_lb(mroot, "binop_exp|regex", index + 1);
        if (index == -1) {
            if (!BuildTermASTFromMPC(mroot->children[save + 1], &temp->binop.RHS))
                return 0;
        } else {
            temp->binop.RHS = new_ASTNodeBinaryOperator(ASTN_BinaryOperator, ConvertContentToUnaryOperatorType(mroot->children[index]->contents));
            temp = temp->binop.RHS;
            if (!BuildTermASTFromMPC(mroot->children[index - 1], &temp->binop.RHS))
                return 0;
        }
    }
    return 1;
}

void PrintAST(ASTNode *root)
{
    if (root) {
        if (root->type == ASTN_Integer) {
            printf("%d", root->integer.value);
        } else if (root->type == ASTN_Floating) {
            printf("%.2ff", root->floating.value);
        } else if (root->type == ASTN_Variable) {
            printf("%s", root->variable.name);
            if (root->variable.LHS) {
                printf(".");
                PrintAST(root->variable.LHS);
            }
        } else if (root->type == ASTN_UnaryOperator) {
            printf("(-)");
            PrintAST(root->unop.LHS);
        } else if (root->type, ASTN_BinaryOperator) {
            PrintAST(root->binop.LHS);
            PrintAST(root->binop.RHS);
            printf("%c", ConvertBinaryOperatorTypeToChar(root->binop.type));
        }
    }
}

void DestroyAST(ASTNode *root)
{
    if (root) {
        if (root->type == ASTN_Integer) {
            free(root);
        } else if (root->type == ASTN_UnaryOperator) {
            DestroyAST(root->unop.LHS);
            free(root);
        } else if (root->type, ASTN_BinaryOperator) {
            DestroyAST(root->binop.LHS);
            DestroyAST(root->binop.RHS);
            free(root);
        }
    }
}