#include <string.h>
#include "lang.h"

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
        // TO-DO: Read a floating point value instead of a integer
        *aroot = new_ASTNodeInteger(ASTN_Integer, atoi(mroot->contents));
        return (*aroot != 0) ? 1 : 0;
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
        return BuildTermASTFromMPC(mroot, aroot);
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