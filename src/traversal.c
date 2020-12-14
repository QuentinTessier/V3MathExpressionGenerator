#include <mpc/mpc.h>
#include "utils/stack.h"
#include "lang/AST/binop.h"
#include "lang/AST/unop.h"
#include "lang/AST/constant.h"
#include "lang/AST/variable.h"
#include "lang/AST/func.h"

static int count_args(mpc_ast_t *root)
{
    if (strcmp(root->tag, "func_call|>") != 0)
        return -1;
    return (root->children_num - 2) / 2 + 1;
}

void traversal_expression(mpc_ast_t *ast, Node **root, int depth);
void traversal_term(mpc_ast_t *ast, Node **root, int depth);

void traversal_primary(mpc_ast_t *ast, Node **root, int depth)
{
    if (strstr(ast->tag, "unary|>") != 0) {
        *root = new_UnaryOperator(ast->children[0]->contents);
        traversal_expression(ast->children[1], &((NodeUnaryOperator *)(*root))->child, depth);
        return;
    } else if (strstr(ast->tag, "postfix|>") != 0) {
        *root = new_FunctionCall(ast->children[0]->contents, count_args(ast->children[1]));
        NodeFunctionCall *tmp = (NodeFunctionCall *)(*root);
        for (int i = 0; i < count_args(ast->children[1]); ++i) {
            int offset = i * 2 + 1;
            traversal_expression(ast->children[1]->children[offset], &tmp->args[i], 0);
        }
        return;
    }
    if (strstr(ast->tag, "primary") == 0) {
        return;
    } else if (strstr(ast->tag, "parens") != 0) {
        traversal_expression(ast->children[1], root, depth);
    } else if (strstr(ast->tag, "double_const") != 0) {
        *root = new_DoubleConstant(ast->contents);
    } else if (strstr(ast->tag, "decimal_const") != 0) {
        *root = new_DecimalConstant(ast->contents);
    } else if (strstr(ast->tag, "identifier") != 0) {
        *root = new_Variable(ast->contents);
    }
}

void traversal_term(mpc_ast_t *ast, Node **root, int depth)
{
    int index = mpc_ast_get_index_lb(ast, "binop_term|regex", 0);

    if (index == -1) {
        traversal_primary(ast, root, depth);
        return;
    }
    int first_pass = 1;
    *root = new_BinaryOperation(ast->children[index]->contents);
    Node *tmp = *root;
    while (index != -1) {
        int save = index;
        if (first_pass) {
            traversal_primary(ast->children[index - 1], &((NodeBinaryOperation *)(*root))->LHS, depth + 1);
            first_pass = 0;
        }
        index = mpc_ast_get_index_lb(ast, "binop_term|regex", index + 1);
        if (index == -1) {
            traversal_term(ast->children[save + 1], &((NodeBinaryOperation *)(tmp))->RHS, depth + 1);
        } else {
            ((NodeBinaryOperation *)(tmp))->RHS = new_BinaryOperation(ast->children[index]->contents);
            tmp = ((NodeBinaryOperation *)(tmp))->RHS;
            traversal_term(ast->children[index - 1], &((NodeBinaryOperation *)(tmp))->LHS, depth + 1);
        }
    }
}

void traversal_expression(mpc_ast_t *ast, Node **root, int depth)
{
    int index = mpc_ast_get_index_lb(ast, "binop_exp|regex", 0);

    if (index == -1) {
        traversal_term(ast, root, depth);
        return;
    }
    int first_pass = 1;
    *root = new_BinaryOperation(ast->children[index]->contents);
    Node *tmp = *root;
    while (index != -1) {
        int save = index;
        if (first_pass) {
            traversal_term(ast->children[save - 1], &((NodeBinaryOperation *)(tmp))->LHS, depth + 1);
            first_pass = 0;
        }
        index = mpc_ast_get_index_lb(ast, "binop_exp|regex", index + 1); // + | -
        if (index == -1) {
            traversal_term(ast->children[save + 1], &((NodeBinaryOperation *)(tmp))->RHS, depth + 1);
        } else {
            ((NodeBinaryOperation *)(tmp))->RHS = new_BinaryOperation(ast->children[index]->contents);
            tmp = ((NodeBinaryOperation *)(tmp))->RHS;
            traversal_term(ast->children[index - 1], &((NodeBinaryOperation *)(tmp))->LHS, depth + 1);
        }
    }
}