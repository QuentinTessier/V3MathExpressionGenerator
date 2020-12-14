#pragma once

#include "Node.h"
#include "binop.h"
#include "constant.h"
#include "variable.h"
#include "unop.h"
#include "func.h"

static inline void print_tree(Node *root)
{
    if (root == 0)
        return;
    if (root->type == DecimalConstant) {
        print_DecimalConstant((NodeDecimalConstant *)root);
        return;
    } else if (root->type == DoubleConstant) {
        print_DoubleConstant((NodeDoubleConstant *)root);
        return;
    } else if (root->type == Variable) {
        print_Variable((NodeVariable *)root);
        return;
    } else if (root->type == BinaryOperation) {
        NodeBinaryOperation *tmp = (NodeBinaryOperation *)root;
        print_BinaryOperation(tmp);
        print_tree(tmp->LHS);
        print_tree(tmp->RHS);
        return;
    } else if (root->type == UnaryOperation) {
        NodeUnaryOperator *tmp = (NodeUnaryOperator *)root;
        print_UnaryOperator(tmp);
        print_tree(tmp->child);
        return;
    } else if (root->type == FunctionCall) {
        NodeFunctionCall *tmp = (NodeFunctionCall *)root;
        Node *ite = tmp->args[0];
        print_FunctionCall(tmp);
        printf("(");
        for (int i = 0; tmp->args[i] != 0; ++i) {
            print_tree(tmp->args[i]);
            if (tmp->args[i + 1] != 0)
                printf(", ");
        }
        printf(")");
        return;
    }
    printf("Unknown node\n");
}