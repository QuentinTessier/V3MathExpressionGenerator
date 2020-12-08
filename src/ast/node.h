#pragma once

#include <stdio.h>

enum NodeType {
    NODE_AST_VARIABLE,
    NODE_AST_NUMBER,
    NODE_AST_IDENTIFIER,
    NODE_AST_PROCEDURE,
    NODE_AST_BINARY_OPERATION,
    NODE_AST_FUNCTION_CALL
};

typedef struct Node ASTNode;

typedef struct Variable {
    char *name;
    char *type;
} ASTNode_Variable;

typedef struct Number {
    char *number;
} ASTNode_Number;

typedef struct Identifier {
    char *name;
} ASTNode_Identifier;

typedef struct Procedure {
    char *name;
    char *return_type;
    int arg_count;
    ASTNode_Variable *args;
    ASTNode *body;
} ASTNode_Procedure;

typedef struct BinaryOperation {
    int op_type;
    ASTNode *LHS, *RHS;
} ASTNode_BinOp;

typedef struct FunctionCall {
    char *fun_name;
    int arg_count;
    ASTNode **args;
} ASTNode_FunctionCall;

typedef struct Node {
    int type;
    union {
        ASTNode_Variable var;
        ASTNode_Number num;
        ASTNode_Identifier ident;
        ASTNode_Procedure proc;
        ASTNode_BinOp binop;
        ASTNode_FunctionCall call;
    } data;
} ASTNode;

static inline void print_procedure(ASTNode_Procedure *proc)
{
    printf("Procedure %s (", proc->name);
    for (int i = 0; i < proc->arg_count; ++i) {
        printf("%s : %s", proc->args[i].name, proc->args[i].type);
        if (i + 1 < proc->arg_count)
            printf(", ");
    }
    printf(") : %s", proc->return_type);
}

void traversal(ASTNode *root, int depth, char ident);

static inline void print_function_call(ASTNode_FunctionCall * fcall, int depth)
{
    printf("FunctionCall %s (\n", fcall->fun_name);
    for (int i = 0; i < fcall->arg_count; ++i) {
        for (int i = 0; i < depth + 1; ++i)
            printf(" ");
        printf("Arg[%d] = ", i);
        traversal(fcall->args[i], depth + 1, 0);
    }
}

static inline void traversal(ASTNode *root, int depth, char ident)
{
    if (root == 0) {
        return;
    } else {
        if (ident) {
            for (int i = 0; i < depth; ++i)
                printf(" ");
        }
        switch (root->type) {
            case NODE_AST_VARIABLE:
                printf("Variable %s : %s\n", root->data.var.name, root->data.var.name);
                break;
            case NODE_AST_NUMBER:
                printf("Number %s\n", root->data.num.number);
                break;
            case NODE_AST_IDENTIFIER:
                printf("Identifier %s\n", root->data.ident.name);
                break;
            case NODE_AST_PROCEDURE:
                print_procedure(&root->data.proc);
                traversal(root->data.proc.body, depth + 1);
                break;
            case NODE_AST_FUNCTION_CALL:
                print_function_call(&root->data.call, depth);
                break;
            case NODE_AST_BINARY_OPERATION:
                printf("BinaryOperation %d", root->data.binop.op_type);
                traversal(root->data.binop.LHS, depth + 1, ident);
                traversal(root->data.binop.RHS, depth + 1, ident);
        }
    }
}