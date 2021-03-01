#pragma once

#include <mpc/mpc.h>
#include "AST.h"

int BuildExpressionASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot);
int BuildTermASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot);

void PrintAST(ASTNode *root);
void DestroyAST(ASTNode *root);

void TranspileAST(ASTNode *root, const char *name);

void TypeAssignementPass(ASTNode **root);