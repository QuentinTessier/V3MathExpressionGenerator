#pragma once

#include <mpc/mpc.h>
#include "AST.h"

int BuildExpressionASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot);
int BuildTermASTFromMPC(mpc_ast_t *mroot, ASTNode **aroot);

void PrintAST(ASTNode *root);