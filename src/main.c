#include <stdio.h>
#include <stdlib.h>
#include <mpc/mpc.h>
#include "lang/AST.h"

int main()
{
    int x = 10;
    ASTNode *root = new_ASTNode(ASTN_Integer, &x, sizeof(x));

    printf("Value : %d\n", root->integer.value);
    return 0;
}