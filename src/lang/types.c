#include <stdarg.h>
#include "AST.h"

void destroy_TypeInfo()
{
    Array_foreach(GAST_Types, ite, {
        Array_free(ite->operators);
        Array_free(ite->fields);
    });
}

ExpressionType *new_DefinedTypeInfo(char *name, int n, ...)
{
    va_list ap;
    va_start(ap, n);
    ExpressionType *base = new_AtomicTypeInfo(name);

    for (int i = 0; i < n; ++i) {
        char *name = va_arg(ap, char *);
        char *v = va_arg(ap, char *);

        ExpressionType *t = findTypeByHash(hash(name));
        if (t != 0) {
            ExpressionType nt = {t->info, t->hashed, t->name, strdup(v), t->fields, t->operators};
            Array_push(base->fields, nt);
        }
    }

    va_end(ap);
}

static ExpressionType *findTypeByHash(uint64_t h)
{
    Array_foreach(GAST_Types, ite, {
        if (ite->hashed == h)
            return ite;
    });
    return 0;
}

static ExpressionType *getBinaryOperatorExpressionType2(Array(ExpressionOverload) a, enum BinaryOperatorType o, uint64_t b)
{
    Array_foreach(a, ite, {
        if (ite->o == o && ite->b == b) {
            return findTypeByHash(ite->res);
        }
    });
}

static ExpressionType *getBinaryOperatorExpressionType(enum BinaryOperatorType o, uint64_t a, uint64_t b)
{
    Array_foreach(GAST_Types, ite, {
        if (ite->hashed == a) {
            return getBinaryOperatorExpressionType2(ite->operators, o, b);
        }
    });
    return 0;
}

void TypeAssignementPass(ASTNode **root)
{
    if (root && *root) {
        if ((*root)->type == ASTN_Integer) {
            (*root)->expType = &GAST_Types[2];
        } else if ((*root)->type == ASTN_Floating) {
            (*root)->expType = &GAST_Types[1];
        } else if ((*root)->type == ASTN_UnaryOperator) {
            TypeAssignementPass(&(*root)->unop.LHS);
            (*root)->expType = (*root)->unop.LHS->expType;
        } else if ((*root)->type == ASTN_BinaryOperator) {
            TypeAssignementPass(&(*root)->binop.LHS);
            TypeAssignementPass(&(*root)->binop.RHS);
            (*root)->expType = getBinaryOperatorExpressionType((*root)->binop.type, (*root)->binop.LHS->expType->hashed, (*root)->binop.RHS->expType->hashed);
        } else if ((*root)->type == ASTN_Variable) {
            (*root)->expType = &GAST_Types[1];
        }
    }
}