#pragma once

#include "../utils/hash.h"
#include "array.h"
#include "Nodes/BinaryOperator.h"

enum TypeInfo {
    AtomicType, // f32, i32
    ComplexType,
    GenericType
};

typedef struct ExpressionOverload {
    enum BinaryOperatorType o;
    uint64_t b;
    uint64_t res;
} ExpressionOverload;

typedef struct ExpressionType {
    enum TypeInfo info;
    uint64_t hashed;
    char *name;
    char *v; // only used as a field
    Array(struct ExpressionType) fields;
    Array(ExpressionOverload) operators;
} ExpressionType;

extern Array(ExpressionType) GAST_Types;

#define INIT_TYPEINFO_CONTEXT() Array(ExpressionType) GAST_Types = 0;

void destroy_TypeInfo();

static inline ExpressionType *new_AtomicTypeInfo(char *name)
{
    ExpressionType n = {AtomicType, hash(name), strdup(name), 0};

    Array_push(GAST_Types, n);

    return &(GAST_Types[ArraySize(GAST_Types) - 1]);
}

static inline void defineOperator(enum BinaryOperatorType o, uint64_t a, uint64_t b, uint64_t res)
{
    Array_foreach(GAST_Types, ite, {
        if (ite->hashed == a) {
            ExpressionOverload n;
            n.o = o;
            n.b = b;
            n.res = res;
            Array_push(ite->operators, n);
        }
    });
}

static inline void init_TypeInfo_AtomicTypes()
{
    new_AtomicTypeInfo("any"); // temporary
    ExpressionType *f32 = new_AtomicTypeInfo("f32");
    ExpressionType *i32 = new_AtomicTypeInfo("i32");
    defineOperator(BinaryOperatorAdd, f32->hashed, i32->hashed, f32->hashed);
    defineOperator(BinaryOperatorSub, f32->hashed, i32->hashed, f32->hashed);
    defineOperator(BinaryOperatorDiv, f32->hashed, i32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMul, f32->hashed, i32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMod, f32->hashed, i32->hashed, f32->hashed);

    defineOperator(BinaryOperatorAdd, f32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorSub, f32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorDiv, f32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMul, f32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMod, f32->hashed, f32->hashed, f32->hashed);

    defineOperator(BinaryOperatorAdd, i32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorSub, i32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorDiv, i32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMul, i32->hashed, f32->hashed, f32->hashed);
    defineOperator(BinaryOperatorMod, i32->hashed, f32->hashed, f32->hashed);

    defineOperator(BinaryOperatorAdd, i32->hashed, i32->hashed, i32->hashed);
    defineOperator(BinaryOperatorSub, i32->hashed, i32->hashed, i32->hashed);
    defineOperator(BinaryOperatorDiv, i32->hashed, i32->hashed, i32->hashed);
    defineOperator(BinaryOperatorMul, i32->hashed, i32->hashed, i32->hashed);
    defineOperator(BinaryOperatorMod, i32->hashed, i32->hashed, i32->hashed);
}