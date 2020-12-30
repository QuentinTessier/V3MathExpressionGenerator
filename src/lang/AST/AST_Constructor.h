#pragma once


#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "utils/hash.h"
#include "Array/src/array.h"
#include "Node.h"

typedef enum AST_DataType {
    AST_DT_STRING = (1 << 0),
    AST_DT_INTEGER = (1 << 1),
    AST_DT_POINTER = (1 << 2),
    AST_DT_CHILD = (1 << 3),
    AST_DT_ARRAY = (1 << 4)
} AST_DataType;

static const size_t AST_DataType_Sizes[] = {
    [AST_DT_STRING] = sizeof(char *),
    [AST_DT_INTEGER] = sizeof(int),
    [AST_DT_POINTER] = sizeof(void *),
    [AST_DT_CHILD] = sizeof(Node),
    [AST_DT_ARRAY] = sizeof(void *)
};

static const char AST_DataType_Strings[][32] = {
    [AST_DT_STRING] = "AST_DT_STRING",
    [AST_DT_INTEGER] = "AST_DT_INTEGER",
    [AST_DT_POINTER] = "AST_DT_POINTER",
    [AST_DT_CHILD] = "AST_DT_CHILD",
    [AST_DT_ARRAY] = "AST_DT_ARRAY"
};

typedef struct AST_DataInfos {
    AST_DataType type;
    char *name;
} AST_DataInfos;

typedef struct AST_NodeType {
    NodeType id;
    char *name;
    size_t data_size;
    Array(AST_DataInfos) dataInfos;
} AST_NodeType;

extern Array(AST_NodeType) GAST_NodeTypes;

#define AST_init_context()                          \
    Array(AST_NodeType) GAST_NodeTypes = 0;

uint64_t AST_register_node_type(const char *name);
void AST_register_node_data_infos(uint64_t id, AST_DataType t, const char *name);
void AST_dump_node_types(FILE *f);
int AST_INTERNAL_check_collision(uint64_t h);
size_t AST_INTERNAL_get_node_type_data_size(uint64_t h);