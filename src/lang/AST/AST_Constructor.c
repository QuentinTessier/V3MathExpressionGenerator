#include <stdio.h>
#include "AST_Constructor.h"

void AST_dump_node_types(FILE *f)
{
    fprintf(f, "AST Node Types:\n");
    Array_foreach(GAST_NodeTypes, ite, {
        fprintf(f, "\t%s:0x%x => %lu\n", ite->name, ite->id, ite->data_size);
        Array_foreach(ite->dataInfos, ite1, {
            if (ite1->type & AST_DT_ARRAY) {
                fprintf(f, "\t\tArray(%s) %s\n", AST_DataType_Strings[ite1->type & ~AST_DT_ARRAY], ite1->name);
            } else {
                fprintf(f, "\t\t%s %s\n", AST_DataType_Strings[ite1->type], ite1->name);
            }
        })
    });
}

int AST_INTERNAL_check_collision(uint64_t h)
{
    Array_foreach(GAST_NodeTypes, ite, {
        if (ite->id == h)
            return 1;
    });
    return 0;
}

size_t AST_INTERNAL_get_node_type_data_size(uint64_t h)
{
    Array_foreach(GAST_NodeTypes, ite, {
        if (ite->id == h)
            return ite->data_size;
    });
    return 0;
}

static AST_NodeType *AST_INTERNAL_get_node_type(uint64_t h)
{
    Array_foreach(GAST_NodeTypes, ite, {
        if (ite->id == h)
            return ite;
    });
    return 0;
}


uint64_t AST_register_node_type(const char *name)
{
    AST_NodeType nt;

    nt.name = strdup(name);
    nt.id = hash(nt.name);
    nt.data_size = 0;
    nt.dataInfos = 0;

    if (AST_INTERNAL_check_collision(nt.id)) {
        fprintf(stderr, "AST_register_node_type(%s) => Type already exist.\n", name);
        return 0;
    }
    Array_push(GAST_NodeTypes, nt);
    return nt.id;
}

void AST_register_node_data_infos(uint64_t id, AST_DataType t, const char *name)
{
    AST_NodeType *dt = AST_INTERNAL_get_node_type(id);

    if (dt == 0)
        return;
    if (t & AST_DT_ARRAY)
        dt->data_size += AST_DataType_Sizes[AST_DT_ARRAY];
    else
        dt->data_size += AST_DataType_Sizes[t];
    AST_DataInfos df;
    df.name = strdup(name);
    df.type = t;
    Array_push(dt->dataInfos, df);
}