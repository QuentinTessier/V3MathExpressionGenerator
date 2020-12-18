#pragma once

#include <stdlib.h>
#include "../utils/array.h"
#include "../utils/hash.h"

#define DEFAULT_TYPETABLE_SIZE 32

typedef struct type_infos {
    char *name;
    Hashed hashed_name;
} TypeInfos;

typedef TypeInfos *TypeTable;

TypeTable new_TypeTable()
{
    return 0;
}

int TypeTable_add(TypeTable *table, char *name)
{
    TypeInfos infos;

    infos.name = name;
    infos.hashed_name = hash((unsigned char *)name);

    return ARRAY_PUSH(*table, infos);
}

