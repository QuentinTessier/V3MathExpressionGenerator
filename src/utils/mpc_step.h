#pragma once

#include <string.h>
#include <mpc/mpc.h>

static inline int mpc_with_tag(mpc_ast_t *root, char const *tag)
{
    return (strncmp(root->tag, tag, strlen(tag)) == 0);
}

static inline int mpc_as_tag(mpc_ast_t *root, char const *tag)
{
    return (strstr(root->tag, tag) != 0);
}