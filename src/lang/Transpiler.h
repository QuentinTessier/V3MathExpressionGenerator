#pragma once

#include <mpc/mpc.h>

#define LANG_PARSER_COUNT 15

static const char LANG_PARSER_NAMES[LANG_PARSER_COUNT - 1][32] = {
    "SOI", "EOI", "binop_term", "binop_exp", "unop",
    "identifier", "variable", "integer", "literal",
    "parens", "primary", "unary", "term", "expression"
};

static const char LANG_PARSER_NAME[] = "lang";

typedef struct Parser {
    unsigned int pcount;
    mpc_parser_t **parsers;
    mpc_parser_t *lang;
} Parser;

static inline int construct_Parser(Parser *self)
{
    self->pcount = LANG_PARSER_COUNT - 1;
    self->parsers = calloc(sizeof(mpc_parser_t *), self->pcount);
    if (self->parsers == 0)
        return 0;
    for (unsigned int i = 0; i < self->pcount; ++i) {
        self->parsers[i] = mpc_new(LANG_PARSER_NAMES[i]);
    }
    self->lang = mpc_new(LANG_PARSER_NAME);
    mpc_err_t *err = mpc_
    return 1;
}

static inline int destroy_Parser(Parser *self)
{
    mpc_cleanup(

    )
}