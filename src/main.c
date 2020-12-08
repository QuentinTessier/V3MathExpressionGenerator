#include <stdio.h>
#include <stdlib.h>
#include <mpc/mpc.h>

char *load_grammar(char const *path)
{
    FILE *f = fopen(path, "r");
    if (f == 0)
        return 0;

    fseek(f, 0L, SEEK_END);
    long int nBytes = ftell(f);
    fseek(f, 0L, SEEK_SET);

    char *buffer = calloc(nBytes, sizeof(char));
    if (buffer == 0)
        return 0;

    fread(buffer, sizeof(char), nBytes, f);
    fclose(f);

    return buffer;
}

int main(int argc, char **argv)
{
    char *grammar = load_grammar("../grammar.mpc");
    mpc_parser_t *Comment = mpc_new("comment");
    mpc_parser_t *Identifier = mpc_new("identifier");
    mpc_parser_t *Number = mpc_new("number");
    mpc_parser_t *Call = mpc_new("call");
    mpc_parser_t *Factor = mpc_new("factor");
    mpc_parser_t *Term = mpc_new("term");
    mpc_parser_t *Lexp = mpc_new("lexp");
    mpc_parser_t *Generic = mpc_new("generic");
    mpc_parser_t *Type = mpc_new("type");
    mpc_parser_t *Variable = mpc_new("variable");
    mpc_parser_t *Args = mpc_new("args");
    mpc_parser_t *Body = mpc_new("body");
    mpc_parser_t *Generate = mpc_new("generate");
    mpc_parser_t *Procedure = mpc_new("procedure");
    mpc_parser_t *Lang = mpc_new("lang");

    mpca_lang(MPCA_LANG_PREDICTIVE, grammar,
        Comment,
        Identifier,
        Number,
        Call,
        Factor,
        Term,
        Lexp,
        Generic,
        Type,
        Variable,
        Args,
        Body,
        Generate,
        Procedure,
        Lang,
        0
    );

    mpc_ast_t *ast;

    if (argc > 1) {
        mpc_result_t r;
        if (mpc_parse_contents(argv[1], Lang, &r)) {
            mpc_ast_print(r.output);
            ast = r.output;
        } else {
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }
    }

    int index = mpc_ast_get_index(ast, "procedure|>");
    mpc_ast_t *child = mpc_ast_get_child(ast, "procedure|>");
    if (child != 0) {

    }
    mpc_ast_delete(ast);

    mpc_cleanup(15,
        Comment,
        Identifier,
        Number,
        Call,
        Factor,
        Term,
        Lexp,
        Generic,
        Type,
        Variable,
        Args,
        Body,
        Generate,
        Procedure,
        Lang
    );

    return 0;
}