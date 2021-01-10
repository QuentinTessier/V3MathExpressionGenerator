#include <stdio.h>
#include <stdlib.h>
#include <mpc/mpc.h>
#include "lang/AST.h"
#include "utils/mpc_step.h"
#include "lang/lang.h"

#define NParser 13
mpc_parser_t *LanguageParser[NParser] = {
    0
};

int ConstructParser(char const *filepath, mpc_parser_t **lang)
{
    static const char names[][32] = {
        "SOI", "EOI", "binop_term", "binop_exp", "unop",
        "integer", "literal", "parens", "primary", "unary",
        "term", "expression", "lang"
    };
    for (int i = 0; i < NParser; ++i) {
        LanguageParser[i] = mpc_new(names[i]);
    }

    mpc_err_t *err = mpca_lang_contents(MPCA_LANG_PREDICTIVE, filepath,
            LanguageParser[0],
            LanguageParser[1],
            LanguageParser[2],
            LanguageParser[3],
            LanguageParser[4],
            LanguageParser[5],
            LanguageParser[6],
            LanguageParser[7],
            LanguageParser[8],
            LanguageParser[9],
            LanguageParser[10],
            LanguageParser[11],
            LanguageParser[12],
            0
        );
    if (err) {
        mpc_err_print(err);
        mpc_err_delete(err);
        return 0;
    }
    *lang = LanguageParser[NParser - 1];
    return 1;
}

void DestroyParser()
{
    for (int i = 0; i < NParser; ++i) {
        mpc_cleanup(1, LanguageParser[i]);
    }
}

int main(int ac, char **av)
{
    mpc_parser_t *lang = 0;
    if (!ConstructParser("grammar.mpc", &lang))
        return 1;

    mpc_result_t r;
    ASTNode *root;
    if (ac > 1) {
        if (mpc_parse_contents(av[1], lang, &r)) {
            mpc_ast_print(r.output);
            mpc_ast_t *ast = r.output;
            if (strcmp(ast->tag, ">") == 0)
                ast = ast->children[0];
            if (!BuildExpressionASTFromMPC(ast->children[0], &root))
                fprintf(stderr, "Failed to build tree\n");
        } else {
            mpc_err_print(r.error);
        }
    } else {
        if (mpc_parse_pipe("<stdin>", stdin, lang, &r)) {
            mpc_ast_print(r.output);
        } else {
            mpc_err_print(r.error);
        }
    }
    PrintAST(root);
    printf("\n");
    TranspileAST(root, "TEST");
    DestroyParser();
    DestroyAST(root);
    return 0;
}