#include <stdio.h>
#include <stdlib.h>
#include <mpc/mpc.h>
#include "lang/AST.h"
#include "utils/mpc_step.h"
#include "lang/lang.h"

#define NParser 15
mpc_parser_t *LanguageParser[NParser] = {
    0
};

int ConstructParser(char const *filepath, mpc_parser_t **lang)
{
    static const char names[][32] = {
        "SOI", "EOI", "binop_term", "binop_exp", "unop",
        "identifier", "variable", "integer", "literal",
        "parens", "primary", "unary", "term", "expression",
        "lang"
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
            LanguageParser[13],
            LanguageParser[14],
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

int REPL(int build_AST, mpc_parser_t *lang, ASTNode **root)
{
    printf("REPL?\n");
    char buffer[2048] = {0}; // No getline on windows
    int run = 1;
    mpc_result_t r;

    fprintf(stdout, "$> ");
    while (fgets(buffer, 2048, stdin)) {
        buffer[strlen(buffer) - 1] = 0;
        if (strcmp(buffer, "exit") == 0) {
            break;
        } else {
            if (mpc_parse("REPL", buffer, lang, &r)) {
                mpc_ast_print(r.output);
                mpc_ast_t *ast = r.output;
                if (strcmp(ast->tag, ">") == 0)
                    ast = ast->children[0];
                if (build_AST) {
                    if (!BuildExpressionASTFromMPC(ast, root)) {
                        fprintf(stderr, "Failed to build tree\n");
                    } else {
                        PrintAST(*root);
                        fprintf(stdout, "\n");
                        //TranspileAST(*root, "REPL");
                        DestroyAST(*root);
                        *root = 0;
                    }
                }
                mpc_ast_delete(r.output);
            } else {
                mpc_err_print(r.error);
                mpc_err_delete(r.error);
            }
        }
        fprintf(stdout, "$> ");
        memset(buffer, 0, 2048);
    }
    return 0;
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
            if (!BuildExpressionASTFromMPC(ast, &root))
                fprintf(stderr, "Failed to build tree\n");
            else {
                PrintAST(root);
                printf("\n");
                TranspileAST(root, "TEST");
                DestroyAST(root);
            }
            mpc_ast_delete(r.output);
        } else {
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }
    } else {
        return REPL(1, lang, &root);
    }

    DestroyParser();
    return 0;
}