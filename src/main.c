#include <stdio.h>
#include <stdlib.h>
#include <mpc/mpc.h>
#include "lang/AST/AST_Constructor.h"

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

// int main(int argc, char **argv)
// {
//     char *grammar = load_grammar("../grammar.mpc");
//     mpc_parser_t *SOI = mpc_new("SOI");
//     mpc_parser_t *EOI = mpc_new("EOI");
//     mpc_parser_t *binop_term = mpc_new("binop_term");
//     mpc_parser_t *binop_exp = mpc_new("binop_exp");
//     mpc_parser_t *unop = mpc_new("unop");
//     mpc_parser_t *parens = mpc_new("parens");
//     mpc_parser_t *identifier = mpc_new("identifier");
//     mpc_parser_t *decimalc = mpc_new("decimal_const");
//     mpc_parser_t *doublec = mpc_new("double_const");
//     mpc_parser_t *literal = mpc_new("literal");
//     mpc_parser_t *array = mpc_new("array");
//     mpc_parser_t *field = mpc_new("field");
//     mpc_parser_t *primary = mpc_new("primary");
//     mpc_parser_t *func_call = mpc_new("func_call");
//     mpc_parser_t *postfix = mpc_new("postfix");
//     mpc_parser_t *unary = mpc_new("unary");
//     mpc_parser_t *term = mpc_new("term");
//     mpc_parser_t *expression = mpc_new("expression");
//     mpc_parser_t *lang = mpc_new("lang");

//     mpca_lang(MPCA_LANG_PREDICTIVE, grammar,
//         SOI,
//         EOI,
//         binop_term,
//         binop_exp,
//         unop,
//         parens,
//         identifier,
//         decimalc,
//         doublec,
//         literal,
//         array,
//         field,
//         primary,
//         func_call,
//         postfix,
//         unary,
//         term,
//         expression,
//         lang,
//         0
//     );

//     mpc_ast_t *ast;

//     if (argc > 1) {
//         mpc_result_t r;
//         if (mpc_parse_contents(argv[1], lang, &r)) {
//             mpc_ast_print(r.output);
//             ast = r.output;
//         } else {
//             mpc_err_print(r.error);
//             mpc_err_delete(r.error);
//         }
//     }
//     Node *tree = 0;
//     traversal_expression(ast->children[1], &tree, 0);
//     print_tree(tree);
//     mpc_ast_delete(ast);

//     mpc_cleanup(18,
//         SOI,
//         EOI,
//         binop_term,
//         binop_exp,
//         unop,
//         parens,
//         identifier,
//         decimalc,
//         doublec,
//         literal,
//         array,
//         field,
//         primary,
//         func_call,
//         postfix,
//         unary,
//         term,
//         expression,
//         lang
//     );

//     return 0;
// }

AST_init_context();

int main(int ac, char **av)
{
    uint64_t idType = AST_register_node_type("Type");
    AST_register_node_data_infos(idType, AST_DT_STRING, "name");
    AST_register_node_data_infos(idType, AST_DT_ARRAY | AST_DT_INTEGER, "values");
    AST_register_node_type("Formula");

    AST_dump_node_types(stdout);
    return 0;
}