#ifndef INCLUDE_PARSER
#define INCLUDE_PARSER

#include "token.hpp"
#include "ast.hpp"

namespace compiler {

struct Parser {
    Parser(TokenStream stream_arg);

    ASTNode* parse();

private:
    EvalNode* parse_eval();
    LambdaNode* parse_lambda();
    SymbolNode* parse_symbol();
    ConstantNode* parse_constant();

    TokenStream stream;
};

}

#endif
