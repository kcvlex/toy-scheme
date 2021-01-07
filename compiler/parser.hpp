#ifndef INCLUDE_PARSER
#define INCLUDE_PARSER

#include "token.hpp"
#include "ast.hpp"

namespace compiler {

struct Parser {
    Parser(TokenStream stream_arg);

    SequenceNode* parse();

private:
    EvalNode* parse_eval();
    LambdaNode* parse_lambda();
    SymbolNode* parse_symbol();
    ConstantNode* parse_constant();
    SequenceNode* parse_seq();

    bool is_lambda() const;
    bool is_begin() const;

    TokenStream stream;
};

}

#endif
