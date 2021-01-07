#include "token.hpp"
#include <cassert>
#include <iostream>

namespace compiler {

namespace {

std::string surround(std::string s) {
    std::string res = "(begin \n";
    res += std::move(s);
    res += ")";
    return res;
}

}

Tokenizer::Tokenizer(const std::string &code_arg)
    : code(std::move(surround(code_arg))), token_list()
{
}

void Tokenizer::tokenize() {
    for (auto cur = std::cbegin(code); cur != std::cend(code); cur++) {
        if (*cur == '(') {
            token_list.push_back("(");
            continue;
        } else if (*cur == ')') {
            token_list.push_back(")");
            continue;
        }

        std::string res;
        for (; cur != std::cend(code); cur++) {
            if (*cur == '(') break;
            if (*cur == ')') break;
            if (*cur == ' ') break;
            if (*cur == '\n') break;
            res += *cur;
        }
        if (!res.empty()) token_list.push_back(std::move(res));
        if (*cur == '(') token_list.push_back("(");
        if (*cur == ')') token_list.push_back(")");
    }
}

TokenStream Tokenizer::build(const std::string &code) {
    Tokenizer tk(code);
    tk.tokenize();
    return TokenStream(std::move(tk.token_list));
}


/*******************************************/


TokenStream::TokenStream(raw_tokens_type token_list_arg)
    : token_list(std::move(token_list_arg)), cur(std::cbegin(token_list))
{
}

TokenStream::token_iterator TokenStream::head() const {
    return cur;
}

TokenStream::token_iterator TokenStream::lookup(const std::size_t n) const {
    return std::next(head(), n);
}

TokenStream::token_iterator TokenStream::advance() {
    if (finished()) assert(false);
    const auto ret = cur;
    cur++;
    return ret;
}

TokenStream& TokenStream::eat(const raw_token_type &token) {
    if (*cur != token) {
        std::cout << *cur << ", " << token << std::endl;
        assert(false);
    }
    advance();
    return *this;
}

std::size_t TokenStream::rest_size() const noexcept {
    return std::distance(cur, std::cend(token_list));
}

bool TokenStream::finished() const {
    return cur == std::cend(token_list);
}

}
