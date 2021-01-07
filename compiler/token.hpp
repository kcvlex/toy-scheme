#ifndef INCLUDE_TOKENIZER
#define INCLUDE_TOKENIZER

#include <vector>
#include <string>

namespace compiler {

struct TokenStream {
    using raw_token_type = std::string;
    using raw_tokens_type = std::vector<raw_token_type>;
    using token_iterator = typename raw_tokens_type::const_iterator;
    
    TokenStream(raw_tokens_type token_list_arg);

    token_iterator head() const;
    token_iterator lookup(const std::size_t n) const;
    token_iterator advance();
    TokenStream& eat(const raw_token_type &token);
    std::size_t rest_size() const noexcept;
    bool finished() const;

private:
    raw_tokens_type token_list;
    token_iterator cur;
};

struct Tokenizer {
    static TokenStream build(const std::string &code);

private:

    const std::string code;
    TokenStream::raw_tokens_type token_list;

    Tokenizer(const std::string &code_arg);
    void tokenize();
};

}

#endif
