#include "lexical_scope.hpp"
#include <algorithm>

namespace compiler {

namespace {

int lex_scope_cnt = 0;

std::string get_lex_scope_name() {
    return std::string("c__") + std::to_string(lex_scope_cnt++);
}

}

LexicalScope::LexicalScope(std::vector<std::string> refs_arg)
    : refs(std::move(refs_arg)),
      name(std::move(get_lex_scope_name()))
{
    init_refs();
}

LexicalScope::LexicalScope(const std::set<std::string> &set)
    : name(std::move(get_lex_scope_name()))
{
    refs.insert(std::end(refs), std::cbegin(set), std::cend(set));
    init_refs();
}

void LexicalScope::init_refs() {
    std::sort(std::begin(refs), std::end(refs));
    const auto ite = std::unique(std::begin(refs), std::end(refs));
    refs.erase(ite, std::end(refs));
}

const std::string& LexicalScope::get_name() const noexcept {
    return name;
}

ssize_t LexicalScope::get_idx(const std::string &var) const noexcept {
    const auto ite = std::lower_bound(std::cbegin(refs), std::cend(refs), var);
    if (ite == std::cend(refs)) return -1;
    return std::distance(std::cbegin(refs), ite);
}

const std::vector<std::string>& LexicalScope::get_refs() const noexcept {
    return refs;
}

}
