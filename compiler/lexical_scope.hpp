#ifndef INCLUDE_LEXICAL_SCOPE
#define INCLUDE_LEXICAL_SCOPE

#include <vector>
#include <set>
#include <string>

namespace compiler {

struct LexicalScope {
    LexicalScope(std::vector<std::string> refs_arg);
    LexicalScope(const std::set<std::string> &set);

    const std::string& get_name() const noexcept;
    ssize_t get_idx(const std::string &var) const noexcept;
    const std::vector<std::string>& get_refs() const noexcept;

private:
    std::vector<std::string> refs;
    std::string name;

    void init_refs();
};

}

#endif
