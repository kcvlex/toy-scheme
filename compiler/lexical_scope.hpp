#ifndef INCLUDE_COMPILER_LEXICAL_SCOPE
#define INCLUDE_COMPILER_LEXICAL_SCOPE

#include <vector>
#include <set>
#include <string>
#include <memory>
#include "util.hpp"

namespace compiler {

struct ExternRefs {

    ExternRefs(std::vector<std::string> refs_arg);
    ExternRefs(const std::set<std::string> &set);

    const std::string& get_name() const noexcept;
    index_opt_type get_idx(const std::string &var) const noexcept;
    const std::vector<std::string>& get_refs() const noexcept;
    std::size_t size() const noexcept;

private:
    std::vector<std::string> refs;
    std::string name;

    void init_refs();
};
    
enum class RefTypeTag {
    Unknown,
    Args,
    Local,
    External
};

using ref_type = std::pair<RefTypeTag, std::size_t>;

struct LexicalScope {
    LexicalScope(std::vector<std::string> args_arg,
                 std::vector<std::string> locals_arg,
                 std::shared_ptr<ExtenRefs> ext_refs_arg);

    ref_type get_ref(const std::string &name) const noexcept;
    const ExternRefs* get_ext_refs() const noexcept;

private:
    std::vector<std::string> args, locals;
    std::shared_ptr<ExternRefs> ext_refs;
};

}

#endif
