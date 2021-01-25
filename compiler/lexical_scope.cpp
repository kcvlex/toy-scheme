#include "extern_refs.hpp"
#include "util.hpp"
#include <algorithm>

namespace compiler {

namespace {

int ext_refs_cnt = 0;

std::string get_ext_refs_name() {
    return std::string("c__") + std::to_string(ext_refs_cnt++);
}

}  // anonymouse


/******************** Extern Refs ********************/

ExternRefs::ExternRefs(std::vector<std::string> refs_arg)
    : refs(std::move(refs_arg)),
      name(std::move(get_ext_refs_name()))
{
    init_refs();
}

ExternRefs::ExternRefs(const std::set<std::string> &set)
    : name(std::move(get_ext_refs_name()))
{
    refs.insert(std::end(refs), std::cbegin(set), std::cend(set));
    init_refs();
}

void ExternRefs::init_refs() {
    std::sort(std::begin(refs), std::end(refs));
    const auto ite = std::unique(std::begin(refs), std::end(refs));
    refs.erase(ite, std::end(refs));
}

const std::string& ExternRefs::get_name() const noexcept {
    return name;
}

index_opt_type ExternRefs::get_idx(const std::string &var) const noexcept {
    const auto ite = std::lower_bound(std::cbegin(refs), std::cend(refs), var);
    if (ite == std::cend(refs)) return index_opt_type(std::nullopt);
    return index_opt_type(std::distance(std::cbegin(refs), ite));
}

const std::vector<std::string>& ExternRefs::get_refs() const noexcept {
    return refs;
}

std::size_ExternRefs::size() const noexcept {
    return refs.size();
}


/******************** Lexical Scope ********************/

LexicalScope::LexicalScope(std::vector<std::string> args_arg,
                           std::vector<std::string> locals_arg,
                           ExternRefs *ext_ref_arg)
    : args(std::move(args_arg)),
      locals(std::move(locals_arg)),
      ext_refs(ext_refs_arg)
{
    assert(ext_refs);
}

ref_type LexicalScope::get_ref(const std::string &name) const noexcept {
    {
        const auto opt = get_idx_aux(locals, name);
        if (opt.has_value()) return ref_type(RefTypeTag::Local, *opt);
    }

    {
        const auto opt = get_idx_aux(args, name);
        if (opt.has_value()) return ref_type(RefTypeTag::Arg, *opt);
    }

    {
        const auto opt = ext_refs->get_idx(name);
        if (opt.has_value()) return ref_type(RefTypeTag::Extenal, *opt);
    }

    return ref_type(RefTypeTag::Unknown, 0);
}

const ExternRefs* LexicalScope::get_ext_refs() const noexcept {
    return ext_refs;
}

}
