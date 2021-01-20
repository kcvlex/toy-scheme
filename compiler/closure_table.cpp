#include "closure.hpp"
#include <algorithm>

namespace compiler {

namespace {

int clsr_cnt = 0;

std::string get_clsr_name() {
    return std::string("c__") + std::to_string(clsr_cnt++);
}

}

ClosureTable::ClosureTable(std::vector<std::string> refs_arg)
    : refs(std::move(refs_arg)),
      name(std::move(get_clsr_name()))
{
    init_refs();
}

ClosureTable::ClosureTable(const std::set<std::string> &set)
    : name(std::move(get_clsr_name()))
{
    refs.insert(std::end(refs), std::cbegin(set), std::cend(set));
    init_refs();
}

void ClosureTable::init_refs() {
    std::sort(std::begin(refs), std::end(refs));
    const auto ite = std::unique(std::begin(refs), std::end(refs));
    refs.erase(ite, std::end(refs));
}

const std::string& ClosureTable::get_name() const noexcept {
    return name;
}

ssize_t ClosureTable::get_idx(const std::string &var) const noexcept {
    const auto ite = std::lower_bound(std::cbegin(refs), std::cend(refs), var);
    if (ite == std::cend(refs)) return -1;
    return std::distance(std::cbegin(refs), ite);
}

const std::vector<std::string>& ClosureTable::get_refs() const noexcept {
    return refs;
}

}
