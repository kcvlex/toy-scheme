#ifndef INCLUDE_CLOSURE_TABLE
#define INCLUDE_CLOSURE_TABLE

#include <vector>
#include <set>
#include <string>

namespace compiler {

struct ClosureTable {
    ClosureTable(std::vector<std::string> refs_arg);
    ClosureTable(const std::set<std::string> &set);

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
