#ifndef INCLUDE_COMPILER_UTIL
#define INCLUDE_COMPILER_UTIL

#include <vector>
#include <optional>
#include <algorithm>
#include <sstream>

namespace compiler {

using index_opt_type = std::optional<std::size_t>;

template <typename T>
std::optional<std::size_t> get_idx_aux(const std::vector<T> &vec,
                                       const T &ele)
{
    const auto ite = std::find(std::cbegin(vec), std::cend(vec), ele);
    if (ite == std::cend(vec)) return index_opt_type(std::nullopt);
    return index_opt_type(std::distance(std::cbegin(vec), ite));
}

template <typename T>
std::ostream& operator<<(std::ostream &os, const std::vector<T> &val) {
    os << '[';
    for (std::size_t i = 0; i != val.size(); i++) {
        os << val[i];
        if (i + 1 == val.size()) break;
        os << ' ';
    }
    return (os << ']');
}

}

#endif
