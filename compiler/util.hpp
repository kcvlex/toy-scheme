#ifndef INCLUDE_COMPILER_UTIL
#define INCLUDE_COMPILER_UTIL

#include <vector>
#include <optional>
#include <algorithm>

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

}

#endif
