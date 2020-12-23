#ifndef INCLUDE_ENUM2STR
#define INCLUDE_ENUM2STR

#include <string_view>
#include <utility>
#include <array>

namespace util {

namespace internal {

template <char... Cs>
constexpr std::size_t find_char(const std::string_view s) {
    constexpr std::array<char, sizeof...(Cs)> arr = {{ Cs... }};
    std::size_t ret = 0;
    for (; ret < s.size(); ret++) {
        for (char c : arr) if (s[ret] == c) return ret;
    }
    // static_assert(s.size() <= ret, "Cs not found");
    return 0;
}

constexpr std::size_t find_str(const std::string_view s,
                               const std::string_view t)
{
    std::size_t ret = 0;
    for (; ret + t.size() <= s.size(); ret++) {
        if (s.substr(ret, t.size()) == t) return ret;
    }
    // static_assert(s.size() < t.size() + ret, "t not found");
    return 0;
}

constexpr std::string_view find_and_remove(const std::string_view s,
                                           const std::string_view t,
                                           const std::size_t len)
{
    return s.substr(find_str(s, t) + t.size() + len);
}

constexpr std::string_view remove_suffix(const std::string_view s) {
    return s.substr(0, find_char<';', ']'>(s));
}

constexpr std::string_view get_value(const std::string_view s,
                                     const std::string_view key) 
{
    constexpr std::size_t len = 3;  // " = "
    return remove_suffix(find_and_remove(s, key, len));
}

constexpr std::string_view remove_type_prefix(const std::string_view enum_type_name,
                                              const std::string_view enum_value)
{
    constexpr std::size_t len = 2;  // "::"
    return enum_value.substr(enum_type_name.size() + len);
}

constexpr std::string_view find_enum_value(const std::string_view pretty,
                                           const std::string_view type_key,
                                           const std::string_view value_key)
{
    return remove_type_prefix(get_value(pretty, type_key), get_value(pretty, value_key));
}

template <typename EnumType, EnumType EnumValue>
constexpr std::string_view to_str_impl() {
    return find_enum_value(std::string_view(__PRETTY_FUNCTION__),
                           std::string_view("EnumType"),
                           std::string_view("EnumValue"));
}

template <typename EnumType, std::size_t Index, std::size_t MaxLen>
constexpr std::string_view to_str_rec(const EnumType value) {
    if constexpr (Index == MaxLen) {
        return "";
    } else {
        if (static_cast<std::size_t>(value) == Index) {
            return to_str_impl<EnumType, static_cast<EnumType>(Index)>();
        } else {
            return to_str_rec<EnumType, Index + 1, MaxLen>(value);
        }
    }
}

}

template <typename EnumType, std::size_t MaxLen = 128>
constexpr std::string_view to_str(const EnumType value) {
    return internal::to_str_rec<EnumType, 0, MaxLen>(value);
}

}

#endif
