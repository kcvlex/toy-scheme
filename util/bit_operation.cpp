#include "bit_operation.hpp"
#include <cassert>

namespace assembler {

BitOperation::BitOperation() noexcept : BitOperation(0) 
{
}

BitOperation::BitOperation(const value_type val_arg) noexcept : val(val_arg) 
{
}

BitOperation::BitOperation(const std::int32_t val_arg) noexcept : val(val_arg) 
{
}

BitOperation BitOperation::operator()(const std::uint8_t r, const std::uint8_t l) const {
    assert(0 <= l && l < 32);
    assert(0 <= r && r < 32);
    assert(l <= r);
    auto b = val;
    b >>= l;
    const auto len = r - l + 1;
    std::bitset<64> mask((1ull << len) - 1);
    b &= mask;
    return BitOperation(b);
}

BitOperation& BitOperation::operator>>=(const std::uint8_t w) noexcept {
    bool minus = val.test(63);
    val >>= w;
    if (minus) {
        if (64 <= w) {
            val = value_type(-1);
        } else {
            std::uint8_t rest = 64 - w;
            std::uint64_t v = 1ull << rest;
            v--;
            val |= (~value_type(v));
        }
    }
    return *this;
}

BitOperation& BitOperation::operator<<=(const std::uint8_t w) noexcept { val <<= w; return *this; }
BitOperation& BitOperation::operator&=(const BitOperation &rhs) noexcept { val &= rhs.val.to_ullong(); return *this; }
BitOperation& BitOperation::operator|=(const BitOperation &rhs) noexcept { val |= rhs.val.to_ullong(); return *this; }
BitOperation& BitOperation::operator^=(const BitOperation &rhs) noexcept { val ^= rhs.val.to_ullong(); return *this; }

BitOperation BitOperation::operator>>(const std::uint8_t w) const noexcept { return BitOperation(*this) >>= w; }
BitOperation BitOperation::operator<<(const std::uint8_t w) const noexcept { return BitOperation(*this) <<= w; }
BitOperation BitOperation::operator&(const BitOperation &rhs) const noexcept { return BitOperation(*this) &= rhs; }
BitOperation BitOperation::operator|(const BitOperation &rhs) const noexcept { return BitOperation(*this) |= rhs; }
BitOperation BitOperation::operator^(const BitOperation &rhs) const noexcept { return BitOperation(*this) ^= rhs; }
BitOperation BitOperation::operator~() const noexcept { return BitOperation(~val); }
bool BitOperation::operator[](std::size_t i) const { return val.test(i); }

std::uint32_t BitOperation::get() const noexcept { return static_cast<std::uint32_t>(val.to_ullong()); }

}
