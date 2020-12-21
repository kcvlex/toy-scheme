#ifndef INCLUDE_BIT_OPERATION
#define INCLUDE_BIT_OPERATION

#include <cstdint>

namespace assembler {

struct BitOperation {
    using value_type = std::bitset<64>();

    BitOperation() noexcept;
    BitOperation(const value_type val_arg) noexcept;
    BitOperation(const std::int32_t val_arg) noexcept;

    // return val[r:l] (like verilog HDL)
    BitOperation operator()(const std::uint8_t r, const std::uint8_t l) const;
    
    // arithmetic right bit shift
    BitOperation& operator>>=(const std::uint8_t w) noexcept;
    BitOperation& operator<<=(const std::uint8_t w) noexcept;
    BitOperation& operator&=(const BitOperation &rhs) noexcept;
    BitOperation& operator|=(const BitOperation &rhs) noexcept;
    BitOperation& operator^=(const BitOperation &rhs) noexcept;

    BitOperation operator>>(const std::uint8_t w) const noexcept;
    BitOperation operator<<(const std::uint8_t w) const noexcept;
    BitOperation operator&(const BitOperation &rhs) const noexcept;
    BitOperation operator|(const BitOperation &rhs) const noexcept;
    BitOperation operator^(const BitOperation &rhs) const noexcept;
    BitOperation operator~() const noexcept;
    bool operator[](std::size_t i) const;

    std::uint32_t get() const noexcept;

private:
    value_type val;
};

}

#endif
