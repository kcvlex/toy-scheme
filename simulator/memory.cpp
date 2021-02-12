#include "memory.hpp"
#include <iostream>

namespace simulator {

constexpr bool check_addr(const addr_type addr) {
    return (addr & 0b11) == 0;
}

InstructionMemory::InstructionMemory(const super_type &vec)
    : vector(vec)
{
}

std::optional<Instruction> InstructionMemory::read(const addr_opt_type addr_opt) const {
    if (!addr_opt.has_value()) return std::nullopt;
    
    const auto addr = *addr_opt;
    if (!check_addr(addr)) return std::nullopt;
   
    const auto idx = addr / 4;
    if (vector::size() <= idx) return std::nullopt;
   
    return vector::operator[](idx);
}

DataMemory::DataMemory() {
    std::fill(std::begin(mem), std::end(mem), std::nullopt);
}

template <typename T>
std::ostream& operator<<(std::ostream &os, const std::optional<T> &val) {
    if (val.has_value()) return (os << *val);
    else return (os << "nullopt");
}

void DataMemory::write(const addr_opt_type addr_opt, const value_type dat) {
    if (!addr_opt.has_value()) return;

    const auto addr = *addr_opt;
    if (!check_addr(addr)) return;
    
    const auto idx = addr / 4;
    if (mem.size() < idx) return;

    std::cout << "Written : [" << idx << ' ' << dat << "]\n";
    mem[idx] = dat;
}

DataMemory::value_type DataMemory::read(const addr_opt_type addr_opt) const {
    if (!addr_opt.has_value()) return std::nullopt;

    const auto addr = *addr_opt;
    if (!check_addr(addr)) return std::nullopt;

    const auto idx = addr / 4;
    if (mem.size() <= idx) return std::nullopt;

    return mem[idx];
}

}
