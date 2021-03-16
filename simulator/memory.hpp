#ifndef INCLUDE_SIMULATOR_MEMORY
#define INCLUDE_SIMULATOR_MEMORY

#include "alias.hpp"
#include "instr.hpp"
#include <vector>

namespace simulator {

struct InstructionMemory : std::vector<Instruction> {
    using super_type = std::vector<Instruction>;
    using super_type::vector;

    InstructionMemory(const super_type &vec);

    std::optional<Instruction> read(const addr_opt_type addr_opt) const;
};

struct DataMemory {
    using value_type = data_opt_type;

    constexpr static std::size_t word_size = sizeof(data_type);
    constexpr static std::size_t mem_size = 65536 / word_size;

    DataMemory();

    void write(const addr_opt_type addr_opt, const value_type dat_opt);
    value_type read(const addr_opt_type addr) const;

private:
    std::array<value_type, mem_size> mem;
};

}

#endif
