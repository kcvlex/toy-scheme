#ifndef INCLUDE_SIMULATOR_CPU
#define INCLUDE_SIMULATOR_CPU

#include <memory>
#include "memory.hpp"
#include "regfile.hpp"

namespace simulator {

struct CPU {
    using value_type = typename DataMemory::value_type;
    constexpr static addr_type exit_addr = 16384;

    enum class OperationTag {
        ADD, SUB, XOR, SLL, SRL, SRA, OR, AND,
        EQ, NEQ, LT, LTU, GE
    };

    CPU() = delete;
    CPU(const std::shared_ptr<DataMemory> data_mem_arg,
        const std::shared_ptr<InstructionMemory> instr_mem_arg);

    bool step();

private:
    std::shared_ptr<DataMemory> data_mem;
    std::shared_ptr<InstructionMemory> instr_mem;
    Regfile regfile;
    addr_type pc;

    value_type ALU(const OperationTag op_tag,
                   const value_type rss1_op, 
                   const value_type rss2_op) const noexcept;
    OperationTag op_tag(const opcode_type opcode) const noexcept;

    friend struct Simulator;
};

}

#endif
