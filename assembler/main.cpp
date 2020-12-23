#include "assembler.hpp"
#include <iostream>
#include <bitset>

int main() {
    const std::vector<std::string> instr_v = {
        "addi    t0,zero,-48",
        // "sw  s0,44(sp)",
        "addi    t1,zero,42",
        "add     t2,t0,t1",
        // "sw  a0,-36(s0)",
        // "sw  zero,-20(s0)",
        // "li  a5,1",
        // "sw  a5,-24(s0)",
    };

    assembler::Assembler assm;
    std::cout << "p.imem.mem[0] = 32'd0;" << std::endl;
    for (std::size_t i = 0; i != instr_v.size(); i++) {
        auto res = assm.encode(instr_v[i]);
        std::cout << "p.imem.mem[" << i + 1 << "] = 32'b" << std::bitset<32>(res) << ";" << std::endl;
    }
    return 0;
}
