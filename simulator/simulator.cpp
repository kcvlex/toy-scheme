#include "simulator.hpp"
#include "asm_loader.hpp"
#include "util/enum2str.hpp"
#include <iostream>

namespace simulator {

Simulator::Simulator(const std::string &filename) {
    AsmFileLoader loader(filename);
    data_mem = std::make_shared<DataMemory>();
    instr_mem = std::make_shared<InstructionMemory>(loader.get_seq());
    cpu = std::make_shared<CPU>(data_mem, instr_mem);
}

std::ostream& operator<<(std::ostream &os, const data_opt_type &data_opt) {
    if (data_opt.has_value()) {
        return (os << *data_opt);
    } else {
        return (os << std::string(32, 'x'));
    }
}

void Simulator::simulate() {
    while (cpu->step()) {
        std::cout << cpu->regfile.read(reg_type::a0) << std::endl;
    }
}

}
