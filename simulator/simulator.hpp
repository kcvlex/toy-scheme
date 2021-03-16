#ifndef INCLUDE_SIMULATOR_SIMULATOR
#define INCLUDE_SIMULATOR_SIMULATOR

#include "cpu.hpp"

namespace simulator {

struct Simulator {
    Simulator(const std::string &filename);

    void simulate();

private:
    std::shared_ptr<DataMemory> data_mem;
    std::shared_ptr<InstructionMemory> instr_mem;
    std::shared_ptr<CPU> cpu;
};

}

#endif
