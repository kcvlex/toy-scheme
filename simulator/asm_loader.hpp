#ifndef INCLUDE_SIMULATOR_ASM_LOADER
#define INCLUDE_SIMULATOR_ASM_LOADER

#include <vector>
#include <string>
#include <unordered_map>
#include "instr.hpp"

namespace simulator {

struct AsmFileLoader {

    AsmFileLoader() = delete;
    AsmFileLoader(std::string filename_arg);

    const std::vector<Instruction>& get_seq() const noexcept;

private:
    constexpr static char comma = ',';
    addr_type cur_addr;
    std::string filename;
    std::vector<Instruction> instrs;
    std::unordered_map<std::string, addr_type> label2addr;

    std::string remove_noise(const std::string &s) const;  // remove indent, comment
    bool is_label(const std::string &s) const;
    opcode_type str2opcode(const std::string &s) const;
    reg_type str2reg(const std::string &s) const;
    addr_type get_addr(const std::string &s) const;
    std::pair<reg_type, imm_value_type> parse_ref(const std::string &s) const;
    Instruction parse_line(const std::string &line) const;

    std::uint32_t set_label2addr();  // return num of instrucions
    void build_instr_seq();
};

}

#endif
