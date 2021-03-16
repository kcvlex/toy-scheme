#ifndef INCLUDE_SIMULATOR_ASM_LOADER
#define INCLUDE_SIMULATOR_ASM_LOADER

#include <vector>
#include <string>
#include <unordered_map>
#include "instr.hpp"

namespace simulator {

/*
 * BNF
 * ==========================
 * ASM     := LINE | ASM
 * LINE    := SPACES CONTENT SPACES COMMENT ENDL
 * SPACES := SPACE*
 * CONTENT := DIRECTIVE | LABEL | INSTR | COMMENT
 * DIRECTIVE := .align | .section | ...
 * LABEL     := LETTERS ':'
 * INSTR    := LABEL SPACE SPACES INSTR-BODY | INSTR-BODY
 * INSTR-BODY := INSTR-NAME SPACE SPACES OPERANDS
 * INSTR-NAME := LETTERS
 * OPERANDS := OPERAND (',' OPERAND)*
 * OPERAND := REG | IMM | IMM '(' REG ')'
 * REG := ...
 * IMM := DIGITS | '-' DIGITS
 *
 * COMMENT := '#' .*
 * DIGITS := [1-9][0-9]*
 * LETTERS := [a-zA-Z]+
 * SPACE := ' ' | '\t'
 * ENDL := '\n'
 */

struct AsmDirective {
    std::string dir;
};

struct AsmOperand {
    reg_opt_type reg;
    imm_opt_type imm;
};

struct AsmLabel {
    std::string label;
};

struct AsmInstr {
    opcode_type op;
    std::vector<AsmOperand> operands;
};

struct AsmParser {
    using result_type = std::vector<asm_line_type>;
    using asm_line_type = std::variant<AsmDirective, AsmLabel, AsmInstr>;

    AsmParser() = delete;
    AsmParser(std::istream *is_arg);
    
    const result_type& result() const noexcept;

private:
    result_type res_priv;
    std::istream* is;

    void parse();
    bool parse_line();

    AsmDirective parse_dir();
    AsmOperand parse_operand();
    AsmInstr parse_instr(const std::string &instr_name);

    reg_opt_type parse_reg();
    imm_opt_type parse_imm();

    bool is_delim(const char c) const noexcept;
    bool is_endl(const char c) const noexcept;

    void eat_space();
    void eat_spaces();
    void eat_comma();
    void eat_lparen();
    void eat_rparen();
    void eat_endl();
    void eat_comment();
};

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
