#include "asm_loader.hpp"
#include <cassert>
#include <fstream>
#include <algorithm>
#include "cpu.hpp"
#include "util/enum2str.hpp"
#include <iostream>

namespace simulator {

namespace {

constexpr std::size_t opcode_size = static_cast<std::size_t>(opcode_type::Size);
constexpr std::size_t reg_size = 32;
std::array<std::string, opcode_size> opcode2str;
std::array<std::string, reg_size> reg2str;

struct Initializer {
    Initializer() {
        // opcode
        for (std::size_t i = 0; i < opcode_size; i++) {
            const opcode_type opcode = static_cast<opcode_type>(i);
            opcode2str[i] = util::to_str(opcode);
        }

        for (std::size_t i = 0; i < reg_size; i++) {
            const reg_type reg = static_cast<reg_type>(i);
            reg2str[i] = util::to_str(reg);
        }
    }
} initializer;

template <typename EnumType, std::size_t Size>
EnumType str2enum_value(const std::string &s, const std::array<std::string, Size> &arr) {
    const auto ite = std::find(std::cbegin(arr), std::cend(arr), s);
    if (ite == std::cend(arr)) {
        throw std::runtime_error(s + " is invalid");
    }
    const auto idx = std::distance(std::cbegin(arr), ite);
    return static_cast<EnumType>(idx);
}

void scan_ss(std::stringstream &ss, const char delim) {
    std::string s;
    if (std::getline(ss, s, delim)) throw std::runtime_error(s);
}

template <typename Head, typename... Tails>
void scan_ss(std::stringstream &ss, const char delim, Head &&head, Tails&&... tails) {
    if (!std::getline(ss, std::forward<Head>(head), delim)) {
        throw std::runtime_error("EOF");
    }
    scan_ss(ss, delim, std::forward<Tails>(tails)...);
}

}  // anonymouse


AsmFileLoader::AsmFileLoader(std::string filename_arg)
    : cur_addr(0),
      filename(std::move(filename_arg))
{
    label2addr["exit"] = CPU::exit_addr;
    instrs.reserve(set_label2addr());
    build_instr_seq();
}

const std::vector<Instruction>& AsmFileLoader::get_seq() const noexcept {
    return instrs;
}

std::string AsmFileLoader::remove_noise(const std::string &s) const {
    std::string res;
    bool is_indent = true;
    const auto is_space = [&](char c) { return c == ' ' || c == '\t'; };
    for (char c : s) {
        if (!is_space(c)) is_indent = false;
        if (c == '\n') break;
        if (c == '#') break;
        if (!is_indent) res += c;
    }
    while (!res.empty() && is_space(res.back())) res.pop_back();
    return res;
}

bool AsmFileLoader::is_label(const std::string &s) const {
    const auto ite = std::find(std::cbegin(s), std::cend(s), ':');
    return ite != std::cend(s);
}

opcode_type AsmFileLoader::str2opcode(const std::string &s) const {
    return str2enum_value<opcode_type, opcode_size>(s, opcode2str);
}

reg_type AsmFileLoader::str2reg(const std::string &s) const {
    return str2enum_value<reg_type, reg_size>(s, reg2str);
}

addr_type AsmFileLoader::get_addr(const std::string &s) const {
    const auto ite = label2addr.find(s);
    if (ite == std::cend(label2addr)) {
        throw std::runtime_error(s + " is invalid label");
    }
    return ite->second;
}

// e.g. -16(sp)
std::pair<reg_type, imm_value_type> AsmFileLoader::parse_ref(const std::string &ref) const {
    const auto l_ite = std::find(std::cbegin(ref), std::cend(ref), '(');
    const auto r_ite = std::find(std::cbegin(ref), std::cend(ref), ')');

    reg_type reg;
    imm_value_type imm;
    
    const bool validate = [&] {
        if (l_ite == std::cend(ref)) return false;
        if (r_ite == std::cend(ref)) return false;
        if (std::next(r_ite) != std::cend(ref)) return false;
        const auto s_imm = std::string(std::cbegin(ref), l_ite);
        const auto s_reg = std::string(std::next(l_ite), r_ite);
        reg = str2reg(s_reg);
        imm = std::stoi(s_imm);
        return true;
    }();

    if (!validate) throw std::runtime_error(ref + " is invalid format");
    return std::make_pair(reg, imm);
}

Instruction AsmFileLoader::parse_line(const std::string &line) const {
    std::string s_instr, operands;

    {
        std::stringstream ss;
        ss << line;
        ss >> s_instr >> operands;
        assert(ss.eof());
    }

    const opcode_type opcode = str2opcode(s_instr);

    std::stringstream ss;
    ss << operands;

    if (opcode == opcode_type::JAL) {
        std::string s_rd, label;
        scan_ss(ss, comma, s_rd, label);
        const auto rd = str2reg(s_rd);
        const auto addr = get_addr(label);
        return make_jal(rd, addr);
    } else if (opcode == opcode_type::JALR) {
        std::string s_rd, ref;
        scan_ss(ss, comma, s_rd, ref);
        const auto rd = str2reg(s_rd);
        const auto [ rs1, imm ] = parse_ref(ref);
        return make_jalr(rs1, rd, imm);
    } else if (is_branch_instr(opcode)) {
        std::string s_rs1, s_rs2, label;
        scan_ss(ss, comma, s_rs1, s_rs2, label);
        const auto rs1 = str2reg(s_rs1), rs2 = str2reg(s_rs2);
        const auto addr = get_addr(label);
        const auto offset = imm_value_type(addr) - imm_value_type(cur_addr);  // FIXME : overflow
        return make_branch(opcode, rs1, rs2, offset);
    } else if (is_load_instr(opcode)) {
        std::string s_rd, ref;
        scan_ss(ss, comma, s_rd, ref);
        const auto rd = str2reg(s_rd);
        const auto [ rs1, imm ] = parse_ref(ref);
        return make_lw(opcode, rs1, rd, imm);
    } else if (is_store_instr(opcode)) {
        std::string s_rs2, ref;
        scan_ss(ss, comma, s_rs2, ref);
        const auto rs2 = str2reg(s_rs2);
        const auto [ rs1, imm ] = parse_ref(ref);
        return make_sw(opcode, rs1, rs2, imm);
    } else if (is_op_imm_instr(opcode)) {
        std::string s_rd, s_rs1, s_imm;
        scan_ss(ss, comma, s_rd, s_rs1, s_imm);
        return make_op_imm(opcode, 
                           str2reg(s_rs1),
                           str2reg(s_rd),
                           std::stoi(s_imm));
    } else if (is_op_instr(opcode)) {
        std::string s_rd, s_rs1, s_rs2;
        scan_ss(ss, comma, s_rd, s_rs1, s_rs2);
        return make_op(opcode,
                       str2reg(s_rs1),
                       str2reg(s_rs2),
                       str2reg(s_rd));
    } else {
        assert(false);
    }
}

std::uint32_t AsmFileLoader::set_label2addr() {
    std::uint32_t res = 0;
    std::ifstream ifs(filename);
    std::string line;
    while (std::getline(ifs, line)) {
        line = std::move(remove_noise(line));
        if (line.empty()) continue;
        if (is_label(line)) {
            line.pop_back();
            label2addr[line] = res * 4;  // FIXME : duplicate
        } else {
            res++;
        }
    }
    return res;
}

void AsmFileLoader::build_instr_seq() {
    std::ifstream ifs(filename);
    std::string line;
    while (std::getline(ifs, line)) {
        line = std::move(remove_noise(line));
        if (line.empty()) continue;
        if (is_label(line)) continue;
        const auto val = parse_line(line);
        instrs.push_back(val);
        cur_addr += 4;
    }
}

}
