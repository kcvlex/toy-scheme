#include "cpu.hpp"
#include "util/bit_operation.hpp"
#include <limits>
#include <cassert>
#include <iostream>
#include "util/enum2str.hpp"

namespace simulator {

CPU::CPU(const std::shared_ptr<DataMemory> data_mem_arg,
         const std::shared_ptr<InstructionMemory> instr_mem_arg)
    : data_mem(data_mem_arg),
      instr_mem(instr_mem_arg),
      regfile(),
      pc(0)
{
    regfile.write(reg_type::sp, DataMemory::mem_size - 4);
    regfile.write(reg_type::s0, DataMemory::mem_size - 4);  // base pointer
}

bool CPU::step() {
    if (pc == exit_addr) return false;

    const auto instr = instr_mem->read(pc).value();
    pc += 4;
    const auto op = instr.op;
    const auto tag = op_tag(op);

    if (op == opcode_type::JAL) {
        const auto tmp = pc;
        pc = instr.imm.value();
        regfile.write(instr.rd, tmp);
        return true;
    }

    if (op == opcode_type::JALR) {
        const auto tmp = pc;
        const auto rss1 = regfile.read(instr.rs1);
        pc = ALU(tag, rss1, instr.imm).value();
        regfile.write(instr.rd, tmp);
        return true;
    }

    // FIXME
    if (is_branch_instr(op)) {
        const auto rss1 = regfile.read(instr.rs1);
        const auto rss2 = regfile.read(instr.rs2);
        const auto jump = ALU(OperationTag::ADD, pc, instr.imm);
        const auto cond = ALU(tag, rss1, rss2);
        if (cond.value()) pc = jump.value() - 4;
        return true;
    }

    // FIXME : only LW
    if (is_load_instr(op)) {
        assert(op == opcode_type::LW);
        const auto rss1 = regfile.read(instr.rs1);
        const auto addr = ALU(tag, rss1, instr.imm);
        regfile.write(instr.rd, data_mem->read(addr));
        return true;
    }

    // FIXME : only SW
    if (is_store_instr(op)) {
        assert(op == opcode_type::SW);
        const auto rss1 = regfile.read(instr.rs1),
                   rss2 = regfile.read(instr.rs2);
        const auto addr = ALU(tag, rss1, instr.imm);
        data_mem->write(addr, rss2);
        return true;
    }
    
    if (is_op_imm_instr(op)) {
        const auto rss1 = regfile.read(instr.rs1);
        const auto res = ALU(tag, rss1, instr.imm);
        regfile.write(instr.rd, res);
        return true;
    }

    if (is_op_instr(op)) {
        const auto rss1 = regfile.read(instr.rs1),
                   rss2 = regfile.read(instr.rs2);
        const auto res = ALU(tag, rss1, rss2);
        regfile.write(instr.rd, res);
        return true;
    }

    return false;
}

CPU::OperationTag CPU::op_tag(const opcode_type opcode) const noexcept {
    switch (opcode) {
        case opcode_type::ADD:
        case opcode_type::ADDI:
            return OperationTag::ADD;
        case opcode_type::SLT:
        case opcode_type::SLTI:
            return OperationTag::LT;
        case opcode_type::SLTU:
        case opcode_type::SLTIU:
            return OperationTag::LTU;
        case opcode_type::XOR:
            return OperationTag::XOR;
        case opcode_type::SLL:
        case opcode_type::SLLI:
            return OperationTag::SLL;
        case opcode_type::SRL:
        case opcode_type::SRLI:
            return OperationTag::SRL;
        case opcode_type::SRA:
        case opcode_type::SRAI:
            return OperationTag::SRA;
        case opcode_type::OR:
        case opcode_type::ORI:
            return OperationTag::OR;
        case opcode_type::AND:
        case opcode_type::ANDI:
            return OperationTag::AND;
        case opcode_type::BEQ:
            return OperationTag::EQ;
        case opcode_type::BNE:
            return OperationTag::NEQ;
        case opcode_type::BLT:
            return OperationTag::LT;
        case opcode_type::BGE:
            return OperationTag::GE;
        default:
            return OperationTag::ADD;
    }
}

CPU::value_type CPU::ALU(const OperationTag tag,
                         const value_type rss1_op,
                         const value_type rss2_op) const noexcept
{
    if (!(rss1_op.has_value() && rss2_op.has_value())) return std::nullopt;
    const auto rss1 = *rss1_op, rss2 = *rss2_op;
    const auto mask32 = 1u << 31;
    const bool bit32 = !!(mask32 & rss1);
    util::BitOperation bop(rss1);

    switch (tag) {
        case OperationTag::ADD:
            return rss1 + rss2;
        case OperationTag::SUB:
            return rss1 - rss2;
        case OperationTag::XOR:
            return rss1 ^ rss2;
        case OperationTag::SLL:
            return rss1 << rss2;
        case OperationTag::SRL:
            bop >>= rss2;
            return bop.get();
        case OperationTag::SRA:
            bop >>= rss2;
            if (bit32) {
                // 00...0011...11
                util::BitOperation bmask(std::numeric_limits<std::uint32_t>::max());
                
                // 11...1100...00
                bmask = ~bmask;

                bmask >>= rss2;
                bop |= bmask;
            }
            return bop.get();
        case OperationTag::OR:
            return rss1 | rss2;
        case OperationTag::AND:
            return rss1 & rss2;
        case OperationTag::EQ:
            return rss1 == rss2;
        case OperationTag::NEQ:
            return rss1 != rss2;
        case OperationTag::LT:
            return std::int32_t(rss1) < std::int32_t(rss2);
        case OperationTag::LTU:
            return rss1 < rss2;
        default:
            return std::nullopt;
    }
}

}
