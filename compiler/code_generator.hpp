#ifndef INCLUDE_COMPILER_CODE_GENRATOR
#define INCLUDE_COMPILER_CODE_GENRATOR

#if 0
#include <unordered_map>
#include <optional>
#include "cps.hpp"
#include "data_type.hpp"
#include "three_address_code.hpp"

namespace compiler {

struct RegisterMapping {
    using vreg_opt_type = std::optional<VirtualRegister>;

    RegisterMapping();
    
    void set_reg(const std::string &name, const VirtualRegister &reg);
    VirtualRegister get_new_reg();
    vreg_opt_type get_reg(const std::string &name) const;

private:
    std::unordered_map<std::string, VirtualRegister> map;
    std::size_t reg_counter;
};
 
 
/*
 * ==================== Environment ====================
 *
 *       |                               |
 *       ---------------------------------
 * sp -> | ref_n                         |
 *       ---------------------------------
 *       | ...                           |
 *       ---------------------------------
 *       | ref_1                         |
 *       ---------------------------------
 *       | ref_0 (entry of new ex-scope) |
 *       ---------------------------------
 *       | ...                           |
 *       ---------------------------------
 *       | local_1                       |
 *       ---------------------------------
 *       | local_0                       |
 *       ---------------------------------
 *       | ...                           |
 *       ---------------------------------
 *       | arg_1 (address to data)       |
 *       ---------------------------------
 *       | arg_0 (address to ext_refs)   |
 *       ---------------------------------
 *       | address of ref_0              |
 *       ---------------------------------
 *       | size                          |
 *       ---------------------------------
 * bp -> | tag                           |
 *       ---------------------------------
 *       |                               |
 *
 *
 * Each of an arg must be an address to a data.
 * Each of a local must be a continuation.
 * Each of a ref must be an address to a data.
 *
 */

struct FunctoinCodeGenerator {
    FunctionCodeGenerator() = delete;
    FunctionCodeGenerator(std::string label_arg, LambdaCPS* const func_arg);

private:
    std::string label;
    LambdaCPS *func;
    RegisterMapping regmap;
    CodeSequence code_seq;

    imm_value_type tag2imm(const DataTypeTag tag);
    void build_env();
};

}

#endif

#endif
