#include "code_generator.hpp"

namespace compiler {


/******************** RegisterMapping ********************/

RegisterMapping::RegisterMapping()
    : reg_counter(0)
{
}

void RegisterMapping::set_reg(const std::string &name,
                              const VirtualRegister &reg)
{
    map[name] = reg;
}

VirtualRegister RegisterMapping::get_new_reg() {
    return VirtualRegister::make_normal_reg(reg_counter++);
}

RegisterMapping::vreg_opt_type RegisterMapping::get_reg(const std::string &var_name) const {
    const auto ite = map.find(var_name);
    if (ite == map.end()) return vreg_opt_type(std::nullopt);
    return vreg_opt_type(ite->second);
}


/******************** FunctionCodeGenerator ********************/

FunctionCodeGenerator::FunctionCodeGenerator(std::string label_arg,
                                             LambdaCPS* const func_arg)
    : label(std::move(label_arg)),
      func(func_arg),
      regmap(),
      code_seq()
{
}

imm_value_type FunctionCodeGenerator::tag2imm(const DataTypeTag tag) {
    return static_cast<imm_value_type>(tag);
}

void FunctionCodeGenerator::build_env() {
    imm_value_type offset = 0;
    const imm_value_type arg_sz = func->get_arg_num(),
                         local_var_sz = func->get_bind_num() * 2,
                         ref_sz = (func->ext_refs ? func->ext_refs->get_refs().size() : 0);
    const imm_value_type env_sz = 1 + 1 + 1 + arg_sz + local_var_sz + ref_sz,
                         ref0_addr_offset = 1 + 1 + 1 + arg_sz + local_var_sz;

}

}

