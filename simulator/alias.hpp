#ifndef INCLUDE_SIMULATOR_ALIAS
#define INCLUDE_SIMULATOR_ALIAS

#include <optional>
#include "compiler/alias.hpp"
#include "compiler/three_address_code.hpp"

namespace simulator {

using opcode_type = compiler::Instructions;
using reg_type = compiler::PhysicalRegister;
using compiler::imm_value_type;
using compiler::addr_type;
using data_type = std::uint32_t;

using reg_opt_type = std::optional<reg_type>;
using imm_opt_type = std::optional<imm_value_type>;
using data_opt_type = std::optional<data_type>;
using addr_opt_type = std::optional<addr_type>;

}

#endif
