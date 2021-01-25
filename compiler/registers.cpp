#include "registers.hpp"

namespace compiler {


/******************** VirtualRegister ********************/

VirtualRegister::VirtualRegister(const Type type_arg,
                                 const std::size_t id_arg)
    : type(type_arg),
      id(id_arg)
{
}

VirtualRegister VirtualRegister::make_normal_reg(const std::size_t id) {
    return VirtualRegister(Type::Normal, id);
}

VirtualRegister VirtualRegister::make_arg_reg(const std::size_t id) {
    return VirtualRegister(Type::Arg, id);
}

}
