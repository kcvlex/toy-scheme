#include "registers.hpp"
#include "util/enum2str.hpp"

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


std::ostream& operator<<(std::ostream &os, const PhysicalRegister val) {
    return (os << util::to_str(val));
}

std::ostream& operator<<(std::ostream &os, const VirtualRegister val) {
    switch (val.type) {
        case VirtualRegister::Type::StackPtr: 
            return (os << "v_sp");
        case VirtualRegister::Type::BasePtr:
            return (os << "v_bp");
        case VirtualRegister::Type::ReturnAddr:
            return (os << "v_ra");
        case VirtualRegister::Type::Arg:
            return (os << "v_arg" << val.id);
        case VirtualRegister::Type::Normal:
            return (os << "v" << val.id);
        default:
            assert(false);
    }
}

}
