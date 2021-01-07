#ifndef INCLUDE_SIMPLE_REGISTER_ALLOCATOR
#define INCLUDE_SIMPLE_REGISTER_ALLOCATOR

#include "registers.hpp"

namespace compiler {

struct SimpleRegisterAllocator {
    SimpleRegisterAllocator() = delete;
    SimpleRegisterAllocator(const std::size_t passed_arg_num_arg);
    
    Reg get_tmp_reg();
    Reg get_callee_saved_reg();
    std::size_t used_tmp_num() const noexcept;
    std::size_t used_callee_saved_num() const noexcept;
    std::size_t arg_num() const noexcept;

private:
    std::size_t used_tmp_reg, used_callee_saved_reg, passed_arg_num;
};

}

#endif
