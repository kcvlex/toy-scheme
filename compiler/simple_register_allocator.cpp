#include "simple_register_allocator.hpp"
#include <cassert>

namespace compiler {

SimpleRegisterAllocator::SimpleRegisterAllocator(const std::size_t passed_arg_num_arg)
    : used_tmp_reg(0),
      used_callee_saved_reg(1),  // Reg::s0 is stack base pointer
      passed_arg_num(passed_arg_num_arg)
{
}

Reg SimpleRegisterAllocator::get_tmp_reg() {
    assert(used_tmp_reg < tmp_reg_num);
    return nth_tmp_reg(used_tmp_reg++);
}

Reg SimpleRegisterAllocator::get_callee_saved_reg() {
    assert(used_callee_saved_reg < callee_saved_reg_num);
    return nth_callee_saved_reg(used_callee_saved_reg++);
}

std::size_t SimpleRegisterAllocator::used_tmp_num() const noexcept {
    return used_tmp_reg;
}

std::size_t SimpleRegisterAllocator::used_callee_saved_num() const noexcept {
    return used_callee_saved_reg;
}

std::size_t SimpleRegisterAllocator::arg_num() const noexcept {
    return passed_arg_num;
}

}
