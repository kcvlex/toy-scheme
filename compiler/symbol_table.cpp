#include "sem_analyzer.hpp"
#include <cassert>
#include <iostream>
#include <limits>

namespace compiler {


/******************** SymboledValue ********************/

SymboledValue::SymboledValue(std::string name_arg,
                             const std::uint32_t nest_arg, 
                             const imm_value_type offset_arg)
    : name(std::move(name_arg)),
      nest(nest_arg),
      offset(offset_arg)
{
}

SymboledValue::SymboledValue(const std::uint32_t nest_arg,
                             const imm_value_type offset_arg)
    : SymboledValue("", nest_arg, offset_arg)
{
}


/******************** ArgumentsMapper ********************/

ArgumentsMapper::ArgumentsMapper() {
    history.emplace(gen_default_map(), 0); // sentinel
}

void ArgumentsMapper::push(const LambdaNode* const lambda, const std::uint32_t cur_nest) {
    auto rmap = gen_default_map();
    std::size_t sz = 0;
    for (; sz != lambda->get_args().size(); sz++) {
        const auto &arg = lambda->get_args()[sz];
        rmap[sz] = new SymboledValue(arg->get_symbol(), cur_nest, 0);
    }
    history.emplace(rmap, sz);
}

void ArgumentsMapper::pop() {
    history.pop();
}

std::optional<Reg> ArgumentsMapper::find(const std::string &name) const noexcept {
    const auto &[ regs, sz ] = history.top();
    for (std::size_t i = 0; i != sz; i++) {
        if (regs[i]->name == name) return std::make_optional(nth_arg_reg(i));
    }
    return std::optional<Reg>(std::nullopt);
}

std::size_t ArgumentsMapper::arg_num() const noexcept {
    return history.top().second;
}

ArgumentsMapper::arg_regs_map ArgumentsMapper::gen_default_map() const {
    arg_regs_map res;
    std::fill(std::begin(res), std::end(res), nullptr);
    return res;
}


/******************** SymboleTable ********************/

SymbolTable::SymbolTable() {
}

void SymbolTable::set_arg_map(const LambdaNode* const lambda, const std::uint32_t cur_nest) {
    arg_mapper.push(lambda, cur_nest);
}

void SymbolTable::restore_arg_map() {
    arg_mapper.pop();
}

SymboledValue* SymbolTable::find(const std::string &name) const {
    // Search stack
    // FIXME : impl

    return nullptr;
}

std::optional<Reg> SymbolTable::find_arg(const std::string &name) const noexcept {
    return arg_mapper.find(name);
}

std::size_t SymbolTable::arg_num() const noexcept {
    return arg_mapper.arg_num();
}

}
