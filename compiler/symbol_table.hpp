#ifndef INCLUDE_SIMBOL_TABLE
#define INCLUDE_SIMBOL_TABLE

#include <array>
#include <optional>
#include <stack>
#include <list>
#include "three_address_code.hpp"

namespace compiler {

struct SymboledValue {
    std::string name;
    const std::uint32_t nest;
    const imm_value_type offset;

    SymboledValue(std::string name_arg, 
                  const std::uint32_t nest_arg, 
                  const imm_value_type offset_arg);

    SymboledValue(const std::uint32_t nest_arg, 
                  const imm_value_type offset_arg);
};

struct ArgumentsMapper {
    template <typename T> using vstack = std::stack<T, std::vector<T>>;
    using arg_regs_map = std::array<SymboledValue*, arg_reg_num - 1>;
    using arg_map_state = std::pair<arg_regs_map, std::size_t>;

    ArgumentsMapper();
    void push(const LambdaNode* const lambda, const std::uint32_t cur_nest);
    void pop();
    std::optional<Reg> find(const std::string &name) const noexcept;
    std::size_t arg_num() const noexcept;

private:
    vstack<arg_map_state> history;

    arg_regs_map gen_default_map() const;
};

struct SymbolTable {
    using symbols_type = std::list<SymboledValue>;

    SymbolTable();
    void set_arg_map(const LambdaNode* const lambda, const std::uint32_t cur_nest);
    void restore_arg_map();
    SymboledValue* find(const std::string &name) const;
    std::optional<Reg> find_arg(const std::string &name) const noexcept;
    std::size_t arg_num() const noexcept;

private:
    symbols_type symbols;
    ArgumentsMapper arg_mapper;
};


}

#endif
