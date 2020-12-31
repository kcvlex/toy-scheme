#ifndef INCLUDE_SEM_ANALYZER
#define INCLUDE_SEM_ANALYZER

#include "ast.hpp"
#include "three_address_code.hpp"
#include <list>
#include <optional>
#include <array>
#include <stack>

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

struct SymbolTable {
    using symbols_type = std::list<SymboledValue>;
    using arg_regs_map = std::array<SymboledValue*, arg_reg_num>;

    SymbolTable();
    void set_arg_mapping(const LambdaNode *lambda, const std::uint32_t cur_nest);
    void restore_arg_mapping();
    SymboledValue* find(const std::string &name) const;
    std::optional<Reg> find_arg(const std::string &name) const;

private:
    arg_regs_map gen_default_map() const;

    symbols_type symbols;
    arg_regs_map regs;
    std::stack<arg_regs_map, std::vector<arg_regs_map>> history;
};

struct FunctionCode {
    std::uint32_t label;
    OutputCodeStream cs;

    FunctionCode(std::uint32_t label_arg, OutputCodeStream ocs_arg);
};

enum class BuiltinOperation { ADD, SUB, };
struct eval_type { };
using lambda_function_type = std::uint32_t;  // label
using symbol_type = std::add_pointer_t<const SymbolNode>;
using operation_type = std::variant<BuiltinOperation, lambda_function_type, symbol_type, eval_type>;

struct SemanticAnalyzer : public ConstNodeVisitor {
    SemanticAnalyzer();

    virtual void visit(const EvalNode *node) override;
    virtual void visit(const LambdaNode *node) override;
    virtual void visit(const SymbolNode *node) override;
    virtual void visit(const ConstantNode *node) override;

    std::vector<FunctionCode> analyze(const std::vector<ASTNode*> &nodes);
    
private:
    SymbolTable table;
    std::uint32_t cur_nest;
    OutputCodeStream cur_code;
    std::vector<FunctionCode> fcodes;
    std::vector<OutputCodeStream> code_buf;

    OutputCodeStream callee_prolog(const LambdaNode *lambda);
    OutputCodeStream callee_epilog(const LambdaNode *lambda);
};

struct SimpleInstructionChecker : public ConstNodeVisitor {
    virtual void visit(const EvalNode *node) override;
    virtual void visit(const LambdaNode *node) override;
    virtual void visit(const SymbolNode *node) override;
    virtual void visit(const ConstantNode *node) override;
    std::optional<operation_type> res = std::optional<operation_type>(std::nullopt);
};

}

#endif
