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
    std::string label;
    InputCodeStream cs;

    FunctionCode(std::string label_arg, InputCodeStream cs_arg);
};

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
    InputCodeStream cur_code;
    std::vector<FunctionCode> fcodes;
    std::vector<InputCodeStream> code_buf;
    std::stack<Operation, std::vector<Operation>> op_stk;
    std::stack<std::string, std::vector<std::string>> label_stk;

    InputCodeStream callee_prolog(const LambdaNode *lambda);
    InputCodeStream callee_epilog(const LambdaNode *lambda);
};

struct SimpleInstructionChecker : public ConstNodeVisitor {
    virtual void visit(const EvalNode *node) override;
    virtual void visit(const LambdaNode *node) override;
    virtual void visit(const SymbolNode *node) override;
    virtual void visit(const ConstantNode *node) override;

    bool get() const noexcept;

private:
    bool res = true;
};

}

#endif
