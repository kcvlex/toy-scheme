#ifndef INCLUDE_SEM_ANALYZER
#define INCLUDE_SEM_ANALYZER

#include "ast.hpp"
#include "symbol_table.hpp"
#include "simple_register_allocator.hpp"
#include <optional>
#include <stack>
#include <vector>

namespace compiler {

struct FunctionCode {
    std::uint32_t label;
    OutputCodeStream cs;

    FunctionCode(std::uint32_t label_arg, OutputCodeStream ocs_arg);
};

template <typename T>
struct simple_stack {
    const std::size_t size() const noexcept {
        return buf.size();
    }

    const T& top() const noexcept {
        return buf.back();
    }

    T& top() noexcept {
        return buf.back();
    }

    void push(T ele) {
        buf.push_back(std::move(ele));
    }

    template <typename... Args>
    void emplace(Args&&... args) {
        buf.emplace_back(std::forward<Args>(args)...);
    }

    void pop() {
        buf.pop_back();
    }

    bool empty() const noexcept {
        return buf.empty();
    }

private:
    std::vector<T> buf;
};

enum class BuiltinOperation { ADD, SUB, };
struct eval_type { };
using lambda_function_type = std::uint32_t;  // label
using symbol_type = std::add_pointer_t<const SymbolNode>;
using operation_type = std::variant<BuiltinOperation, lambda_function_type, symbol_type, eval_type>;

struct SemanticAnalyzer : public ASTNodeVisitor {
    SemanticAnalyzer();

    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;
    virtual void visit(const SequenceNode* const node) override;

    std::vector<FunctionCode> analyze(const SequenceNode* const nodes);
    
private:
    SymbolTable table;
    std::uint32_t cur_nest;
    OutputCodeStream cur_code;
    std::vector<FunctionCode> fcodes;
    std::vector<OutputCodeStream> code_buf;
    simple_stack<SimpleRegisterAllocator> sra_stk;

    OutputCodeStream callee_prolog();
    OutputCodeStream callee_epilog();
};

struct SimpleInstructionChecker : public ASTNodeVisitor {
    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;
    virtual void visit(const SequenceNode* const node) override;
    std::optional<operation_type> res = std::optional<operation_type>(std::nullopt);
};

}

#endif
