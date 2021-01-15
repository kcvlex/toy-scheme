#include "cps.hpp"
#include "util/enum2str.hpp"

namespace compiler {

namespace {

std::string get_cont_label() {
    static int cnt = 0;
    return std::string("k") + std::to_string(cnt++);
}

}  // anonymouse


void LambdaCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void PrimitiveCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void ApplyCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void VarCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void ConstantCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }

CPSNode::~CPSNode() {
}


/******************** Lambda CPS ********************/

LambdaCPS::LambdaCPS(std::vector<std::string> args_arg, node_ptr body_arg)
    : args(std::move(args_arg)),
      body(body_arg)
{
}

LambdaCPS::~LambdaCPS() {
    delete body;
}

const std::string& LambdaCPS::get_arg(const std::size_t i) const noexcept {
    return args[i];
}

std::size_t LambdaCPS::get_arg_num() const noexcept {
    return args.size();
}

CPSNode::const_node_ptr LambdaCPS::get_body() const noexcept {
    return body;
}


/******************** Primitive CPS ********************/

PrimitiveCPS::PrimitiveCPS(const Type type_arg)
    : type(type_arg)
{
}

PrimitiveCPS::~PrimitiveCPS() {
}

#define MAKE(STR, TYPE) \
    if (s == STR) return new PrimitiveCPS(PrimitiveCPS::Type::TYPE)
PrimitiveCPS* PrimitiveCPS::try_make(const std::string &s) {
    MAKE("+", ADD);
    MAKE("-", DEC);
    MAKE("and", AND);
    MAKE("or", OR);
    return nullptr;
}
#undef MAKE

const PrimitiveCPS::Type PrimitiveCPS::get_type() const noexcept {
    return type;
}


/******************** Apply CPS ********************/

ApplyCPS::ApplyCPS(node_ptr proc_arg, std::vector<node_ptr> args_arg)
    : proc(proc_arg),
      args(std::move(args_arg))
{
}

ApplyCPS::~ApplyCPS() {
    delete proc;
    for (auto arg : args) delete arg;
}

CPSNode::const_node_ptr ApplyCPS::get_proc() const noexcept {
    return proc;
}

CPSNode::const_node_ptr ApplyCPS::get_arg(const std::size_t i) const noexcept {
    return args[i];
}

std::size_t ApplyCPS::get_arg_num() const noexcept {
    return args.size();
}


/******************** Var CPS ********************/

VarCPS::VarCPS(std::string var_arg)
    : var(std::move(var_arg))
{
}

VarCPS::~VarCPS() {
}

const std::string& VarCPS::get_var() const noexcept {
    return var;
}


/******************** Constant CPS ********************/

ConstantCPS::ConstantCPS(const std::int32_t c_arg)
    : c(c_arg)
{
}

ConstantCPS::~ConstantCPS() {
}

std::int32_t ConstantCPS::get_value() const noexcept {
    return c;
}


/******************** CPS transformation ********************/

void AST2CPS::visit(const EvalNode* const node) {
    const auto k = get_cont_label();
    std::vector<CPSNode::node_ptr> args;
    CPSNode::node_ptr f_node = nullptr;

    auto gen_rec = [&] {
        auto f = [&](const std::size_t i, auto rec) -> void {
            if (i == node->get_children().size()) {
                const auto f_k = new ApplyCPS(f_node, { new VarCPS(k) });
                this->res = new ApplyCPS(f_k, args);
                return;
            }
            
            const auto k_i = get_cont_label();
            if (!f_node) {
                f_node = new VarCPS(k_i);
            } else {
                args.push_back(new VarCPS(k_i));
            }
            
            rec(i + 1, rec);
            AST2CPS visitor_i;
            node->get_children()[i]->accept(visitor_i);
            this->res = new ApplyCPS(visitor_i.res, { new LambdaCPS({ k_i }, this->res) });
        };
        f(0, f);
    };

    gen_rec();
    this->res = new LambdaCPS({ k }, this->res);
}

void AST2CPS::visit(const LambdaNode* const node) {
    const auto k1 = get_cont_label();
    const auto k2 = get_cont_label();
    std::vector<std::string> args;
    args.reserve(node->get_args().size());
    for (auto ptr : node->get_args()) args.push_back(ptr->get_symbol());

    node->get_body()->accept(*this);
    this->res = new ApplyCPS(this->res, { new VarCPS(k2) });
    this->res = new LambdaCPS(args, this->res);
    this->res = new LambdaCPS({ k2 }, this->res);
    this->res = new ApplyCPS(new VarCPS(k1), { this->res });
    this->res = new LambdaCPS({ k1 }, this->res);
}

void AST2CPS::visit(const SymbolNode* const node) {
    decltype(auto) symbol = node->get_symbol();
    std::string use = symbol;
    if (auto ptr = PrimitiveCPS::try_make(symbol)) {
        use = util::to_str<PrimitiveCPS::Type>(ptr->get_type());
    }
    const auto k = get_cont_label();
    this->res = new LambdaCPS({ k },
                              new ApplyCPS(new VarCPS(k), 
                                           { new VarCPS(use) }));
}

void AST2CPS::visit(const ConstantNode* const node) {
    const auto k = get_cont_label();
    this->res = new LambdaCPS({ k }, 
                              new ApplyCPS(new VarCPS(k), 
                                           { new ConstantCPS(node->get_value()) }));
}

void AST2CPS::visit(const SequenceNode* const node) {
    const auto k = get_cont_label();
  
    auto gen_rec = [&] {
        auto f = [&](const std::size_t i, auto rec) -> void {
            if (i == node->get_seq().size()) {
                this->res = new VarCPS(k);
                return;
            }
            const auto k_i = get_cont_label();
            rec(i + 1, rec);
            AST2CPS visitor_i;
            node->get_seq()[i]->accept(visitor_i);
            this->res = new LambdaCPS({ k_i }, new ApplyCPS(visitor_i.res, { this->res }));
        };
        f(0, f);
    };

    gen_rec();
    this->res = new LambdaCPS({ k }, this->res);
}


namespace internal {

struct PrintCPS : public CPSVisitor {
    PrintCPS() = delete;

    PrintCPS(const std::string &filename)
        : ofs(filename)
    {
        ofs << "(define ADD (lambda (k) (lambda args (k (apply + args)))))\n"
            << "(";
    }
    

    ~PrintCPS() {
        ofs << " (lambda (x) (display x)))\n";
    }

    virtual void visit(const LambdaCPS* const cps) override {
        ofs << "(lambda ";
        char elim = '(';
        for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
            ofs << std::exchange(elim, ' ') << cps->get_arg(i);
        }
        ofs << ") ";
        cps->get_body()->accept(*this);
        ofs << ")";
    }

    virtual void visit(const PrimitiveCPS* const cps) override {
        ofs << util::to_str<PrimitiveCPS::Type>(cps->get_type());
    }

    virtual void visit(const ApplyCPS* const cps) override {
        ofs << '(';
        cps->get_proc()->accept(*this);
        for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
            ofs << ' ';
            cps->get_arg(i)->accept(*this);
        }
        ofs << ')';
    }

    virtual void visit(const VarCPS* const cps) override {
        ofs << cps->get_var();
    }

    virtual void visit(const ConstantCPS* const cps) override {
        ofs << cps->get_value();
    }

private:
    std::ofstream ofs;
};

} // internal

void print_cps_code(const std::string &filename, const CPSNode* const cps) {
    internal::PrintCPS pc(filename);
    cps->accept(pc);
}

}
