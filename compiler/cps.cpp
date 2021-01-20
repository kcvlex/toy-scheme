#include "cps.hpp"
#include "util/enum2str.hpp"
#include <cassert>

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
void BindCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void VarCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }
void ConstantCPS::accept(CPSVisitor &visitor) const { visitor.visit(this); }

void LambdaCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }
void PrimitiveCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }
void ApplyCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }
void BindCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }
void VarCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }
void ConstantCPS::accept(ModifyCPSVisitor &visitor) { visitor.visit(this); }

CPSNode::~CPSNode() {
}


/******************** Lambda CPS ********************/

LambdaCPS::LambdaCPS(std::vector<std::string> args_arg,
                     std::vector<bind_ptr> binds_arg,
                     node_ptr body_arg)
    : clsr(nullptr),
      args(std::move(args_arg)),
      binds(std::move(binds_arg)),
      body(body_arg)
{
}

LambdaCPS::LambdaCPS(std::vector<std::string> args_arg, 
                     node_ptr body_arg)
    : LambdaCPS(std::move(args_arg), { }, body_arg)
{
    if (clsr) delete clsr;
}

LambdaCPS::~LambdaCPS() {
    for (auto bind : binds) delete bind;
    delete body;
}

const std::string& LambdaCPS::get_arg(const std::size_t i) const {
    return args[i];
}

std::size_t LambdaCPS::get_arg_num() const noexcept {
    return args.size();
}

LambdaCPS::const_bind_ptr LambdaCPS::get_bind(const std::size_t i) const {
    return binds[i];
}

std::size_t LambdaCPS::get_bind_num() const noexcept {
    return binds.size();
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

CPSNode::node_ptr ApplyCPS::get_proc() noexcept {
    return proc;
}

CPSNode::const_node_ptr ApplyCPS::get_proc() const noexcept {
    return proc;
}

CPSNode::node_ptr ApplyCPS::get_arg(const std::size_t i) noexcept {
    return args[i];
}

CPSNode::const_node_ptr ApplyCPS::get_arg(const std::size_t i) const noexcept {
    return args[i];
}

std::size_t ApplyCPS::get_arg_num() const noexcept {
    return args.size();
}


/******************** Bind CPS ********************/

BindCPS::BindCPS(std::string name_arg, node_ptr value_arg)
    : name(std::move(name_arg)),
      value(value_arg)
{
    auto extract_value = new LambdaCPS({ "x" }, new VarCPS("x"));
    value = new ApplyCPS(value, { extract_value });
}

BindCPS::~BindCPS() {
    delete value;
}

const std::string& BindCPS::get_name() const noexcept {
    return name;
}

CPSNode::node_ptr BindCPS::get_value() noexcept {
    return value;
}

CPSNode::const_node_ptr BindCPS::get_value() const noexcept {
    return value;
}


/******************** Var CPS ********************/

VarCPS::VarCPS(std::string var_arg)
    : clsr_flag(false),
      var(std::move(var_arg)),
      clsr(nullptr),
      clsr_idx(-1)
{
}

VarCPS::~VarCPS() {
}

const std::string& VarCPS::get_var() const noexcept {
    return var;
}

void VarCPS::set_clsr(const clsr_ptr val) noexcept {
    clsr = val;
    clsr_idx = clsr->get_idx(var);
}

VarCPS::clsr_ptr VarCPS::get_clsr() const noexcept {
    return clsr;
}
    
ssize_t VarCPS::get_clsr_idx() const noexcept {
    return clsr_idx;
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

    // args
    std::vector<std::string> args;
    args.reserve(node->get_arg_num());
    for (std::size_t i = 0; i != node->get_arg_num(); i++) args.push_back(node->get_arg(i));

    // binds
    std::vector<LambdaCPS::bind_ptr> binds;
    if (node->get_body_num() != 0) {
        binds.reserve(node->get_body_num() - 1);
        for (std::size_t i = 0; i + 1 < node->get_body_num(); i++) {
            node->get_body(i)->accept(*this);
            auto casted = dynamic_cast<LambdaCPS::bind_ptr>(this->res);
            assert(casted);
            binds.push_back(casted);
        }
    }

    // F[[body]]
    node->get_body(node->get_body_num() - 1)->accept(*this);

    // (F[[body]] k2)
    this->res = new ApplyCPS(this->res, { new VarCPS(k2) });

    // (lambda args. binds (F[[body]] k2))
    this->res = new LambdaCPS(std::move(args), std::move(binds), this->res);

    /* 
     * (lambda k2.
     *   (lambda args. 
     *     binds
     *     (F[[body]] k2)))
     */
    this->res = new LambdaCPS({ k2 }, this->res);

    /* 
     * (k1
     *   (lambda k2.
     *     (lambda args. 
     *       binds
     *       (F[[body]] k2))))
     */
    this->res = new ApplyCPS(new VarCPS(k1), { this->res });
    
    /* 
     * (lambda k1. (k1
     *   (lambda k2.
     *     (lambda args. 
     *       binds
     *       (F[[body]] k2)))))
     */
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
            AST2CPS visitor_i;
            node->get_seq()[i]->accept(visitor_i);
            
            if (i + 1 == node->get_seq().size()) {
                // (k v)
                this->res = new ApplyCPS(new VarCPS(k), { new VarCPS("v") });
                
                // (lambda v. (k v))
                this->res = new LambdaCPS({ "v" }, this->res);

                // (F[[body_n]] (lambda v. (k v)))
                this->res = new ApplyCPS(visitor_i.res, { this->res });
            } else {
                rec(i + 1, rec);
                const std::string dummy = "dummy";
                
                // (lambda _. ...)
                this->res = new LambdaCPS({ dummy }, this->res);

                // (F[[body_i]] (lambda _. ...))
                this->res = new ApplyCPS(visitor_i.res, { this->res });
            }
        };

        f(0, f);
    };

    gen_rec();
    this->res = new LambdaCPS({ k }, this->res);
}

void AST2CPS::visit(const BindNode* const node) {
    node->get_value()->accept(*this);
    this->res = new BindCPS(node->get_name(), this->res);
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
        auto gen = [&] {
            ofs << "(lambda (";

            {
                for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
                    ofs <<  cps->get_arg(i) << ' ';
                }
            }

            ofs << ") ";

            {
                for (std::size_t i = 0; i != cps->get_bind_num(); i++) {
                    ofs << ' ';
                    cps->get_bind(i)->accept(*this);
                }
            }

            cps->get_body()->accept(*this);
            ofs << ")";
        };

        if (cps->clsr) {
            ofs << "((lambda ("
                << cps->clsr->get_name()
                << ") ";
            gen();
            ofs << ") (cons*";
            for (const auto &e : cps->clsr->get_refs()) ofs << ' ' << e;
            ofs << " ()))";
        } else {
            gen();
        }
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

    virtual void visit(const BindCPS* const cps) override {
        ofs << "(define " << cps->get_name() << ' ';
        cps->get_value()->accept(*this);
        ofs << ')';
    }

    virtual void visit(const VarCPS* const cps) override {
        if (!cps->clsr_flag) {
            ofs << cps->get_var();
        } else {
            ofs << "(list-ref " 
                << cps->get_clsr()->get_name()
                << ' '
                << cps->get_clsr_idx()
                << ')';
        }
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
