#ifndef INCLUDE_COMPILER_LEXICAL_SCOPE
#define INCLUDE_COMPILER_LEXICAL_SCOPE

#include <vector>
#include <set>
#include <string>
#include <memory>
#include "util.hpp"
#include "alias.hpp"

namespace compiler {

enum class RefTypeTag {
    Unknown,
    Args,
    Local,
    External,
    Global,
};

struct VarLocation {
    RefTypeTag type;
    std::size_t idx;
    std::optional<std::string> record_name;

    VarLocation(const RefTypeTag type_arg,
                const std::size_t idx_arg,
                std::optional<std::string> record_name_arg);
    VarLocation(const RefTypeTag type_arg,
                const std::size_t idx_arg);
    VarLocation();
};

struct ClosureRecord {
    using raw_record_ptr_type = std::shared_ptr<refs_seq_type>;
    using const_raw_record_ptr_type = std::shared_ptr<const refs_seq_type>;

    ClosureRecord() = delete;

    const std::string& get_name() const noexcept;
    index_opt_type get_idx_opt(const std::string &name) const noexcept;
    std::size_t size() const noexcept;
    const_raw_record_ptr_type get_raw_record() const;
    static ClosureRecord get_empty_clsr_record();

private:
    std::string name;
    raw_record_ptr_type record;

    ClosureRecord(std::string name_arg,
                  const raw_record_ptr_type record_arg);

    friend struct ClosureRecordFactory;
};

struct ClosureRecordFactory {
    ClosureRecordFactory() = delete;

    ClosureRecord produce() const noexcept;
    ClosureRecord::const_raw_record_ptr_type get_raw_record() const noexcept;

private:
    ClosureRecord::raw_record_ptr_type record;

    ClosureRecordFactory(refs_seq_type ext_refs);

    friend struct ClosureRecordFactoryBuilder;
};

struct ClosureRecordFactoryBuilder {
    using ref_self_type = ClosureRecordFactoryBuilder&;
    ref_self_type append(const refs_seq_type &ext_refs);
    ref_self_type append(const std::string &name);
    ClosureRecordFactory build();

private:
    refs_seq_type ext_refs_sum;
};
    
struct LexicalScope {
    LexicalScope(const refs_seq_ptr_type args_arg,
                 const refs_seq_ptr_type locals_arg,
                 const refs_seq_ptr_type ext_refs_arg,
                 const ClosureRecord clsr_record_arg);

    VarLocation get_ref(const std::string &name) const noexcept;
    const ClosureRecord& get_closure_record() const noexcept;

private:
    refs_seq_ptr_type args, locals, ext_refs;
    ClosureRecord clsr_record;
};

}

#endif
