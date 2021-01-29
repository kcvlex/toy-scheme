#include "lexical_scope.hpp"
#include "util.hpp"
#include <algorithm>
#include <iostream>

namespace compiler {

namespace {

int clsr_record_counter = 0;

std::string get_clsr_record_name() {
    return std::string("c__") + std::to_string(clsr_record_counter++);
}

}  // anonymouse



/******************** VarLocation ********************/

VarLocation::VarLocation(const RefTypeTag type_arg,
                         const std::size_t idx_arg,
                         std::optional<std::string> record_name_arg)
    : type(type_arg),
      idx(idx_arg),
      record_name(std::move(record_name_arg))
{
}

VarLocation::VarLocation(const RefTypeTag type_arg,
                         const std::size_t idx_arg)
    : VarLocation(type_arg, idx_arg, std::nullopt)
{
}

VarLocation::VarLocation()
    : VarLocation(RefTypeTag::Unknown, 0, std::nullopt)
{
}


/******************** ClosureRecord ********************/

ClosureRecord::ClosureRecord(std::string name_arg,
                             const raw_record_ptr_type record_arg)
    : name(std::move(name_arg)),
      record(record_arg)
{
}

const std::string& ClosureRecord::get_name() const noexcept {
    return name;
}

index_opt_type ClosureRecord::get_idx_opt(const std::string &name) const noexcept {
    const auto cb = std::cbegin(*record), ce = std::cend(*record);
    const auto ite = std::lower_bound(cb, ce, name);
    if (ite == ce) return index_opt_type(std::nullopt);
    return index_opt_type(std::distance(cb, ite));
}

std::size_t ClosureRecord::size() const noexcept {
    return record->size();
}

ClosureRecord::const_raw_record_ptr_type ClosureRecord::get_raw_record() const {
    return record;
}

ClosureRecord ClosureRecord::get_empty_clsr_record() {
    return ClosureRecord(std::move(get_clsr_record_name()),
                         std::make_shared<refs_seq_type>());
}


/******************** ClosureRecordFactory ********************/

ClosureRecordFactory::ClosureRecordFactory(refs_seq_type refs)
    : record(std::make_shared<refs_seq_type>(std::move(refs)))
{
}

ClosureRecord ClosureRecordFactory::produce() const noexcept {
    return ClosureRecord(std::move(get_clsr_record_name()), record);
}

ClosureRecord::const_raw_record_ptr_type
ClosureRecordFactory::get_raw_record() const noexcept {
    return record;
}


/******************** ClosureRecordFactoryBuilder ********************/

ClosureRecordFactoryBuilder::ref_self_type
ClosureRecordFactoryBuilder::append(const refs_seq_type &refs) {
    ext_refs_sum.insert(std::end(ext_refs_sum), std::cbegin(refs), std::cend(refs));
    return *this;
}

ClosureRecordFactoryBuilder::ref_self_type
ClosureRecordFactoryBuilder::append(const std::string &name) {
    ext_refs_sum.push_back(name);
    return *this;
}

ClosureRecordFactory ClosureRecordFactoryBuilder::build() {
    std::sort(std::begin(ext_refs_sum), std::end(ext_refs_sum));
    const auto ite = std::unique(std::begin(ext_refs_sum), std::end(ext_refs_sum));
    ext_refs_sum.erase(ite, std::end(ext_refs_sum));
    return ClosureRecordFactory(std::move(ext_refs_sum));
}


/******************** Lexical Scope ********************/

LexicalScope::LexicalScope(const refs_seq_ptr_type args_arg,
                           const refs_seq_ptr_type locals_arg,
                           const refs_seq_ptr_type ext_refs_arg,
                           const ClosureRecord clsr_record_arg)
    : args(args_arg),
      locals(locals_arg),
      ext_refs(ext_refs_arg),
      clsr_record(clsr_record_arg)
{
}

VarLocation LexicalScope::get_ref(const std::string &name) const noexcept {
    {
        const auto opt = get_idx_aux(*locals, name);
        if (opt.has_value()) return VarLocation(RefTypeTag::Local, *opt);
    }

    {
        const auto opt = get_idx_aux(*args, name);
        if (opt.has_value()) return VarLocation(RefTypeTag::Args, *opt);
    }

    {
        const auto opt = clsr_record.get_idx_opt(name);
        if (opt.has_value()) return VarLocation(RefTypeTag::External, *opt, clsr_record.get_name());
    }

    {
        if (name == "ADD") return VarLocation(RefTypeTag::Global, 0);  // FIXME
    }

    return VarLocation(RefTypeTag::Unknown, 0);
}

const ClosureRecord& LexicalScope::get_closure_record() const noexcept {
    return clsr_record;
}

}
