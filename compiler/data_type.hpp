#ifndef INCLUDE_COMPILER_DATA_TYPE
#define INCLUDE_COMPILER_DATA_TYPE

#include <variant>
#include <vector>
#include <cstdint>
#include "alias.hpp"
#include "three_address_code.hpp"

namespace compiler {

struct IntData;
struct ConsData;
struct NilData;
struct RefData;
struct ContinuationData;
struct EnvData;

using data_type = std::variant<IntData*, 
                               ConsData*, 
                               NilData*, 
                               RefData*, 
                               ContinuationData*,
                               EnvData*>;

enum class DataTypeTag {
    Unknown = 0,
    Int,
    Cons,
    Nil,
    Ref,
    Continuation,
    Env,
    Size
};

template <typename T>
struct IDataType {
    constexpr static DAtaTypeTag tag = T::tag;

    CodeSequence gen_code() const {
        return static_cast<T*>(this)->gen_code();
    }
};

struct IntData : public IDataType<IntData> {
    using data_type = std::uint32_t;

    constexpr static DataTypeTag tag = DataTypeTag::Int;
    data_type dat;

    IntData() = delete;
    IntData(const data_type dat_arg);

    CodeSequence gen_code() const;
};

struct ConsData : public IDataType<ConsData> {
    constexpr static DataTypeTag tag = DataTypeTag::Cons;
    addr_type *car;
    addr_type *cdr;

    ConsData() = delete;
    ConsData(data_type* const car_arg,
             data_type* const cdr_arg);
    ~ConsData();
    
    CodeSequence gen_code() const;
};

struct NilData {
    constexpr static DataTypeTag tag = DataTypeTag::Nil;
};

struct RefData {
    constexpr static DataTypeTag tag = DataTypeTag::Ref;
    addr_type addr;

    RefData() = delete;
    RefData(const addr_type addr_arg);
};

struct ContinuationData {
    constexpr static DataTypeTag tag = DataTypeTag::Continuation;
    label_type pc;
    addr_type env_base;

    ContinuationData() = delete;
    ContinuationData(const label_type pc_arg,
                     const addr_type env_base_arg);
};

struct EnvData {
    constexpr static DataTypeTag tag = DataTypeTag::Env;
    addr_type sp, bp;

    EnvData() = delete;
    EnvData(const addr_type sp_arg,
            const addr_type bp_arg);
};

constexpr DataTypeTag get_data_type(const data_type &d) {
    return std::visit([&](const data_type &arg) { return arg::tag; }, d);
}

}

#endif
