#ifndef INCLUDE_COMPILER_DATA_TYPE
#define INCLUDE_COMPILER_DATA_TYPE

#include <variant>
#include <vector>
#include <cstdint>
#include "three_address_code.hpp"

namespace compiler {

using addr_type = std::uint32_t;

struct IntData;
struct ConsData;
struct NilData;
struct RefData;
struct ContinuationData;
struct EnvData;

using data_type = std::variant<IntData*, 
                               // ConsData*, 
                               NilData*, 
                               RefData*, 
                               ContinuationData*,
                               EnvData*>;

enum class DataTypeTag {
    Int = 0,
    Cons,
    Nil,
    Ref,
    Continuation,
    Env,
    Size
};

struct IntData {
    using data_type = std::uint32_t;
    data_type dat;

    IntData() = delete;
    IntData(const data_type dat_arg);
};

struct ConsData {
    data_type car, cdr;

    ConsData() = delete;
    ConsData(const data_type car_arg,
             const data_type cdr_arg);
    ~ConsData();
};

struct NilData {
};

struct RefData {
    addr_type addr;

    RefData() = delete;
    RefData(const addr_type addr_arg);
};

struct ContinuationData {
    label_type pc;
    addr_type env_base;

    ContinuationData() = delete;
    ContinuationData(const label_type pc_arg,
                     const addr_type env_base_arg);
};

struct EnvData {
    std::size_t data_cnt;
    addr_type sp, bp;
    std::vector<addr_type> refs;

    EnvData() = delete;
    EnvData(const std::size_t data_cnt_arg,
            const addr_type sp_arg,
            const addr_type bp_arg);
};

template <typename T, typename... Args>
data_type make_record(Args&&... args) {
    const T *dat = new T(std::forward<Args>(args)...);
    const data_type ret { std::in_place_type<T*>, dat };
    return ret;
}

}

#endif
