#include "data_type.hpp"

namespace compiler {

/******************** IntData ********************/

IntData::IntData(const data_type dat_arg)
    : dat(dat_arg)
{
}


/******************** ConsData ********************/

ConsData::ConsData(const record_data_type car_arg,
                   const record_data_type cdr_arg)
    : car(car_arg),
      cdr(cdr_arg)
{
}

ConsData::~ConsData() {
    const auto free = [&](auto ptr) {
        if (ptr) delete ptr;
    };
    std::visit(free, car);
    std::visit(free, cdr);
}


/******************** NilData ********************/


/******************** RefData ********************/

RefData::RefData(const addr_type addr_arg)
    : addr(addr_arg)
{
}


/******************** ContinuationData ********************/

ContinuationData::ContinuationData(const label_type pc_arg,
                                   const addr_type env_base_arg)
    : pc(pc_arg),
      env_base(env_base_arg)
{
}


/******************** EnvData ********************/

EnvData::EnvData(const std::size_t data_cnt_arg,
                 const addr_type sp_arg,
                 const addr_type bp_arg)
    : data_cnt(data_cnt_arg),
      sp(sp_arg),
      bp(bp_arg),
      refs(data_cnt)
{
}


}
