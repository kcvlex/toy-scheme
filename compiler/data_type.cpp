#include "data_type.hpp"

namespace compiler {

/******************** IntData ********************/

IntData::IntData(const data_type dat_arg)
    : dat(dat_arg)
{
}


ConsData::ConsData(addr_type* const car_arg,
                   addr_type* const cdr_arg)
    : car(car_arg),
      cdr(cdr_arg)
{
}

ConsData::~ConsData() {
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

EnvData::EnvData(const addr_type sp_arg,
                 const addr_type bp_arg)
    : sp(sp_arg),
      bp(bp_arg),
{
}


}
