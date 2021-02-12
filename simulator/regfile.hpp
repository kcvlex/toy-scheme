#ifndef INCLUDE_SIMULATOR_REGFILE
#define INCLUDE_SIMULATOR_REGFILE

#include "alias.hpp"

namespace simulator {

struct Regfile {
    using value_type = data_opt_type;

    Regfile();

    void write(const reg_opt_type reg_opt, const value_type dat);
    value_type read(const reg_opt_type reg_opt) const;

private:
    std::array<value_type, 32> regs;
};

}

#endif
