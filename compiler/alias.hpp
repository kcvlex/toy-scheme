#ifndef INCLUDE_COMPILER_ALIAS
#define INCLUDE_COMPILER_ALIAS

#include <string>
#include <vector>

namespace compiler {

using ref_type = std::string;
using refs_seq_type = std::vector<std::string>;
using refs_seq_ptr_type = refs_seq_type*;
using addr_type = std::uint32_t;
using imm_value_type = std::int16_t;
using label_type = std::string;  // FIXME

}

#endif
