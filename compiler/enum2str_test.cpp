#include "enum2str.hpp"
#include <iostream>

enum class E {
    V1 = 0,
    V2,
    V3,
};

int main() {
    std::cout << util::to_str(E::V1) << std::endl;
    return 0;
}
