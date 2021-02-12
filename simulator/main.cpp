#include "simulator.hpp"

int main(int argc, const char *argv[]) {
    simulator::Simulator sim(argv[1]);
    sim.simulate();
    return 0;
}
