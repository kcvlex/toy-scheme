CXX = g++
CXX_FLAGS = -Wall -g -std=c++17

all: main

main: main.cpp assembler.o bit_operation.o
	${CXX} ${CXX_FLAGS} $^ -o $@

assembler.o: assembler.cpp assembler.hpp bit_operation.hpp enum2str.hpp
	${CXX} ${CXX_FLAGS} -c $<

bit_operation.o: bit_operation.cpp bit_operation.hpp
	${CXX} ${CXX_FLAGS} -c $<

clean:
	rm *.o
