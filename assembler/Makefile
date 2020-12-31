CXX = g++
ROOT_DIR = $(shell pwd)/..
UTIL_DIR = ${ROOT_DIR}/util
COMPILER_DIR = ${ROOT_DIR}/compiler
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}

all: assembler.a

assembler.a: assembler.o bit_operation.o
	ar rcs $@ $^

assembler.o: assembler.cpp assembler.hpp bit_operation.hpp ${UTIL_DIR}/enum2str.hpp ${COMPILER_DIR}/sem_analyzer.hpp
	${CXX} ${CXX_FLAGS} -c $< -o $@

bit_operation.o: bit_operation.cpp bit_operation.hpp
	${CXX} ${CXX_FLAGS} -c $< -o $@

clean:
	rm *.{o,a,gch}
