CXX = g++
ROOT_DIR = $(shell pwd)
COMPILER_DIR = ${ROOT_DIR}/compiler
ASSEMBLER_DIR = ${ROOT_DIR}/assembler
UTIL_DIR = ${ROOT_DIR}/util
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}
HEADERS = $(shell ls ${COMPILER_DIR}/*.hpp ${ASSEMBLER_DIR}/*hpp ${UTIL_DIR}/*hpp)

all: 
	+$(MAKE) -C compiler
	+$(MAKE) -C assembler
	+$(MAKE) prog

prog: main.o ${COMPILER_DIR}/compiler.a ${ASSEMBLER_DIR}/assembler.a
	${CXX} ${CXX_FLAGS} $^ -o $@

main.o: main.cpp ${HEADERS} ${UTIL_DIR}/enum2str.hpp
	${CXX} ${CXX_FLAGS} $^ -c $<

clean:
	rm *.o ${COMPILER_DIR}/*.{o,a} ${ASSEMBLER_DIR}/*.{o,a} prog
