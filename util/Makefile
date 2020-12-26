CXX = g++
ROOT_DIR = $(shell pwd)/..
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}

all: main

main: enum2str_test.cpp enum2str.hpp
	${CXX} ${CXX_FLAGS} $^ -o $@

clean:
	rm *.o
	rm main
