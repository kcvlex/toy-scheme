CXX = g++
ROOT_DIR = $(shell pwd)/..
UTIL_DIR = ${ROOT_DIR}/util
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}
HEADERS = $(shell ls *hpp)

all: main

main: main.o token.o parser.o ast.o three_address_code.o sem_analyzer.o
	${CXX} ${CXX_FLAGS} $^ -o $@

main.o: main.cpp ${HEADERS} ${UTIL_DIR}/enum2str.hpp
	${CXX} ${CXX_FLAGS} -c $<

sem_analyzer.o: sem_analyzer.cpp sem_analyzer.hpp ast.hpp three_address_code.hpp
	${CXX} ${CXX_FLAGS} -c $< 

three_address_code.o: three_address_code.cpp three_address_code.hpp 
	${CXX} ${CXX_FLAGS} -c $< 

token.o: token.cpp token.hpp
	${CXX} ${CXX_FLAGS} -c $< 

parser.o: parser.cpp parser.hpp token.hpp ast.hpp
	${CXX} ${CXX_FLAGS} -c $< 

ast.o: ast.cpp ast.hpp
	${CXX} ${CXX_FLAGS} -c $< 

clean:
	rm *.o
