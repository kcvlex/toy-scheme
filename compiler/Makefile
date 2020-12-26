CXX = g++
CXX_FLAGS = -Wall -g -std=c++17
HEADERS = $(shell ls *hpp)

all: main

main: main.o token.o parser.o ast.o three_address_code.o sem_analyzer.o
	${CXX} ${CXX_FLAGS} $^ -o $@

main.o: main.cpp ${HEADERS} 
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

enum2str-test: enum2str.hpp enum2str_test.cpp
	${CXX} ${CXX_FLAGS} $^ -o $@
	./$@

clean:
	rm *.o
