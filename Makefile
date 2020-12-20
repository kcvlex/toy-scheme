CXX = g++
CXX_FLAGS = -Wall -g -std=c++17

prog: main.o token.o parser.o ast.o
	${CXX} ${CXX_FLAGS} $^ -o $@

main.o: main.cpp token.hpp
	${CXX} ${CXX_FLAGS} -c $<

token.o: token.cpp token.hpp
	${CXX} ${CXX_FLAGS} -c $< 

parser.o: parser.cpp parser.hpp token.hpp ast.hpp
	${CXX} ${CXX_FLAGS} -c $< 

ast.o: ast.cpp ast.hpp
	${CXX} ${CXX_FLAGS} -c $< 

clean:
	rm *.o *.gch
