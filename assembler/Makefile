CXX = g++
CC = cc
ROOT_DIR = $(shell pwd)/..
SOURCES = $(wildcard *cpp)
OBJECTS = $(patsubst %.cpp,%.o,${SOURCES})
DEPS = $(patsubst %.cpp,%.d,${SOURCES})
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}

all: assembler.a

assembler.a: ${DEPS} ${OBJECTS}
	ar rcs $@  ${OBJECTS}

clean:
	rm -f assembler.a ${OBJECTS} ${DEPS}

ifneq (${MAKECMDGOALS},clean)
include ${DEPS}
endif

%.d: %.cpp
	${CC} -MM ${CXX_FLAGS} $< | sed -e "s/\(.*.o\):/\1 $@:/g" > $@

%.o: %.d
	${CXX} ${CXX_FLAGS} -c $(shell echo $< | sed -e "s/\(.*\).d/\1.cpp/g") -o $@
