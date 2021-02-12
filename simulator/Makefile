CXX = g++
CC = cc
ROOT_DIR = $(shell pwd)/..
ALL_SOURCES = $(wildcard *cpp)
TEST_SOURCES = $(wildcard *test*)
SOURCES = $(filter-out ${TEST_SOURCES},${ALL_SOURCES})
OBJECTS = $(patsubst %.cpp,%.o,${SOURCES})
DEPS = $(patsubst %.cpp,%.d,${ALL_SOURCES})
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}

prog: ${OBJECTS} ${DEPS}
	${MAKE} -C ../util
	${CXX} ${CXX_FLAGS} ${OBJECTS} ../util/util.a -o prog

clean:
	rm -f prog *o *d

ifneq (${MAKECMDGOALS},clean)
include ${DEPS}
endif

%.d: %.cpp
	${CC} -MM ${CXX_FLAGS} $< | sed -e "s/\(.*.o\):/\1 $@:/g" > $@

%.o: %.d %.cpp
	${CXX} ${CXX_FLAGS} -c $(shell echo $< | sed -e "s/\(.*\).d/\1.cpp/g") -o $@
