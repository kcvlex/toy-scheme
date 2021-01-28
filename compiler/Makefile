CXX = g++
CC = cc
ROOT_DIR = $(shell pwd)/..
ALL_SOURCES = $(wildcard *cpp)
TEST_SOURCES = $(wildcard *test*)
SOURCES = $(filter-out ${TEST_SOURCES},${ALL_SOURCES})
OBJECTS = $(patsubst %.cpp,%.o,${SOURCES})
DEPS = $(patsubst %.cpp,%.d,${ALL_SOURCES})
CXX_FLAGS = -Wall -g -std=c++17 -I${ROOT_DIR}

all: compiler.a

compiler.a: ${DEPS} ${OBJECTS}
	ar rcs $@  ${OBJECTS}

define TEST_RULES =
$(1): $(1).cpp $(1).d
	$$(eval DEPS_HPP=$$(shell cat $(1).d | sed -e "s/^.*://g" | tr '\\' ' ' | tr ' ' '\n' | grep -E "*hpp"))
	$$(eval TO_CPP=$$(shell echo $${DEPS_HPP} | tr ' ' '\n' | sed -e "s/\(.*\)\..*/\1.cpp/g"))
	$$(eval DEPS_CPP=$$(filter $${SOURCES},$${TO_CPP}))
	$$(eval DEPS_OBJ=$$(shell echo $${DEPS_CPP} | tr ' ' '\n' | sed -e "s/\(.*\)\..*/\1.o/g" | grep -v test | sort | uniq))
	echo $${DEPS_OBJ}
	for i in $${DEPS_OBJ}; do echo $$$${i}; make $$$${i}; done
	$${CXX} $${CXX_FLAGS} $$< $${DEPS_OBJ} -o $$@
endef

$(eval $(call TEST_RULES,test_translate))

clean:
	rm -f compiler.a test-translate *o *d

ifneq (${MAKECMDGOALS},clean)
include ${DEPS}
endif

%.d: %.cpp
	${CC} -MM ${CXX_FLAGS} $< | sed -e "s/\(.*.o\):/\1 $@:/g" > $@

%.o: %.d
	${CXX} ${CXX_FLAGS} -c $(shell echo $< | sed -e "s/\(.*\).d/\1.cpp/g") -o $@
