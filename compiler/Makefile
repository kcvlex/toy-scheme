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

clean:
	rm -f compiler.a test_translate *o *d

ifneq (${MAKECMDGOALS},clean)
include ${DEPS}
endif

%.d: %.cpp
	${CC} -MM ${CXX_FLAGS} $< | sed -e "s/\(.*.o\):/\1 $@:/g" > $@

%.o: %.d %.cpp
	${CXX} ${CXX_FLAGS} -c $(shell echo $< | sed -e "s/\(.*\).d/\1.cpp/g") -o $@

define TEST_RULES =
$(1).obj.d: $(1).cpp $(1).d 
	$$(eval DEPS_HPP=$$(shell cat $(1).d | sed -e "s/^.*://g" | tr '\\' ' ' | tr ' ' '\n' | grep -E "*hpp"))
	$$(eval TO_CPP=$$(shell echo $${DEPS_HPP} | tr ' ' '\n' | sed -e "s/\(.*\)\..*/\1.cpp/g"))
	$$(eval DEPS_CPP=$$(filter $${SOURCES},$${TO_CPP}))
	$$(eval DEPS_OBJ=$$(shell echo $${DEPS_CPP} | tr ' ' '\n' | sed -e "s/\(.*\)\..*/\1.o/g" | grep -v test | sort | uniq))
	$$(eval TARGET_AND_DEPS=$$(shell echo $${DEPS_OBJ} | sed -e "s/^/$(1): /g"))
	echo $${TARGET_AND_DEPS} > $$@

ifneq ($${MAKECMDGOALS},clean)
include $(1).obj.d
endif

$(1): $(1).cpp $(1).d $(1).obj.d
	$${CXX} $${CXX_FLAGS} $$< $$(shell cat $(1).obj.d | sed -e "s/^.*: //g") -o $$@
endef

$(eval $(call TEST_RULES,test_translate))
