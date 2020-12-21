FILES=$(shell ls *\.v)

all: proc
	./proc

proc: ${FILES}
	iverilog ${FILES} -o proc
