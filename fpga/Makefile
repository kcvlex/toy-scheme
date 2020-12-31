FILES=$(shell ls *\.v)

all: prog
	./prog

prog: ${FILES}
	iverilog ${FILES} -o prog
