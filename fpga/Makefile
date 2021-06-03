FILES=$(shell ls *\.v | grep -v DE10 | grep -v SEG | grep -v program)

simulate: simulate.o
	./$<

simulate.o: ${FILES} simple_sdram.v
	iverilog $^ -o simulate.o

clean:
	rm -f simulate.o
