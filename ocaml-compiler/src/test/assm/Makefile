FPGA_BIN = rv32.o
SIM_BIN = rv64.o
LINKER_SCRIPT = link.ld

${FPGA_BIN} : asm32.s ${LINKER_SCRIPT}
	riscv32-elf-as $< -o tmp.o
	riscv32-elf-ld tmp.o -o $@ -T ${LINKER_SCRIPT}

${SIM_BIN} : asm64.s
	riscv64-elf-as $< -fpic -o tmp.o
	riscv64-elf-gcc tmp.o -o $@ -mcmodel=medany

.PHONY : simulate
simulate : ${SIM_BIN}
	spike /riscv64-elf/bin/pk $<

.PHONY : fpga
fpga : ${FPGA_BIN} parser.py
	riscv32-elf-objdump -x -h -s -Mno-aliases $< | python parser.py > top.v

.PHONY : uart
uart: ${FPGA_BIN} parser.py uart.py
	riscv32-elf-objdump -x -h -s -Mno-aliases $< | sudo python uart.py

.PHONY : dump32
dump32 : ${FPGA_BIN}
	riscv32-elf-objdump -d -x -h -Mno-aliases $< | less

.PHONY : clean
clean :
	rm -f ${FPGA_BIN} ${SIM_BIN} tmp.o
