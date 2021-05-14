OUTPUT_ARCH("riscv");
ENTRY(main);

SECTIONS {
  . = 0xffff + 0x10;
  .text : {
    *(.text)
  }
  .rodata : {
    *(.rodata)
  }
  .data : {
    *(.data)
  }

  .bss : {
    *(.bss)
    *(COMMON)
  }
}
