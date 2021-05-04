OUTPUT_ARCH("riscv");
ENTRY(main);

SECTIONS {
  . = 0x00000010;
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
