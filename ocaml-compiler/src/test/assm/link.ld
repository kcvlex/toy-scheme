OUTPUT_ARCH("riscv");
ENTRY(main);

SECTIONS {
  . = 0x00000010;
  .text : {
    *(.text)
  }

  . = 0xff00 * 0x4;
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
