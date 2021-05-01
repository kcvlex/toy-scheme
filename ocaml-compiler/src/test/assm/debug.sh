#!/bin/zsh
set -eux
riscv64-elf-as asm.s -fpic
riscv64-elf-gcc a.out -o a.exe -mcmodel=medany
spike /riscv64-elf/bin/pk a.exe
