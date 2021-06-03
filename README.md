# toy-scheme

## Compile

```
$ cd ocaml-compiler/src
$ dune build
$ cd ../_build/default/src/bin
$ ./main.exe [source file] -o [output name] -arch [RV32I | RV64I] -mode [FPGA | Simulation]
```

## Simulation

Use [Spike](https://github.com/riscv/riscv-isa-sim)

## Send Program

```
$ cd ocaml-compiler/src/test/assm
$ cp [input file] asm32.s
$ make uart
```
