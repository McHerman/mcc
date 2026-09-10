# mcc

A 5-stage, single-issue RISC-V (RV32) core written in Chisel. Instruction and data memory are accessed synchronously over TileLink, and the core integrates with `ATAN`'s `MemTier` infrastructure (memory bus, scratchpads, hardware semaphores).

## Layout

- `src/main/scala/mcc/rv32_5stage/` — the 5-stage pipeline (fetch, decode, execute, memory, writeback, control/datapath)
- `src/main/scala/mcc/common/` — shared constants, instruction encodings, CSR events, debug interfaces
- `src/main/scala/mcc/ImemTL.scala` — synchronous, TileLink-backed instruction memory
- `src/main/scala/mcc/MccTestHarness.scala` — test harness (hex memory loading, wiring for simulation)
- `test/programs_rv32/` — RISC-V/C test programs, linker script, and a `Makefile` that builds `.elf`/`.memhex` images
- `../ATAN/` — sibling module providing the memory-tier/bus infrastructure this core depends on

## Prerequisites

This project uses [Nix flakes](https://nixos.wiki/wiki/Flakes) to provide `mill`, `verilator`, `circt`, `just`, `gtkwave`, and an LLVM-based RISC-V toolchain:

```sh
nix develop
```

## Building & testing

Common tasks are wrapped in the `justfile`:

```sh
just build-programs-rv32   # compile test/programs_rv32 into .memhex images
just test <name>           # build programs, then run mcc.MccProgramTest against <name>.memhex
just test-trace <name>     # same, with VCD waveform output
just test-all              # run every program in test/programs_rv32
just test-llvm             # run the LLVM-generated program test
just elab                  # elaboration-only sanity test
```

Run `just` (or `just --list`) to see all available recipes.
