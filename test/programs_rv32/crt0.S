.section .text.init
.globl _start
_start:
    li      sp, 0x80000F00

    la      a0, __bss_start
    la      a1, __bss_end
1:
    bgeu    a0, a1, 2f
    sw      zero, 0(a0)
    addi    a0, a0, 4
    j       1b
2:
    call    main

    beqz    a0, .Lpass
    slli    a0, a0, 1
    ori     a0, a0, 1
    j       .Lwrite
.Lpass:
    li      a0, 1
.Lwrite:
    la      t0, tohost
    sw      a0, 0(t0)
.Lspin:
    j       .Lspin
