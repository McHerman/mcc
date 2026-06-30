.section .text.init
.globl _start
_start:
    # Test 1: basic addition
    li      a0, 1
    li      a1, 2
    add     a2, a0, a1
    li      a3, 3
    bne     a2, a3, fail

    # Test 2: subtraction
    li      a0, 10
    li      a1, 7
    sub     a2, a0, a1
    bne     a2, a3, fail

    # Test 3: logical AND/OR/XOR
    li      a0, 0xFF
    li      a1, 0x0F
    and     a2, a0, a1
    li      a3, 0x0F
    bne     a2, a3, fail

    or      a2, a0, a1
    li      a3, 0xFF
    bne     a2, a3, fail

    xor     a2, a0, a1
    li      a3, 0xF0
    bne     a2, a3, fail

    # Test 4: shifts
    li      a0, 1
    slli    a2, a0, 4
    li      a3, 16
    bne     a2, a3, fail

    li      a0, 256
    srli    a2, a0, 4
    bne     a2, a3, fail

    # Test 5: store and load (32-bit)
    li      a0, 0xDEAD
    la      a1, scratch
    sw      a0, 0(a1)
    lw      a2, 0(a1)
    bne     a0, a2, fail

    # Test 6: branch loop
    li      a0, 5
    li      a1, 0
loop:
    addi    a1, a1, 1
    bne     a1, a0, loop
    li      a3, 5
    bne     a1, a3, fail

    j       pass

fail:
    la      a0, tohost
    li      a1, 3
    sw      a1, 0(a0)
    j       spin

pass:
    la      a0, tohost
    li      a1, 1
    sw      a1, 0(a0)

spin:
    j       spin

.section .data
.align 2
scratch:
    .word 0
