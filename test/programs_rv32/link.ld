OUTPUT_ARCH("riscv")
ENTRY(_start)

SECTIONS {
    . = 0x80000000;

    .text : {
        *(.text.init)
        *(.text*)
    }

    .rodata : { *(.rodata*) }

    .data : {
        *(.data*)
        *(.sdata*)
    }

    .bss : {
        __bss_start = .;
        *(.bss*)
        *(.sbss*)
        __bss_end = .;
    }

    . = 0x80001000;
    .tohost : {
        tohost   = .;
        . += 4;
        fromhost = .;
        . += 4;
    }
}
