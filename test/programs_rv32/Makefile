CC      = clang
OBJCOPY = llvm-objcopy

CFLAGS        = --target=riscv32 -march=rv32im -mabi=ilp32 -mcmodel=medany -nostdlib -nostartfiles -fuse-ld=lld
CFLAGS_ATOMIC = --target=riscv32 -march=rv32ia_zabha -mabi=ilp32 -mcmodel=medany -nostdlib -nostartfiles -fuse-ld=lld
LDSCRIPT = link.ld
BIN2HEX  = python3 bin2hex.py
CRT0     = crt0.S

ASM_TESTS    = test_add
C_TESTS      = test_c_example
ATOMIC_TESTS = test_zaamo test_sem

TESTS = $(ASM_TESTS) $(C_TESTS) $(ATOMIC_TESTS)

all: $(addsuffix .memhex, $(TESTS))

$(addsuffix .elf, $(ASM_TESTS)): %.elf: %.S $(LDSCRIPT)
	$(CC) $(CFLAGS) -T $(LDSCRIPT) -o $@ $<

$(addsuffix .elf, $(C_TESTS)): %.elf: %.c $(CRT0) $(LDSCRIPT) test.h
	$(CC) $(CFLAGS) -T $(LDSCRIPT) -o $@ $(CRT0) $<

$(addsuffix .elf, $(ATOMIC_TESTS)): %.elf: %.c $(CRT0) $(LDSCRIPT) test.h
	$(CC) $(CFLAGS_ATOMIC) -T $(LDSCRIPT) -o $@ $(CRT0) $<

%.memhex: %.elf
	$(OBJCOPY) -O binary $< $@.bin
	$(BIN2HEX) $@.bin $@
	@rm -f $@.bin

%.dump: %.elf
	llvm-objdump -d $< > $@

clean:
	rm -f *.elf *.memhex *.dump *.bin

.PHONY: all clean
