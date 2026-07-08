// Semaphore round-trip test.
//
// Harness maps semaphore 0 at mcc address 0x80004000:
//   0x80004000 (word 0) = full  register
//   0x80004004 (word 1) = empty register
//
// Harness initialises full=64, empty=0 before mcc starts executing.
// Test sequence:
//   1. Spin-wait until full >= 64 (software poll via lw / TileLink Get)
//   2. amoadd.w: subtract 64 from full  (data trick: 0x80000040)
//   3. amoadd.w: add    64 to   empty   (data: 64)
//   4. Verify full==0, empty==64

#include "test.h"

#define SEM_FULL  ((volatile uint16_t *)0x80004000u)
#define SEM_EMPTY ((volatile uint16_t *)0x80004004u)

// Atomic add word, returns old value.
static inline uint32_t amoadd_w(volatile uint32_t *addr, uint32_t val) {
    uint16_t old;
    __asm__ volatile ("amoadd.w %0, %2, 0(%1)"
                      : "=r"(old) : "r"(addr), "r"(val) : "memory");
    return old;
}

int main(void) {
    // 1. Spin-wait: poll full register until >= 64.
    //    The semaphore is initialised by the harness in the first few cycles,
    //    long before mcc starts; this loop should exit immediately.
    while (*SEM_FULL < 64u) {}

    // 2. Subtract 64 from full.
    //    Semaphore ADD rule: when data.asSInt < 0 → newVal = full − data.asUInt (mod 16-bit).
    //    data = 0x80000040: asSInt = −2147483584 < 0;
    //    64 − 0x80000040 = 0x80000000 (32-bit) → 0x0000 (16-bit truncation).
    uint16_t old = amoadd_w(SEM_FULL, 0x80000040u);

    ASSERT(old == 64u,  1);

    // 3. Add 64 to empty.
    //    data = 64 = 0x40: asSInt = 64 ≥ 0 → newVal = empty + 64 = 64.
    uint16_t old2 = amoadd_w(SEM_EMPTY, 64u);

    ASSERT(old2 == 0u,  2);



    // 4. Verify.
    ASSERT(*SEM_FULL  == 0u,  3);
    ASSERT(*SEM_EMPTY == 64u, 4);

    pass();
    return 0;
}
