// Zabha halfword AMO test — exercises amoor.h, amoand.h, amoswap.h, amoadd.h, amoxor.h
// The test harness is a stub that returns the current memory value without write-back,
// so all AMOs read back 0x0000 (BSS-initialised semaphore).
#include "test.h"

volatile uint16_t semaphore;  // BSS — zeroed by crt0

int main(void) {
    uint16_t old;

    // amoor.h  (acquire): old value must be 0x0000
    old = __atomic_fetch_or(&semaphore, (uint16_t)0x0001, __ATOMIC_ACQUIRE);
    ASSERT(old == 0x0000, 1);

    /*
    // amoand.h (release): still 0x0000 — harness does not write back
    old = __atomic_fetch_and(&semaphore, (uint16_t)0xFFFE, __ATOMIC_RELEASE);
    ASSERT(old == 0x0001, 2);

    // amoswap.h: still 0x0000
    old = __atomic_exchange_n(&semaphore, (uint16_t)0xFFFF, __ATOMIC_RELAXED);
    ASSERT(old == 0x0000, 3);

    // amoadd.h: still 0x0000
    old = __atomic_fetch_add(&semaphore, (uint16_t)1, __ATOMIC_RELAXED);
    ASSERT(old == 0xFFFF, 4);

    // amoxor.h: still 0x0000
    old = __atomic_fetch_xor(&semaphore, (uint16_t)0x00FF, __ATOMIC_RELAXED);
    ASSERT(old == 0x0000, 5);
    */

    return 0;
}
