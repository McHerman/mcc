#ifndef TEST_H
#define TEST_H

typedef unsigned char      uint8_t;
typedef unsigned short     uint16_t;
typedef unsigned int       uint32_t;

extern volatile int tohost;
extern volatile int fromhost;

static inline void pass(void) {
    tohost = 1;
    while (1);
}

static inline void fail(int test_num) {
    tohost = (test_num << 1) | 1;
    while (1);
}

#define ASSERT(cond, test_num) do { if (!(cond)) fail(test_num); } while (0)

#endif
