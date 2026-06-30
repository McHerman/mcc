#include "test.h"

int main(void) {
    int a = 3, b = 4;
    ASSERT(a + b == 7, 1);

    int arr[4] = {10, 20, 30, 40};
    int sum = 0;
    for (int i = 0; i < 4; i++) sum += arr[i];
    ASSERT(sum == 100, 2);

    return 0;
}
