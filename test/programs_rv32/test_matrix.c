#include "test.h"

int main(void) {
  int h[] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
  int x[] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
  int lenY;
  float *y = convolve(h,x,5,5,&lenY);
  for(int i=0;i<lenY;i++) {
    printf("%0.f ",y[i]);
  }
  puts("");
  free(y);
  return 0;
}
