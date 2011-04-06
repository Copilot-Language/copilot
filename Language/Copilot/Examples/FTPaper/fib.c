#include <stdio.h>
#include <stdint.h>

uint64_t x = 0;
uint64_t y = 1;

void fib(void) {
  uint64_t tmp;
  tmp = x;
  x = y;
  y = tmp + y;
  //  printf("val: %llu\n", x);
}

int main(void) {
  uint64_t i = 100000000LLU;
  for (; i > 0; i--) { 
    fib();
  }
}


