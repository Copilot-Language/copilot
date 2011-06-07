#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

uint64_t x0 = 1;
uint64_t x1 = 2;
uint64_t x2 = 1;
uint64_t x3 = 2;
uint64_t x4 = 1;


void x0up(void) {
  x0 = x0++;
}

void x1up(void) {
  x1 = x1++;
}

void x2up(void) {
  x2 = x2++;
}

void x3up(void) {
  x3 = x3++;
}

void x4up(void) {
  x4 = x4++;
}

uint64_t maj(void) {
  uint64_t arr[5];
  uint64_t candidate = 0;
  int cnt, loop;
  
  // populate temp array
  arr[0] = x0;
  arr[1] = x1;
  arr[2] = x2;
  arr[3] = x3;
  arr[4] = x4;
  
  for(loop = 0; loop < 5; loop++) {
    bool b_cand = candidate == arr[loop];
    bool b_cnt = cnt == 0;
    cnt = (b_cand || b_cnt) ? cnt + 1 : cnt;
    candidate = b_cnt ? arr[loop] : candidate;
  }
    /* if (candidate == arr[loop])  */
    /*   cnt++; */
    /* else if (cnt == 0) { */
    /*   candidate = arr[loop]; */
    /*   cnt++; */
    /* } */
    /* else cnt--; */

  return candidate;
}

int main(void) {
  uint64_t i;
  for(i = 0; i < 10000000LLU; i++) {
    printf("maj: %llu\n\n", maj());

    x0up();
    x1up();
    x2up();
    x3up();
    x4up();
  }
}
