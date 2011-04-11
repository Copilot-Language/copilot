#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>




static uint64_t __global_clock = 0;






struct {  /* copilotStatecopilot_fib */
  struct {  /* copilot_fib */
    uint64_t prophVal__f[3];
    uint64_t f;
    uint32_t updateIndex__f;
    uint32_t outputIndex__f;
  } copilot_fib;
} copilotStatecopilot_fib =
{  /* copilotStatecopilot_fib */
  {  /* copilot_fib */
    /* prophVal__f */
    { 0ULL
    , 1ULL
    , 0ULL
    },
    /* f */  0ULL,
    /* updateIndex__f */  2UL,
    /* outputIndex__f */  0UL
  }
};

/* copilot_fib.update__f */
static void __r0() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_fib.copilot_fib.outputIndex__f;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 3UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_fib.copilot_fib.prophVal__f[__5];
  uint32_t __7 = 1UL;
  uint32_t __8 = __7 + __2;
  uint32_t __9 = __8 % __4;
  uint64_t __10 = copilotStatecopilot_fib.copilot_fib.prophVal__f[__9];
  uint64_t __11 = __6 + __10;
  uint32_t __12 = copilotStatecopilot_fib.copilot_fib.updateIndex__f;
  if (__0) {
  }
  copilotStatecopilot_fib.copilot_fib.prophVal__f[__12] = __11;
}

/* copilot_fib.output__f */
static void __r1() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_fib.copilot_fib.outputIndex__f;
  uint64_t __2 = copilotStatecopilot_fib.copilot_fib.prophVal__f[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 3UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_fib.copilot_fib.f = __2;
  copilotStatecopilot_fib.copilot_fib.outputIndex__f = __6;
}

/* copilot_fib.incrUpdateIndex__f */
static void __r2() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_fib.copilot_fib.updateIndex__f;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 3UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_fib.copilot_fib.updateIndex__f = __5;
}



void copilot_fib() {


  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r0();  /* copilot_fib.update__f */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r1();  /* copilot_fib.output__f */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 4;
    if (__scheduling_clock == 0) {
      __r2();  /* copilot_fib.incrUpdateIndex__f */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}



void __copilot_fib(void) {
  int i;
  for(i = 0; i < 5; i++) {
    copilot_fib();
  }
}

int main(void) {
  uint64_t i = 10000000LLU;
  for (; i > 0; i--) { 
    __copilot_fib();
    printf("copilot_fib: %llu\n\n", copilotStatecopilot_fib.copilot_fib.f);
  }
}
