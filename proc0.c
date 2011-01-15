#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "proc0.h"


static uint64_t __global_clock = 0;

void send0(uint16_t val, int port) {
  switch (port) {
  case 1: p01 = val;
  case 2: p02 = val;
  }
}

struct {  /* copilotStateproc0 */
  struct {  /* proc0 */
    uint16_t prophVal__p0[2];
    bool chk;
    uint16_t p0;
    uint16_t maj;
    uint64_t updateIndex__p0;
    uint64_t outputIndex__p0;
    uint16_t tmpSampleVal__p01;
    uint16_t tmpSampleVal__p02;
  } proc0;
} copilotStateproc0 =
{  /* copilotStateproc0 */
  {  /* proc0 */
    /* prophVal__p0 */
    { 0
    , 0
    },
    /* chk */  false,
    /* p0 */  0,
    /* maj */  0,
    /* updateIndex__p0 */  1ULL,
    /* outputIndex__p0 */  0ULL,
    /* tmpSampleVal__p01 */  0,
    /* tmpSampleVal__p02 */  0
  }
};

/* proc0.updateOutput__chk */
static void __r0() {
  bool __0 = true;
  uint32_t __1 = 3UL;
  uint16_t __2 = copilotStateproc0.proc0.tmpSampleVal__p02;
  uint16_t __3 = copilotStateproc0.proc0.tmpSampleVal__p01;
  uint64_t __4 = 0ULL;
  uint64_t __5 = copilotStateproc0.proc0.outputIndex__p0;
  uint64_t __6 = __4 + __5;
  uint64_t __7 = 2ULL;
  uint64_t __8 = __6 % __7;
  uint16_t __9 = copilotStateproc0.proc0.prophVal__p0[__8];
  bool __10 = __3 == __9;
  uint16_t __11 = __10 ? __9 : __2;
  bool __12 = __2 == __11;
  bool __13 = __3 == __11;
  bool __14 = __9 == __11;
  uint32_t __15 = 1UL;
  uint32_t __16 = 0UL;
  uint32_t __17 = __14 ? __15 : __16;
  uint32_t __18 = __17 + __15;
  uint32_t __19 = __13 ? __18 : __17;
  uint32_t __20 = __19 + __15;
  uint32_t __21 = __12 ? __20 : __19;
  uint32_t __22 = 2UL;
  uint32_t __23 = __21 * __22;
  bool __24 = __1 < __23;
  if (__0) {
  }
  copilotStateproc0.proc0.chk = __24;
}

/* proc0.update__p0 */
static void __r1() {
  bool __0 = true;
  uint64_t __1 = 0ULL;
  uint64_t __2 = copilotStateproc0.proc0.outputIndex__p0;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc0.proc0.prophVal__p0[__5];
  uint16_t __7 = 1;
  uint16_t __8 = __6 + __7;
  uint64_t __9 = copilotStateproc0.proc0.updateIndex__p0;
  if (__0) {
  }
  copilotStateproc0.proc0.prophVal__p0[__9] = __8;
}

/* proc0.updateOutput__maj */
static void __r4() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc0.proc0.tmpSampleVal__p01;
  uint64_t __2 = 0ULL;
  uint64_t __3 = copilotStateproc0.proc0.outputIndex__p0;
  uint64_t __4 = __2 + __3;
  uint64_t __5 = 2ULL;
  uint64_t __6 = __4 % __5;
  uint16_t __7 = copilotStateproc0.proc0.prophVal__p0[__6];
  bool __8 = __1 == __7;
  uint16_t __9 = copilotStateproc0.proc0.tmpSampleVal__p02;
  uint16_t __10 = __8 ? __7 : __9;
  if (__0) {
  }
  copilotStateproc0.proc0.maj = __10;
}

/* proc0.output__p0 */
static void __r2() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc0.proc0.outputIndex__p0;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc0.proc0.prophVal__p0[__1];
  if (__0) {
  }
  copilotStateproc0.proc0.outputIndex__p0 = __5;
  copilotStateproc0.proc0.p0 = __6;
}

/* proc0.__send_send0_port_2_var_p0 */
static void __r5() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc0.proc0.p0;
  if (__0) {
    send0(__1,2);
  }
}

/* proc0.__send_send0_port_1_var_p0 */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc0.proc0.p0;
  if (__0) {
    send0(__1,1);
  }
}

/* proc0.sample__p02 */
static void __r7() {
  bool __0 = true;
  uint16_t __1 = p02;
  if (__0) {
  }
  copilotStateproc0.proc0.tmpSampleVal__p02 = __1;
}

/* proc0.sample__p01 */
static void __r8() {
  bool __0 = true;
  uint16_t __1 = p01;
  if (__0) {
  }
  copilotStateproc0.proc0.tmpSampleVal__p01 = __1;
}

/* proc0.incrUpdateIndex__p0 */
static void __r3() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc0.proc0.updateIndex__p0;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStateproc0.proc0.updateIndex__p0 = __5;
}



void proc0() {
  {
    static uint8_t __scheduling_clock = 0;
    if (__scheduling_clock == 0) {
      __r0();  /* proc0.updateOutput__chk */
      __r1();  /* proc0.update__p0 */
      __r4();  /* proc0.updateOutput__maj */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r2();  /* proc0.output__p0 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r5();  /* proc0.__send_send0_port_2_var_p0 */
      __r6();  /* proc0.__send_send0_port_1_var_p0 */
      __r7();  /* proc0.sample__p02 */
      __r8();  /* proc0.sample__p01 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 3;
    if (__scheduling_clock == 0) {
      __r3();  /* proc0.incrUpdateIndex__p0 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}


int main (void) {
  int rnds = 0;
  for(rnds; rnds < 40; rnds++) {
    proc0();
    proc1();
    proc2();
    printf("proc0 maj: %u  ", copilotStateproc0.proc0.maj);   
    printf("chk: %u\n", copilotStateproc0.proc0.chk);   
  }
  return 0;
}
