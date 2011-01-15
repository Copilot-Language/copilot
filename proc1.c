#include <stdbool.h>
#include <stdint.h>
#include "proc0.h"



static uint64_t __global_clock = 0;

void send1(uint16_t val, int port) {
  switch (port) {
  case 0: p10 = val;
  case 2: p12 = val;
  }
}



struct {  /* copilotStateproc1 */
  struct {  /* proc1 */
    uint16_t prophVal__p1[2];
    bool chk;
    uint16_t p1;
    uint16_t maj;
    uint64_t updateIndex__p1;
    uint64_t outputIndex__p1;
    uint16_t tmpSampleVal__p10;
    uint16_t tmpSampleVal__p12;
  } proc1;
} copilotStateproc1 =
{  /* copilotStateproc1 */
  {  /* proc1 */
    /* prophVal__p1 */
    { 0
    , 0
    },
    /* chk */  false,
    /* p1 */  0,
    /* maj */  0,
    /* updateIndex__p1 */  1ULL,
    /* outputIndex__p1 */  0ULL,
    /* tmpSampleVal__p10 */  0,
    /* tmpSampleVal__p12 */  0
  }
};

/* proc1.updateOutput__chk */
static void __r0() {
  bool __0 = true;
  uint32_t __1 = 3UL;
  uint16_t __2 = copilotStateproc1.proc1.tmpSampleVal__p12;
  uint64_t __3 = 0ULL;
  uint64_t __4 = copilotStateproc1.proc1.outputIndex__p1;
  uint64_t __5 = __3 + __4;
  uint64_t __6 = 2ULL;
  uint64_t __7 = __5 % __6;
  uint16_t __8 = copilotStateproc1.proc1.prophVal__p1[__7];
  uint16_t __9 = copilotStateproc1.proc1.tmpSampleVal__p10;
  bool __10 = __8 == __9;
  uint16_t __11 = __10 ? __9 : __2;
  bool __12 = __2 == __11;
  bool __13 = __8 == __11;
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
  copilotStateproc1.proc1.chk = __24;
}

/* proc1.update__p1 */
static void __r1() {
  bool __0 = true;
  uint64_t __1 = 0ULL;
  uint64_t __2 = copilotStateproc1.proc1.outputIndex__p1;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc1.proc1.prophVal__p1[__5];
  uint16_t __7 = 1;
  uint16_t __8 = __6 + __7;
  uint64_t __9 = copilotStateproc1.proc1.updateIndex__p1;
  if (__0) {
  }
  copilotStateproc1.proc1.prophVal__p1[__9] = __8;
}

/* proc1.updateOutput__maj */
static void __r4() {
  bool __0 = true;
  uint64_t __1 = 0ULL;
  uint64_t __2 = copilotStateproc1.proc1.outputIndex__p1;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc1.proc1.prophVal__p1[__5];
  uint16_t __7 = copilotStateproc1.proc1.tmpSampleVal__p10;
  bool __8 = __6 == __7;
  uint16_t __9 = copilotStateproc1.proc1.tmpSampleVal__p12;
  uint16_t __10 = __8 ? __7 : __9;
  if (__0) {
  }
  copilotStateproc1.proc1.maj = __10;
}

/* proc1.output__p1 */
static void __r2() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc1.proc1.outputIndex__p1;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc1.proc1.prophVal__p1[__1];
  if (__0) {
  }
  copilotStateproc1.proc1.outputIndex__p1 = __5;
  copilotStateproc1.proc1.p1 = __6;
}

/* proc1.__send_send1_port_2_var_p1 */
static void __r5() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc1.proc1.p1;
  if (__0) {
    send1(__1,2);
  }
}

/* proc1.__send_send1_port_0_var_p1 */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc1.proc1.p1;
  if (__0) {
    send1(__1,0);
  }
}

/* proc1.sample__p12 */
static void __r7() {
  bool __0 = true;
  uint16_t __1 = p12;
  if (__0) {
  }
  copilotStateproc1.proc1.tmpSampleVal__p12 = __1;
}

/* proc1.sample__p10 */
static void __r8() {
  bool __0 = true;
  uint16_t __1 = p10;
  if (__0) {
  }
  copilotStateproc1.proc1.tmpSampleVal__p10 = __1;
}

/* proc1.incrUpdateIndex__p1 */
static void __r3() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc1.proc1.updateIndex__p1;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStateproc1.proc1.updateIndex__p1 = __5;
}



void proc1() {
  {
    static uint8_t __scheduling_clock = 0;
    if (__scheduling_clock == 0) {
      __r0();  /* proc1.updateOutput__chk */
      __r1();  /* proc1.update__p1 */
      __r4();  /* proc1.updateOutput__maj */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r2();  /* proc1.output__p1 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r5();  /* proc1.__send_send1_port_2_var_p1 */
      __r6();  /* proc1.__send_send1_port_0_var_p1 */
      __r7();  /* proc1.sample__p12 */
      __r8();  /* proc1.sample__p10 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 3;
    if (__scheduling_clock == 0) {
      __r3();  /* proc1.incrUpdateIndex__p1 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}


