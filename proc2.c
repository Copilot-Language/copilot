#include <stdbool.h>
#include <stdint.h>
#include "proc0.h"



static uint64_t __global_clock = 0;

void send2(uint16_t val, int port) {
  switch (port) {
  case 0: p20 = val;
  case 1: p21 = val;
  }
}


struct {  /* copilotStateproc2 */
  struct {  /* proc2 */
    uint16_t prophVal__p2[2];
    bool chk;
    uint16_t p2;
    uint16_t maj;
    uint64_t updateIndex__p2;
    uint64_t outputIndex__p2;
    uint16_t tmpSampleVal__p20;
    uint16_t tmpSampleVal__p21;
  } proc2;
} copilotStateproc2 =
{  /* copilotStateproc2 */
  {  /* proc2 */
    /* prophVal__p2 */
    { 0
    , 0
    },
    /* chk */  false,
    /* p2 */  0,
    /* maj */  0,
    /* updateIndex__p2 */  1ULL,
    /* outputIndex__p2 */  0ULL,
    /* tmpSampleVal__p20 */  0,
    /* tmpSampleVal__p21 */  0
  }
};

/* proc2.updateOutput__chk */
static void __r0() {
  bool __0 = true;
  uint32_t __1 = 3UL;
  uint64_t __2 = 0ULL;
  uint64_t __3 = copilotStateproc2.proc2.outputIndex__p2;
  uint64_t __4 = __2 + __3;
  uint64_t __5 = 2ULL;
  uint64_t __6 = __4 % __5;
  uint16_t __7 = copilotStateproc2.proc2.prophVal__p2[__6];
  uint16_t __8 = copilotStateproc2.proc2.tmpSampleVal__p21;
  uint16_t __9 = copilotStateproc2.proc2.tmpSampleVal__p20;
  bool __10 = __8 == __9;
  uint16_t __11 = __10 ? __9 : __7;
  bool __12 = __7 == __11;
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
  copilotStateproc2.proc2.chk = __24;
}

/* proc2.update__p2 */
static void __r1() {
  bool __0 = true;
  uint64_t __1 = 0ULL;
  uint64_t __2 = copilotStateproc2.proc2.outputIndex__p2;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc2.proc2.prophVal__p2[__5];
  uint16_t __7 = 1;
  uint16_t __8 = __6 + __7;
  uint64_t __9 = copilotStateproc2.proc2.updateIndex__p2;
  if (__0) {
  }
  copilotStateproc2.proc2.prophVal__p2[__9] = __8;
}

/* proc2.updateOutput__maj */
static void __r4() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.tmpSampleVal__p21;
  uint16_t __2 = copilotStateproc2.proc2.tmpSampleVal__p20;
  bool __3 = __1 == __2;
  uint64_t __4 = 0ULL;
  uint64_t __5 = copilotStateproc2.proc2.outputIndex__p2;
  uint64_t __6 = __4 + __5;
  uint64_t __7 = 2ULL;
  uint64_t __8 = __6 % __7;
  uint16_t __9 = copilotStateproc2.proc2.prophVal__p2[__8];
  uint16_t __10 = __3 ? __2 : __9;
  if (__0) {
  }
  copilotStateproc2.proc2.maj = __10;
}

/* proc2.output__p2 */
static void __r2() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc2.proc2.outputIndex__p2;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc2.proc2.prophVal__p2[__1];
  if (__0) {
  }
  copilotStateproc2.proc2.outputIndex__p2 = __5;
  copilotStateproc2.proc2.p2 = __6;
}

/* proc2.__send_send2_port_1_var_p2 */
static void __r5() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.p2;
  if (__0) {
    send2(__1,1);
  }
}

/* proc2.__send_send2_port_0_var_p2 */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.p2;
  if (__0) {
    send2(__1,0);
  }
}

/* proc2.sample__p21 */
static void __r7() {
  bool __0 = true;
  uint16_t __1 = p21;
  if (__0) {
  }
  copilotStateproc2.proc2.tmpSampleVal__p21 = __1;
}

/* proc2.sample__p20 */
static void __r8() {
  bool __0 = true;
  uint16_t __1 = p20;
  if (__0) {
  }
  copilotStateproc2.proc2.tmpSampleVal__p20 = __1;
}

/* proc2.incrUpdateIndex__p2 */
static void __r3() {
  bool __0 = true;
  uint64_t __1 = copilotStateproc2.proc2.updateIndex__p2;
  uint64_t __2 = 1ULL;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStateproc2.proc2.updateIndex__p2 = __5;
}



void proc2() {
  {
    static uint8_t __scheduling_clock = 0;
    if (__scheduling_clock == 0) {
      __r0();  /* proc2.updateOutput__chk */
      __r1();  /* proc2.update__p2 */
      __r4();  /* proc2.updateOutput__maj */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r2();  /* proc2.output__p2 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r5();  /* proc2.__send_send2_port_1_var_p2 */
      __r6();  /* proc2.__send_send2_port_0_var_p2 */
      __r7();  /* proc2.sample__p21 */
      __r8();  /* proc2.sample__p20 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 3;
    if (__scheduling_clock == 0) {
      __r3();  /* proc2.incrUpdateIndex__p2 */
      __scheduling_clock = 3;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}


