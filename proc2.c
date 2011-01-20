#include <stdbool.h>
#include <stdint.h>


#include <stdlib.h>
#include <stdio.h>
uint16_t p0;
uint16_t p1;
uint16_t p2;
void send2(uint16_t val, int port) {
  switch (port) {
  case 1: p0 = val;
  case 2: p1 = val;
  }
}


static uint64_t __global_clock = 0;



struct {  /* copilotStateproc2 */
  struct {  /* proc2 */
    uint16_t prophVal__p2[2];
    bool chk;
    uint16_t p2;
    uint16_t maj;
    uint64_t updateIndex__p2;
    uint64_t outputIndex__p2;
    uint16_t tmpSampleVal__p0;
    uint16_t tmpSampleVal__p1;
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
    /* tmpSampleVal__p0 */  0,
    /* tmpSampleVal__p1 */  0
  }
};

/* proc2.sample__p0 */
static void __r7() {
  bool __0 = true;
  uint16_t __1 = p0;
  if (__0) {
  }
  copilotStateproc2.proc2.tmpSampleVal__p0 = __1;
}

/* proc2.sample__p1 */
static void __r8() {
  bool __0 = true;
  uint16_t __1 = p1;
  if (__0) {
  }
  copilotStateproc2.proc2.tmpSampleVal__p1 = __1;
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
  uint16_t __8 = copilotStateproc2.proc2.tmpSampleVal__p1;
  uint16_t __9 = copilotStateproc2.proc2.tmpSampleVal__p0;
  bool __10 = __8 == __9;
  uint32_t __11 = 1UL;
  uint32_t __12 = __11 + __11;
  uint32_t __13 = __11 - __11;
  uint32_t __14 = __10 ? __12 : __13;
  uint32_t __15 = 0UL;
  bool __16 = __14 == __15;
  uint16_t __17 = __16 ? __7 : __9;
  bool __18 = __7 == __17;
  bool __19 = __8 == __17;
  bool __20 = __9 == __17;
  uint32_t __21 = __20 ? __11 : __15;
  uint32_t __22 = __21 + __11;
  uint32_t __23 = __19 ? __22 : __21;
  uint32_t __24 = __23 + __11;
  uint32_t __25 = __18 ? __24 : __23;
  uint32_t __26 = 2UL;
  uint32_t __27 = __25 * __26;
  bool __28 = __1 < __27;
  if (__0) {
  }
  copilotStateproc2.proc2.chk = __28;
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

/* proc2.updateOutput__maj */
static void __r4() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.tmpSampleVal__p1;
  uint16_t __2 = copilotStateproc2.proc2.tmpSampleVal__p0;
  bool __3 = __1 == __2;
  uint32_t __4 = 1UL;
  uint32_t __5 = __4 + __4;
  uint32_t __6 = __4 - __4;
  uint32_t __7 = __3 ? __5 : __6;
  uint32_t __8 = 0UL;
  bool __9 = __7 == __8;
  uint64_t __10 = 0ULL;
  uint64_t __11 = copilotStateproc2.proc2.outputIndex__p2;
  uint64_t __12 = __10 + __11;
  uint64_t __13 = 2ULL;
  uint64_t __14 = __12 % __13;
  uint16_t __15 = copilotStateproc2.proc2.prophVal__p2[__14];
  uint16_t __16 = __9 ? __15 : __2;
  if (__0) {
  }
  copilotStateproc2.proc2.maj = __16;
}

/* proc2.__send_send2_port_2_var_p2 */
static void __r5() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.p2;
  if (__0) {
    send2(__1,2);
  }
}

/* proc2.__send_send2_port_1_var_p2 */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc2.proc2.p2;
  if (__0) {
    send2(__1,1);
  }
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
      __r7();  /* proc2.sample__p0 */
      __r8();  /* proc2.sample__p1 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r1();  /* proc2.update__p2 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r0();  /* proc2.updateOutput__chk */
      __r2();  /* proc2.output__p2 */
      __r4();  /* proc2.updateOutput__maj */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 3;
    if (__scheduling_clock == 0) {
      __r5();  /* proc2.__send_send2_port_2_var_p2 */
      __r6();  /* proc2.__send_send2_port_1_var_p2 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 4;
    if (__scheduling_clock == 0) {
      __r3();  /* proc2.incrUpdateIndex__p2 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}



void __proc2(void) {
  int i;
  for(i = 0; i < 5; i++) {
    proc2();
  }
}

