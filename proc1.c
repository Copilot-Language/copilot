#include <stdbool.h>
#include <stdint.h>


#include <stdlib.h>
#include <stdio.h>
uint16_t p0;
uint16_t p1;
uint16_t p2;
void send1(uint16_t val, int port) {
  switch (port) {
  case 1: p2 = val;
  case 2: p0 = val;
  }
}


static uint64_t __global_clock = 0;



struct {  /* copilotStateproc1 */
  struct {  /* proc1 */
    uint16_t prophVal__p1[2];
    bool chk;
    uint16_t p1;
    uint16_t maj;
    uint64_t updateIndex__p1;
    uint64_t outputIndex__p1;
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
    /* outputIndex__p1 */  0ULL
  }
};

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

/* proc1.updateOutput__chk */
static void __r0() {
  bool __0 = true;
  uint32_t __1 = 3UL;
  uint32_t __2 = 1UL;
  uint32_t __3 = __2 + __2;
  uint32_t __4 = __3 + __2;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 * __5;
  bool __7 = __1 < __6;
  if (__0) {
  }
  copilotStateproc1.proc1.chk = __7;
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

/* proc1.updateOutput__maj */
static void __r4() {
  bool __0 = true;
  uint64_t __1 = 0ULL;
  uint64_t __2 = copilotStateproc1.proc1.outputIndex__p1;
  uint64_t __3 = __1 + __2;
  uint64_t __4 = 2ULL;
  uint64_t __5 = __3 % __4;
  uint16_t __6 = copilotStateproc1.proc1.prophVal__p1[__5];
  if (__0) {
  }
  copilotStateproc1.proc1.maj = __6;
}

/* proc1.__send_send1_port_2_var_p1 */
static void __r5() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc1.proc1.p1;
  if (__0) {
    send1(__1,2);
  }
}

/* proc1.__send_send1_port_1_var_p1 */
static void __r6() {
  bool __0 = true;
  uint16_t __1 = copilotStateproc1.proc1.p1;
  if (__0) {
    send1(__1,1);
  }
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
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r1();  /* proc1.update__p1 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r0();  /* proc1.updateOutput__chk */
      __r2();  /* proc1.output__p1 */
      __r4();  /* proc1.updateOutput__maj */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 3;
    if (__scheduling_clock == 0) {
      __r5();  /* proc1.__send_send1_port_2_var_p1 */
      __r6();  /* proc1.__send_send1_port_1_var_p1 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 4;
    if (__scheduling_clock == 0) {
      __r3();  /* proc1.incrUpdateIndex__p1 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}



void __proc1(void) {
  int i;
  for(i = 0; i < 5; i++) {
    proc1();
  }
}

