#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>


static uint64_t __global_clock = 0;






struct {  /* copilotStatecopilot_maj */
  struct {  /* copilot_maj */
    uint64_t prophVal__v4[2];
    uint64_t prophVal__v3[2];
    uint64_t prophVal__v2[2];
    uint64_t prophVal__v1[2];
    uint64_t prophVal__v0[2];
    uint64_t v4;
    uint64_t v3;
    uint64_t v2;
    uint64_t v1;
    uint64_t v0;
    uint64_t ans;
    uint32_t updateIndex__v4;
    uint32_t updateIndex__v3;
    uint32_t updateIndex__v2;
    uint32_t updateIndex__v1;
    uint32_t updateIndex__v0;
    uint32_t outputIndex__v4;
    uint32_t outputIndex__v3;
    uint32_t outputIndex__v2;
    uint32_t outputIndex__v1;
    uint32_t outputIndex__v0;
  } copilot_maj;
} copilotStatecopilot_maj =
{  /* copilotStatecopilot_maj */
  {  /* copilot_maj */
    /* prophVal__v4 */
    { 1ULL
    , 0ULL
    },
    /* prophVal__v3 */
    { 2ULL
    , 0ULL
    },
    /* prophVal__v2 */
    { 1ULL
    , 0ULL
    },
    /* prophVal__v1 */
    { 2ULL
    , 0ULL
    },
    /* prophVal__v0 */
    { 1ULL
    , 0ULL
    },
    /* v4 */  0ULL,
    /* v3 */  0ULL,
    /* v2 */  0ULL,
    /* v1 */  0ULL,
    /* v0 */  0ULL,
    /* ans */  0ULL,
    /* updateIndex__v4 */  1UL,
    /* updateIndex__v3 */  1UL,
    /* updateIndex__v2 */  1UL,
    /* updateIndex__v1 */  1UL,
    /* updateIndex__v0 */  1UL,
    /* outputIndex__v4 */  0UL,
    /* outputIndex__v3 */  0UL,
    /* outputIndex__v2 */  0UL,
    /* outputIndex__v1 */  0UL,
    /* outputIndex__v0 */  0UL
  }
};

/* copilot_maj.update__v4 */
static inline void __r0() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v4;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v4[__5];
  uint64_t __7 = 1ULL;
  uint64_t __8 = __6 + __7;
  uint32_t __9 = copilotStatecopilot_maj.copilot_maj.updateIndex__v4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.prophVal__v4[__9] = __8;
}

/* copilot_maj.update__v3 */
static inline void __r3() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v3;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v3[__5];
  uint64_t __7 = 1ULL;
  uint64_t __8 = __6 + __7;
  uint32_t __9 = copilotStatecopilot_maj.copilot_maj.updateIndex__v3;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.prophVal__v3[__9] = __8;
}

/* copilot_maj.update__v2 */
static inline void __r6() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v2;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v2[__5];
  uint64_t __7 = 1ULL;
  uint64_t __8 = __6 + __7;
  uint32_t __9 = copilotStatecopilot_maj.copilot_maj.updateIndex__v2;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.prophVal__v2[__9] = __8;
}

/* copilot_maj.update__v1 */
static inline void __r9() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v1;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v1[__5];
  uint64_t __7 = 1ULL;
  uint64_t __8 = __6 + __7;
  uint32_t __9 = copilotStatecopilot_maj.copilot_maj.updateIndex__v1;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.prophVal__v1[__9] = __8;
}

/* copilot_maj.update__v0 */
static inline void __r12() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v0;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v0[__5];
  uint64_t __7 = 1ULL;
  uint64_t __8 = __6 + __7;
  uint32_t __9 = copilotStatecopilot_maj.copilot_maj.updateIndex__v0;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.prophVal__v0[__9] = __8;
}

/* copilot_maj.updateOutput__ans */
static inline void __r15() {
  bool __0 = true;
  uint32_t __1 = 0UL;
  uint32_t __2 = copilotStatecopilot_maj.copilot_maj.outputIndex__v3;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  uint64_t __6 = copilotStatecopilot_maj.copilot_maj.prophVal__v3[__5];
  uint32_t __7 = copilotStatecopilot_maj.copilot_maj.outputIndex__v1;
  uint32_t __8 = __1 + __7;
  uint32_t __9 = __8 % __4;
  uint64_t __10 = copilotStatecopilot_maj.copilot_maj.prophVal__v1[__9];
  uint32_t __11 = copilotStatecopilot_maj.copilot_maj.outputIndex__v0;
  uint32_t __12 = __1 + __11;
  uint32_t __13 = __12 % __4;
  uint64_t __14 = copilotStatecopilot_maj.copilot_maj.prophVal__v0[__13];
  bool __15 = __10 == __14;
  uint32_t __16 = 1UL;
  uint32_t __17 = __16 + __16;
  uint32_t __18 = __16 - __16;
  uint32_t __19 = __15 ? __17 : __18;
  bool __20 = __19 == __1;
  uint32_t __21 = copilotStatecopilot_maj.copilot_maj.outputIndex__v2;
  uint32_t __22 = __1 + __21;
  uint32_t __23 = __22 % __4;
  uint64_t __24 = copilotStatecopilot_maj.copilot_maj.prophVal__v2[__23];
  uint64_t __25 = __20 ? __24 : __14;
  bool __26 = __6 == __25;
  bool __27 = ! __26;
  bool __28 = __24 == __14;
  bool __29 = ! __28;
  bool __30 = ! __20;
  bool __31 = __29 && __30;
  uint32_t __32 = __19 - __16;
  uint32_t __33 = __19 + __16;
  uint32_t __34 = __31 ? __32 : __33;
  bool __35 = __34 == __1;
  bool __36 = ! __35;
  bool __37 = __27 && __36;
  uint32_t __38 = __34 - __16;
  uint32_t __39 = __34 + __16;
  uint32_t __40 = __37 ? __38 : __39;
  bool __41 = __40 == __1;
  uint32_t __42 = copilotStatecopilot_maj.copilot_maj.outputIndex__v4;
  uint32_t __43 = __1 + __42;
  uint32_t __44 = __43 % __4;
  uint64_t __45 = copilotStatecopilot_maj.copilot_maj.prophVal__v4[__44];
  uint64_t __46 = __35 ? __6 : __25;
  uint64_t __47 = __41 ? __45 : __46;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.ans = __47;
}

/* copilot_maj.output__v4 */
static inline void __r1() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.outputIndex__v4;
  uint64_t __2 = copilotStatecopilot_maj.copilot_maj.prophVal__v4[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.v4 = __2;
  copilotStatecopilot_maj.copilot_maj.outputIndex__v4 = __6;
}

/* copilot_maj.output__v3 */
static inline void __r4() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.outputIndex__v3;
  uint64_t __2 = copilotStatecopilot_maj.copilot_maj.prophVal__v3[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.v3 = __2;
  copilotStatecopilot_maj.copilot_maj.outputIndex__v3 = __6;
}

/* copilot_maj.output__v2 */
static inline void __r7() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.outputIndex__v2;
  uint64_t __2 = copilotStatecopilot_maj.copilot_maj.prophVal__v2[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.v2 = __2;
  copilotStatecopilot_maj.copilot_maj.outputIndex__v2 = __6;
}

/* copilot_maj.output__v1 */
static inline void __r10() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.outputIndex__v1;
  uint64_t __2 = copilotStatecopilot_maj.copilot_maj.prophVal__v1[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.v1 = __2;
  copilotStatecopilot_maj.copilot_maj.outputIndex__v1 = __6;
}

/* copilot_maj.output__v0 */
static inline void __r13() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.outputIndex__v0;
  uint64_t __2 = copilotStatecopilot_maj.copilot_maj.prophVal__v0[__1];
  uint32_t __3 = 1UL;
  uint32_t __4 = __1 + __3;
  uint32_t __5 = 2UL;
  uint32_t __6 = __4 % __5;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.v0 = __2;
  copilotStatecopilot_maj.copilot_maj.outputIndex__v0 = __6;
}

/* copilot_maj.incrUpdateIndex__v4 */
static inline void __r2() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.updateIndex__v4;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.updateIndex__v4 = __5;
}

/* copilot_maj.incrUpdateIndex__v3 */
static inline void __r5() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.updateIndex__v3;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.updateIndex__v3 = __5;
}

/* copilot_maj.incrUpdateIndex__v2 */
static inline void __r8() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.updateIndex__v2;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.updateIndex__v2 = __5;
}

/* copilot_maj.incrUpdateIndex__v1 */
static inline void __r11() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.updateIndex__v1;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.updateIndex__v1 = __5;
}

/* copilot_maj.incrUpdateIndex__v0 */
static inline void __r14() {
  bool __0 = true;
  uint32_t __1 = copilotStatecopilot_maj.copilot_maj.updateIndex__v0;
  uint32_t __2 = 1UL;
  uint32_t __3 = __1 + __2;
  uint32_t __4 = 2UL;
  uint32_t __5 = __3 % __4;
  if (__0) {
  }
  copilotStatecopilot_maj.copilot_maj.updateIndex__v0 = __5;
}



void copilot_maj() {


  {
    static uint8_t __scheduling_clock = 1;
    if (__scheduling_clock == 0) {
      __r0();  /* copilot_maj.update__v4 */
      __r3();  /* copilot_maj.update__v3 */
      __r6();  /* copilot_maj.update__v2 */
      __r9();  /* copilot_maj.update__v1 */
      __r12();  /* copilot_maj.update__v0 */
      __r15();  /* copilot_maj.updateOutput__ans */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 2;
    if (__scheduling_clock == 0) {
      __r1();  /* copilot_maj.output__v4 */
      __r4();  /* copilot_maj.output__v3 */
      __r7();  /* copilot_maj.output__v2 */
      __r10();  /* copilot_maj.output__v1 */
      __r13();  /* copilot_maj.output__v0 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }
  {
    static uint8_t __scheduling_clock = 4;
    if (__scheduling_clock == 0) {
      __r2();  /* copilot_maj.incrUpdateIndex__v4 */
      __r5();  /* copilot_maj.incrUpdateIndex__v3 */
      __r8();  /* copilot_maj.incrUpdateIndex__v2 */
      __r11();  /* copilot_maj.incrUpdateIndex__v1 */
      __r14();  /* copilot_maj.incrUpdateIndex__v0 */
      __scheduling_clock = 4;
    }
    else {
      __scheduling_clock = __scheduling_clock - 1;
    }
  }

  __global_clock = __global_clock + 1;

}



void __copilot_maj(void) {
  int i;
  for(i = 0; i < 5; i++) {
    copilot_maj();
  }
}

int main(void) {
  uint64_t i;
  for(i = 0; i < 10000000LLU; i++) {
    __copilot_maj();
    printf("maj: %llu\n\n", copilotStatecopilot_maj.copilot_maj.ans);
  }
  return 1;
}
