#ifndef __COUNTDOWN
#define __COUNTDOWN

#include <platform.h>

typedef struct CountdownWork {
  u8 field_0x0[0x40 - 0x0]; //0x0
} CountdownWork;

void countDownInit(void);
void countDownSaveReStart(void);



void countDownMain(void);

#endif
