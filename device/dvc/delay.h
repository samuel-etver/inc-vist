#ifndef DELAY_H_INCLUDED
#define DELAY_H_INCLUDED

/*---------------------------------------------------------
 delay.h
---------------------------------------------------------*/

#include "xmain.h"
#include "system.h"

typedef DWORD TDelay;

TDelay DelayBegin(void);
void DelayEnd(TDelay delay, DWORD value);
BOOL DelayIsEnded(TDelay delay, DWORD value);
void Delay(DWORD value);

#endif
