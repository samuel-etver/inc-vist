#ifndef SYSTEM_PD_H_INCLUDED
#define SYSTEM_PD_H_INCLUDED

/*---------------------------------------------------------
  system_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void InitSystem_pd(void);
void InitInterrupts_pd(void);
void EnableInterrupts_pd(void);
void DisableInterrupts_pd(void);
void InitTicks_pd(void);
DWORD GetTicks_pd(void);
void SetFastFrequency_pd(void);
void SetNormalFrequency_pd(void);
void Halt_pd(void);

#ifdef PD_PC

BOOL SystemIsOn(void);

#else

#endif

#endif
