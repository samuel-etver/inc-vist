/*---------------------------------------------------------
  system_pd.c
---------------------------------------------------------*/

#include "system_pd.h"
#include "system.h"

static BOOL SystemOn;

//---------------------------------------------------------
void PrepareSystem_pd(void)
{
}

//---------------------------------------------------------
void InitSystem_pd(void)
{
  SystemOn = TRUE;
  DebugInt32 = 0;
}

//---------------------------------------------------------
void InitTicks_pd(void)
{

}

//---------------------------------------------------------
void InitInterrupts_pd(void)
{

}

//---------------------------------------------------------
void EnableInterrupts_pd(void)
{

}

//---------------------------------------------------------
void DisableInterrupts_pd(void)
{

}

//---------------------------------------------------------
void SetNormalFrequency_pd(void)
{

}

//---------------------------------------------------------
void SetFastFrequency_pd(void)
{

}

//---------------------------------------------------------
DWORD GetTicks_pd(void)
{
  return (DWORD)GetTickCount();
}

//---------------------------------------------------------
BOOL SystemIsOn(void)
{
  return SystemOn;
}

//---------------------------------------------------------
void Halt_pd(void)
{
  SystemOn = FALSE;
}
