/*---------------------------------------------------------
  delay.c
---------------------------------------------------------*/

#include "delay.h"
#include "system.h"

//---------------------------------------------------------
TDelay DelayBegin(void)
{
  return (TDelay)GetTicks();
}

//---------------------------------------------------------
void DelayEnd(TDelay delay, DWORD value)
{
  while((DWORD)(GetTicks() - (DWORD)delay) < value);
}

//---------------------------------------------------------
BOOL DelayIsEnded(TDelay delay, DWORD value)
{
  if((DWORD)(GetTicks() - (DWORD)delay) < value)
    return FALSE;
  return TRUE;
}

//---------------------------------------------------------
void Delay(DWORD value)
{
  TDelay delay = DelayBegin();
  DelayEnd(delay, value);
}
