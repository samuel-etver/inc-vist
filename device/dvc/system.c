/*---------------------------------------------------------
  system.h
---------------------------------------------------------*/

#include "system.h"
#include "system_pd.h"
#include "light.h"
#include "power.h"
#include "lcd.h"
#include "sound.h"
#include "iic.h"

static NOINIT BOOL InterruptsEnabled;
static NOINIT DWORD Ticks;

static void InitTicks(void);
static void InitInterrupts(void);

static void SetFastFrequencyBefore(void);
static void SetFastFrequencyAfter(void);
static void SetNormalFrequencyBefore(void);
static void SetNormalFrequencyAfter(void);

//---------------------------------------------------------
void PrepareSystem(void)
{
  InitInterrupts();
}

//---------------------------------------------------------
void InitSystem(void)
{
#ifdef SUPERUSER  
  PrgFlags.SuperUser = 1;
#else  
  PrgFlags.SuperUser = 0;
#endif  

  PrgVerb = VB_CHIPCHECK_INI;

  InitTicks();
  InitSystem_pd();
  EnableInterrupts();
}

//---------------------------------------------------------
static void InitInterrupts(void)
{
  InitInterrupts_pd();
}

//---------------------------------------------------------
void EnableInterrupts(void)
{
  EnableInterrupts_pd();
  InterruptsEnabled = TRUE;
}

//---------------------------------------------------------
void DisableInterrupts(void)
{
  DisableInterrupts_pd();
  InterruptsEnabled = FALSE;
}

//---------------------------------------------------------
BOOL IsInterruptsEnabled(void)
{
  return InterruptsEnabled;
}

//---------------------------------------------------------
static void InitTicks(void)
{
  InitTicks_pd();
  Ticks = GetTicks();
}

//---------------------------------------------------------
DWORD GetTicks(void)
{
  return GetTicks_pd();
}

//---------------------------------------------------------
void DoTicks(void)
{
  DWORD new_ticks = GetTicks();

  if((DWORD)(new_ticks - Ticks) >= SYS_CONVERT_MCS(200000)) {
    Ticks = new_ticks;
    PrgFlags.CentiSec = 1;
  }
  else if((DWORD)(new_ticks - Ticks) >= SYS_CONVERT_MCS(100000)) {
    Ticks += SYS_CONVERT_MCS(100000);
    PrgFlags.CentiSec = 1;
  }
  else
    PrgFlags.CentiSec = 0;
}

//---------------------------------------------------------
void Halt(void)
{
  LightOff();
  LcdDrawBegin();
  LcdSetFgColor(0);
  LcdDrawRect(0,       0, LCD_W/2, LCD_H);
  LcdDrawRect(LCD_W/2, 0, LCD_W/2, LCD_H);
  LcdDrawEnd();
  PowerOff();
  Halt_pd();
}

//---------------------------------------------------------
void SetFastFrequency(void)
{
  SetFastFrequencyBefore();
  SetFastFrequency_pd();
  SetFastFrequencyAfter();
}

//---------------------------------------------------------
void SetNormalFrequency(void)
{
  SetNormalFrequencyBefore();
  SetNormalFrequency_pd();
  SetNormalFrequencyAfter();
}

//---------------------------------------------------------
static void SetFastFrequencyBefore(void)
{
  SoundSetFastFrequencyBefore();
  IicSetFastFrequencyBefore();
}

//---------------------------------------------------------
static void SetFastFrequencyAfter(void)
{
}

//---------------------------------------------------------
static void SetNormalFrequencyBefore(void)
{
  SoundSetNormalFrequencyBefore();
}

//---------------------------------------------------------
static void SetNormalFrequencyAfter(void)
{
  IicSetNormalFrequencyAfter();
}
