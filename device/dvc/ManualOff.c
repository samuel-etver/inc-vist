/*---------------------------------------------------------
  manualoff.c
---------------------------------------------------------*/

#include "manualoff.h"
#include "ManualOff_pd.h"
#include "system.h"
#include "power.h"
#include "keybrd.h"
#include "delay.h"

static NOINIT BOOL ManualOn;
static NOINIT BYTE ManualOnClk;
static NOINIT BOOL ManualOff;

static BOOL IsPowerOffButtonPressed(void);

//---------------------------------------------------------
void InitManualOff(void)
{
  InitManualOff_pd();
  ManualOn = FALSE;
  ManualOnClk = 0;
  ManualOff = FALSE;
}

//---------------------------------------------------------
static BOOL IsPowerOffButtonPressed(void)
{
  return IsPowerOffButtonPressed_pd();
}

//---------------------------------------------------------
void DoManualOn(void) {
  if(ManualOn == TRUE)
    return;

  if(KeyDown == KB_F1)
    PrgFlags.SuperUser = 1;

  if(PrgFlags.CentiSec)
    if(++ManualOnClk == 30)
      ManualOn = TRUE;
}

//---------------------------------------------------------
void DoManualOff(void)
{
  WORD i;

  if(ManualOn == FALSE)
    return;

  if(ManualOff == TRUE) {
    for(i=0; i<100; i++) {
      if(IsPowerOffButtonPressed() == TRUE)
        return;
      Delay(SYS_CONVERT_MCS(100));
    }

    Halt();
  }

  for(i=0; i<100; i++) {
    if(IsPowerOffButtonPressed() == FALSE)
      return;
    Delay(SYS_CONVERT_MCS(100));
  }

  ManualOff = TRUE;
}


