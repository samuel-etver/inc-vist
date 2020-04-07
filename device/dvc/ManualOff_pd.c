/*---------------------------------------------------------
  ManualOff_pd.c
---------------------------------------------------------*/

#include "ManualOff_pd.h"
#include "system.h"

#define MANUALOFF_PORT  GPIOC
#define MANUALOFF_PIN   GPIO_PIN_13

//---------------------------------------------------------
void InitManualOff_pd(void)
{
}

//---------------------------------------------------------
BOOL IsPowerOffButtonPressed_pd(void)
{
  if(GPIO_IS_IN_HIGH(MANUALOFF_PORT, MANUALOFF_PIN))
    return FALSE;
  return TRUE;
}
