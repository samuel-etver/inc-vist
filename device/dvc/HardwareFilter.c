/*---------------------------------------------------------
  HardwareFilter.c
---------------------------------------------------------*/

#include "HardwareFilter.h"
#include "max7400.h"

//---------------------------------------------------------
void HardwareFilterInit(void)
{
  InitMax7400();
  HardwareFilterOff();
}

//---------------------------------------------------------
void HardwareFilterOn(void)
{
  Max7400On();
}

//---------------------------------------------------------
void HardwareFilterOff(void)
{
  Max7400Off();
}

//---------------------------------------------------------
void HardwareFilterSetFrequency(float f)
{
  Max7400SetFrequency(f);
}
