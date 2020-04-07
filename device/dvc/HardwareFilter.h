#ifndef HARDWAREFILTER_H_INCLUDED
#define HARDWAREFILTER_H_INCLUDED

/*---------------------------------------------------------
  HardwareFilter.h
---------------------------------------------------------*/

#include "xmain.h"

void HardwareFilterInit(void);

void HardwareFilterOn(void);
void HardwareFilterOff(void);

void HardwareFilterSetFrequency(float f);

#endif
