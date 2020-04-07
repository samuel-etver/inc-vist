#ifndef LIGHT_H_INCLUDED
#define LIGHT_H_INCLUDED

/*---------------------------------------------------------
  light.h
---------------------------------------------------------*/

#include "xmain.h"

void InitLight(void);
void SetupLight(void);
void DoLight(void);

void LightOn(void);
void LightOff(void);
void LightSetContrast(BYTE contrast);

extern NOINIT BOOL Light;

#endif
