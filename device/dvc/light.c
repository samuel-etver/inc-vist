/*---------------------------------------------------------
  light.c
---------------------------------------------------------*/

#include "light.h"
#include "light_pd.h"
#include "keybrd.h"
#include "accelerometer.h"

NOINIT BOOL Light;

//---------------------------------------------------------
void InitLight(void)
{
  Light = TRUE;
  InitLight_pd();
}

//---------------------------------------------------------
void SetupLight(void)
{
  SetupLight_pd();
}

//---------------------------------------------------------
void DoLight(void)
{
  DoLight_pd();
}

//---------------------------------------------------------
void LightOn(void)
{
  if(Light != TRUE) {
    Light = TRUE;
    LightOn_pd();
  }
}

//---------------------------------------------------------
void LightOff(void)
{
  if(Light != FALSE) {
    Light = FALSE;
    LightOff_pd();
  }
}

//---------------------------------------------------------
void LightSetContrast(BYTE contrast)
{
  LightSetContrast_pd(contrast);
}
