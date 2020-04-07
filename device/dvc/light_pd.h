#ifndef LIGHT_PD_H_INCLUDED
#define LIGHT_PD_H_INCLUDED

/*---------------------------------------------------------
  light_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void InitLight_pd(void);
void SetupLight_pd(void);
void DoLight_pd(void);
void LightOn_pd(void);
void LightOff_pd(void);
void LightSetContrast_pd(BYTE contrast);

#endif
