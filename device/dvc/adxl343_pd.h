#ifndef ADXL343_PD_H_INCLUDED
#define ADXL343_PD_H_INCLUDED

/*---------------------------------------------------------
  adxl343_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void Adxl343Init_pd(void);
void Adxl343Setup_pd(void);
BOOL Adxl343IsActive_pd(void);
BYTE Adxl343ReadStatus_pd(void);
BOOL Adxl343CheckSlave_pd(void);

#endif
