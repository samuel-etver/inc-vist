#ifndef ADXL343_H_INCLUDED
#define ADXL343_H_INCLUDED

/*---------------------------------------------------------
  adxl343.h
---------------------------------------------------------*/

#include "xmain.h"
#include "adxl343_pd.h"

void Adxl343Init(void);
void Adxl343Setup(void);
BOOL Adxl343IsActive(void);
BYTE Adxl343ReadStatus(void);
BOOL Adxl343CheckSlave(void);

#endif

