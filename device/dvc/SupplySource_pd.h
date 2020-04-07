#ifndef SUPPLYSOURCE_PD_H_INCLUDED
#define SUPPLYSOURCE_PD_H_INCLUDED

/*---------------------------------------------------------
  SupplySource_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void InitSupplySource_pd(void);
BOOL SupplySourceIsCharging_pd(void);
FLOAT32 GetSupplySourceVoltage_pd(void);

#ifdef PD_PC

extern FLOAT32 SupplySourceVoltage;

#else

#endif

#endif
