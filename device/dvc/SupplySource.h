#ifndef SUPPLYSOURCE_H_INCLUDED
#define SUPPLYSOURCE_H_INCLUDED

/*---------------------------------------------------------
  SupplySource.h
---------------------------------------------------------*/

#include "xmain.h"

typedef enum {
  ssAccumulator = 0, SUPPLYSOURCE_COUNT
} TSupplySource;

void InitSupplySource(void);
void DoSupplySource(void);
void ShowSupplySourceIni(void);
void ShowSupplySource(void);

void SupplySourceVoltageToStr(BYTE* buff, FLOAT32 voltage);

TSupplySource SupplySourceGet(void);
FLOAT32 GetSupplySourceChargeLevel(void);
BOOL SupplySourceIsCharging(void);

extern NOINIT BOOL SupplySourceMeasureDone;
extern NOINIT FLOAT32 SupplySourceVoltage;

#endif
