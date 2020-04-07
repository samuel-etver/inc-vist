/*---------------------------------------------------------
  SupplySource_pd.c
---------------------------------------------------------*/

#include "SupplySource_pd.h"

FLOAT32 SupplySourceVoltage;

//---------------------------------------------------------
void InitSupplySource_pd(void)
{
  SupplySourceVoltage = 4.0f;
}

//---------------------------------------------------------
BOOL SupplySourceIsCharging_pd(void)
{
   return FALSE;
}

//---------------------------------------------------------
float GetSupplySourceVoltage_pd(void)
{
  return SupplySourceVoltage;
}
