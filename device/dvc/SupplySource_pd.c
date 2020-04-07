/*---------------------------------------------------------
  SupplySource_pd.c
---------------------------------------------------------*/

#include "SupplySource_pd.h"
#include "system.h"
#include "adc.h"
#include "delay.h"

#define SUPPLYSOURCE_CHANNEL    14

static NOINIT TDelay SupplySourceChargingTicks;
static BOOL SupplySourceChargingStarted;
static NOINIT BOOL SupplySourceCharging;

//---------------------------------------------------------
void InitSupplySource_pd(void)
{
  SupplySourceCharging = FALSE;
  SupplySourceChargingStarted = FALSE;
}

//---------------------------------------------------------
BOOL SupplySourceIsCharging_pd(void)
{
  if(SupplySourceChargingStarted == FALSE) {
    SupplySourceChargingStarted = TRUE;
    SupplySourceChargingTicks = DelayBegin();
  }

  if(DelayIsEnded(SupplySourceChargingTicks, SYS_CONVERT_MCS(1000000U)) == TRUE) {
    SupplySourceChargingTicks = DelayBegin();
    if(AdcGet(15U) < (WORD)(0x555)) {
      SupplySourceCharging = TRUE;
    }
    else {
      SupplySourceCharging = FALSE;
    }
  }

  return SupplySourceCharging;
}

//---------------------------------------------------------
FLOAT32 GetSupplySourceVoltage_pd(void)
{
  AdcGet(SUPPLYSOURCE_CHANNEL);
  return 1.833333f*AdcGetAsFloat(SUPPLYSOURCE_CHANNEL); // K=(20+24)/24=1.833(3)
}
