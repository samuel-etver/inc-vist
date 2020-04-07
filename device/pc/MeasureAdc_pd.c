/*---------------------------------------------------------
  MeasureAdc_pd.h
---------------------------------------------------------*/

#include "MeasureAdc_pd.h"
#include "MeasureAdc.h"

FLOAT32 MeasureAdcVoltageF = 0.0f;
FLOAT32 MeasureAdcVoltageS = 0.0f;

//---------------------------------------------------------
FLOAT32 MeasureAdcGetVoltage_pd(BYTE channel)
{
/*  switch(channel) {
    case MEASUREADC_CHANNEL_F:
      return MeasureAdcVoltageF + MeasureAdcOffsetFasFloat;
    case MEASUREADC_CHANNEL_S:
      return MeasureAdcVoltageS + MeasureAdcOffsetSasFloat;
    default: ;
  }*/
  return 0;
}
