/*---------------------------------------------------------
  adc.c
---------------------------------------------------------*/

#include "adc.h"
#include "IonCalibration.h"

//---------------------------------------------------------
void InitAdc(void)
{
  InitAdc_pd();
}

//---------------------------------------------------------
FLOAT32 AdcGetK(void)
{
  return IonCalibrationGet()/(FLOAT32)ADC_CODE_MAX;
}

//---------------------------------------------------------
WORD AdcGet(BYTE channel)
{
  return AdcGet_pd(channel);
}

//---------------------------------------------------------
FLOAT32 AdcGetAsFloat(BYTE channel)
{
  return AdcGet(channel)*AdcGetK();
}
