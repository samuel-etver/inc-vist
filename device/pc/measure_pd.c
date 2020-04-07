/*---------------------------------------------------------
  measure_pc.c
---------------------------------------------------------*/

#include "measure_pd.h"
#include "measure.h"

NOINIT FLOAT32 MeasureT_pd;
NOINIT FLOAT32 MeasureNoise_pd;

//---------------------------------------------------------
void MeasureMeasureF_pd(void)
{
}

//---------------------------------------------------------
void MeasureOnEnter_pd(void)
{
}

//---------------------------------------------------------
void MeasureOnExit_pd(void)
{
}

//---------------------------------------------------------
void MeasureInit_pd(void)
{
  MeasureT_pd = 0.0f;
  MeasureNoise_pd = 0.0f;
}

//---------------------------------------------------------
void MeasureSetSon_pd(void)
{
}

//---------------------------------------------------------
void MeasureSetVon_pd(void)
{
}

//---------------------------------------------------------
void MeasureSetAon_pd(void)
{
}

//---------------------------------------------------------
BOOL MeasureCalcT_pd(FLOAT32* data)
{
  data[0] = MeasureT_pd;
  data[1] = MeasureNoise_pd;
  data[2] = MeasureUpeak;
  data[3] = MeasureUamp;
  MeasurePeriodAvailable = TRUE;
  return TRUE;
}

//---------------------------------------------------------
FLOAT32 MeasureCalcUavr_pd(FLOAT32 value)
{
  return MeasureUavr;
}

//---------------------------------------------------------
void MeasurePowerOn_pd(BYTE mask)
{
}

//---------------------------------------------------------
void MeasurePowerOff_pd(BYTE mask)
{
}
