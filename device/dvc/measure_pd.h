#ifndef MEASURE_DVC_H_INCLUDED
#define MEASURE_DVC_H_INCLUDED

/*---------------------------------------------------------
  measure_dvc.h
---------------------------------------------------------*/

#include "xmain.h"

void MeasureInit_pd(void);
void MeasureOnEnter_pd(void);
void MeasureOnExit_pd(void);
void MeasureSetSon_pd(void);
void MeasureSetVon_pd(void);
void MeasureSetAon_pd(void);
BOOL MeasureCalcT_pd(FLOAT32* data);
FLOAT32 MeasureCalcUavr_pd(FLOAT32 value);
void MeasurePowerOn_pd(BYTE mask);
void MeasurePowerOff_pd(BYTE mask);

#ifdef WIN32
extern NOINIT FLOAT32 MeasureT_pd;
extern NOINIT FLOAT32 MeasureNoise_pd;
#endif

#endif
