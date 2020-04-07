#ifndef MEASURINGTRACT_H_INCLUDED
#define MEASURINGTRACT_H_INCLUDED

/*---------------------------------------------------------
  MeasuringTract.h
---------------------------------------------------------*/

#include "xmain.h"

void InitMeasuringTract(void);
void ShowMeasuringTractIni(void);
void ShowMeasuringTract(void);

void LoadMeasuringTract(void);
void SaveMeasuringTract(void);

void LoadFirstProducedMeasuringTract(void);

BYTE MeasuringTractAxToStr(BYTE* buff, BYTE* factor);
void MeasuringTractCalc(void);

#define MEASURINGTRACT_X_FACTOR_COMMA     3U
#define MEASURINGTRACT_X_FACTOR_SIZE      (5U + MEASURINGTRACT_X_FACTOR_COMMA)

#define MEASURINGTRACT_X_FACTORS_COUNT    2

extern NOINIT WORD MeasuringTractFs;
extern NOINIT FLOAT32 MeasuringTractAs[MEASURINGTRACT_X_FACTORS_COUNT];
extern NOINIT FLOAT32 MeasuringTractAv[MEASURINGTRACT_X_FACTORS_COUNT];
extern NOINIT FLOAT32 MeasuringTractAa[MEASURINGTRACT_X_FACTORS_COUNT];
#define COUNT MEASURINGTRACT_X_FACTORS_COUNT
#define SIZE  MEASURINGTRACT_X_FACTOR_SIZE
extern NOINIT BYTE MeasuringTractAsFactor[COUNT][SIZE];
extern NOINIT BYTE MeasuringTractAvFactor[COUNT][SIZE];
extern NOINIT BYTE MeasuringTractAaFactor[COUNT][SIZE];
#undef COUNT
#undef SIZE

#endif
