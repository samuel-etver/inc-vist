#ifndef INCKMEASUREMODE_H_INCLUDED
#define INCKMEASUREMODE_H_INCLUDED

/*---------------------------------------------------------
  IncMeasureMode.h
---------------------------------------------------------*/

#include "xmain.h"

void InitIncMeasureMode(void);
void ShowIncMeasureModeIni(void);
void ShowIncMeasureMode(void);

void LoadIncMeasureMode(void);
void SaveIncMeasureMode(void);

void LoadFirstProducedIncMeasureMode(void);

typedef enum {
  incMmInc = 0, incMmVist, incMmLast
} TIncMeasureMode;

#define INC_MEASUREMODE_COUNT           ((BYTE)incMmLast)

extern NOINIT TIncMeasureMode IncMeasureMode;

#endif
