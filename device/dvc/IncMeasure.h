#ifndef INCMEASURE_H_INCLUDED
#define INCMEASURE_H_INCLUDED

/*---------------------------------------------------------
  IncMeasure.h
---------------------------------------------------------*/

#include "xmain.h"
#include "lcd.h"
#include "utils.h"

void ShowIncMeasureIni(void);
void ShowIncMeasure(void);

void IncMeasureDrawLength(WORD length, BOOL redraw);
void IncMeasureDrawDiameter(BYTE diameter, BOOL redraw);
void IncMeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw);
void IncMeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw);
void IncMeasureDrawSigma(WORD y, FLOAT32 sigma, BOOL f, BOOL redraw);
void IncMeasureDrawNoise(WORD y, FLOAT32 noise, BOOL f, BOOL redraw);
void IncMeasureReset(void);
void IncMeasureDrawUnits(WORD y);
void IncMeasureDrawDeltaL(WORD y, FLOAT32 delta, BOOL f, BOOL redraw);
void IncMeasureDrawEpsilon(WORD y, FLOAT32 epsilon, BOOL f, BOOL redraw);

extern NOINIT FLOAT32 IncMeasureSigma;
extern NOINIT FLOAT32 IncMeasureDeltaL;
extern NOINIT FLOAT32 IncMeasureEpsilon;
extern NOINIT TTrigger IncMeasureSigmaTrigger;
extern NOINIT TLcdRect IncMeasureLengthRect;
extern NOINIT TLcdRect IncMeasureDiameterRect;
extern NOINIT TLcdRect IncMeasureSigmaRect;

#endif
