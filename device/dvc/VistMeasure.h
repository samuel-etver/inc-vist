#ifndef VISTMEASURE_H_INCLUDED
#define VISTMEASURE_H_INCLUDED

/*---------------------------------------------------------
  VistMeasure.h
---------------------------------------------------------*/

#include "xmain.h"
#include "VistParams.h"

void ShowVistMeasureIni(void);
void ShowVistMeasure(void);

void VistMeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw);
void VistMeasureDrawS(WORD y, FLOAT32 s, BOOL f, BOOL redraw);
void VistMeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw);
void VistMeasureDrawA(WORD y, FLOAT32 v, BOOL f, BOOL redraw);
void VistMeasureDrawNoise(WORD y, FLOAT32 noise, BOOL f, BOOL redraw);
void VistMeasureDrawObject(BYTE o, BOOL redraw);
void VistMeasureSetType(void);
void VistMeasureDrawType(WORD y, TVistParamsType param);

#endif
