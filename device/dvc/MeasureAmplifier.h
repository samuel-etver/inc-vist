#ifndef MEASUREAMPLIFIER_H_INCLUDED
#define MEASUREAMPLIFIER_H_INCLUDED

/*---------------------------------------------------------
  MeasureAmplifier.h
---------------------------------------------------------*/

#include "xmain.h"
#include "pga113.h"

#define MEASUREAMPLIFIER_GET_KI(i)    PGA113_GET_KI(i)
#define MEASUREAMPLIFIER_GET_K()      PGA113_GET_K()
#define MEASUREAMPLIFIER_GET_INDEX()  PGA113_GET_INDEX()

void MeasureAmplifierInit(void);
void MeasureAmplifierSetOn(void);
void MeasureAmplifierSetOff(void);
void MeasureAmplifierSetK(FLOAT32 value);
void MeasureAmplifierSetKmin(void);
void MeasureAmplifierSetKmax(void);
BOOL MeasureAmplifierSetKLower(void);
BOOL MeasureAmplifierSetKLower2(void);
BOOL MeasureAmplifierSetKHigher(void);
FLOAT32 MeasureAmplifierGetK(void);
FLOAT32 MeasureAmplifierGetKmin(void);
FLOAT32 MeasureAmplifierGetKmax(void);
void MeasureAmplifierSetKbyIndex(BYTE index);
FLOAT32 MeasureAmplifierGetKbyIndex(BYTE index);
BYTE MeasureAmplifierGetIndexMax(void);

#endif
