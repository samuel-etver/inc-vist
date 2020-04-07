#ifndef INCPARAMS_H_INCLUDED
#define INCPARAMS_H_INCLUDED

/*---------------------------------------------------------
  IncParams.h
---------------------------------------------------------*/

#include "xmain.h"

void InitIncParams(void);
void ShowIncParamsIni(void);
void ShowIncParams(void);

void LoadIncParams(void);
void SaveIncParams(void);

void LoadFirstProducedIncParams(void);

BYTE IncParamsLengthToStr(BYTE* buff, WORD length);
BYTE IncParamsDiameterToStr(BYTE* buff, BYTE diameter);
BYTE IncParamsTensionToStr(BYTE* buff, WORD tension);
BYTE IncParamsUnitsToStr(BYTE* buff, BYTE units);

extern NOINIT WORD IncParamsLength;
extern NOINIT BYTE IncParamsDiameter;
extern NOINIT WORD IncParamsTension;
extern NOINIT BYTE IncParamsUnits;

#endif
