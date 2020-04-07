#ifndef FACTOR_H_INCLUDED
#define FACTOR_H_INCLUDED

/*---------------------------------------------------------
  Factor.h
---------------------------------------------------------*/

#include "xmain.h"

FLOAT32 FactorToFloat(BYTE* factor, BYTE len, BYTE comma);
void FactorFromFloat(BYTE* factor, BYTE len, BYTE comma, FLOAT32 value);
BYTE FactorToStr(BYTE* buff, BYTE* factor, BYTE len, BYTE comma);

#endif
