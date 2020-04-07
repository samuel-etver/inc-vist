#ifndef PGA113_H_INCLUDED
#define PGA113_H_INCLUDED

/*--------------------------------------------------------
  pga113.h
--------------------------------------------------------*/

#include "xmain.h"
#include "pga113_pd.h"

void Pga113Init(void);
void Pga113On(void);
void Pga113Off(void);
void Pga113SetK(FLOAT32 value);
void Pga113SetKmin(void);
void Pga113SetKmax(void);
BOOL Pga113SetKLower(void);
BOOL Pga113SetKLower2(void);
BOOL Pga113SetKHigher(void);
FLOAT32 Pga113GetK(void);
void Pga113SetKbyIndex(BYTE index);
BYTE Pga113GetIndexMax(void);
FLOAT32 Pga113GetKbyIndex(BYTE index);

extern const FLOAT32 Pga113K[];

#endif
