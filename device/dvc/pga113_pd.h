#ifndef PGA113_PD_H_INCLUDED
#define PGA113_PD_H_INCLUDED

/*---------------------------------------------------------
  pga113_pd.h
---------------------------------------------------------*/

#include "xmain.h"
#include "pga113.h"

#define PGA113_GET_KI(i)    Pga113K[i]
#define PGA113_GET_K()      Pga113K[Pga113Index]
#define PGA113_GET_INDEX()  Pga113Index

void Pga113Init_pd(void);
void Pga113On_pd(void);
void Pga113Off_pd(void);
void Pga113SetK_pd(FLOAT32 value);
void Pga113SetKmin_pd(void);
void Pga113SetKmax_pd(void);
BOOL Pga113SetKLower_pd(void);
BOOL Pga113SetKLower2_pd(void);
BOOL Pga113SetKHigher_pd(void);
FLOAT32 Pga113GetK_pd(void);
void Pga113SetKbyIndex_pd(BYTE index);

extern NOINIT WORD Pga113Index;

#endif
