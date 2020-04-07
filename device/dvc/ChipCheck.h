#ifndef CHIPCHECK_H_INCLUDED
#define CHIPCHECK_H_INCLUDED

/*---------------------------------------------------------
  ChipCheck.h
---------------------------------------------------------*/

#include "xmain.h"

void InitChipCheck(void);
void ShowChipCheckIni(void);
void ShowChipCheck(void);

typedef struct {
  BIT FlashAvailable:         1;
  BIT AccelerometerAvailable: 1;
} TChipCheckFlags;

extern NOINIT TChipCheckFlags ChipCheckFlags;

#endif
