#ifndef ZEROCALIBRATION_H_INCLUDED
#define ZEROCALIBRATION_H_INCLUDED

/*---------------------------------------------------------
  ZeroCalibration.h
---------------------------------------------------------*/

#include "xmain.h"

void InitZeroCalibration(void);
void ShowZeroCalibrationIni(void);
void ShowZeroCalibration(void);

void SaveZeroCalibration(void);
void LoadZeroCalibration(void);

void LoadFirstProducedZeroCalibration(void);

WORD ZeroCalibrationGetCode(void);

#endif
