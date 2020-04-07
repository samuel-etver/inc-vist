#ifndef IONCALIBRATION_H_INCLUDED
#define IONCALIBRATION_H_INCLUDED

/*---------------------------------------------------------
  IonCalibration.h
---------------------------------------------------------*/

#include "xmain.h"

#define IONCALIBRATION_NOM            2500

void InitIonCalibration(void);
void ShowIonCalibrationIni(void);
void ShowIonCalibration(void);

void LoadIonCalibration(void);
void SaveIonCalibration(void);

void LoadFirstProducedIonCalibration(void);

FLOAT32 IonCalibrationGet(void);

#endif
