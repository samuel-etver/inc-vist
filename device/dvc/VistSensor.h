#ifndef VISTSENSOR_H_INCLUDED
#define VISTSENSOR_H_INCLUDED

/*---------------------------------------------------------
  VistSensor.h
---------------------------------------------------------*/

#include "xmain.h"

void InitVistSensor(void);
void ShowVistSensorIni(void);
void ShowVistSensor(void);

void LoadVistSensor(void);
void SaveVistSensor(void);

void LoadFirstProducedVistSensor(void);

FLOAT32 VistSensorGetK(WORD i);

#endif
