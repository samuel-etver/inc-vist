#ifndef ACCELEROMETER_H_INCLUDED
#define ACCELEROMETER_H_INCLUDED

/*---------------------------------------------------------
  Accelerometer.h
---------------------------------------------------------*/

#include "xmain.h"

void InitAccelerometer(void);
void SetupAccelerometer(void);
void DoAccelerometer(void);
BOOL AccelerometerCheckSlave(void);

extern NOINIT BOOL AccelerometerActive;

#endif
