#ifndef DEVICETYPE_H_INCLUDED
#define DEVICETYPE_H_INCLUDED

/*---------------------------------------------------------
  DeviceType.h
---------------------------------------------------------*/

#include "xmain.h"

void InitDeviceType(void);
void ShowDeviceTypeIni(void);
void ShowDeviceType(void);

void LoadDeviceType(void);
void SaveDeviceType(void);

void LoadFirstProducedDeviceType(void);

typedef enum {
  dtInc = 0, dtIncVist, dtVist, dtLast
} TDeviceType;

#define DEVICETYPE_COUNT    ((BYTE)dtLast)

extern NOINIT TDeviceType DeviceType;

#endif
