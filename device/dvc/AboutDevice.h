#ifndef ABOUTDEVICE_H_INCLUDED
#define ABOUTDEVICE_H_INCLUDED

/*---------------------------------------------------------
  AboutDevice.h
---------------------------------------------------------*/

#include "xmain.h"

void ShowAboutDeviceIni(void);
void ShowAboutDevice(void);

void AboutDrawDevice(BYTE y);

BYTE AboutGetDeviceNameStr(BYTE* buff);


#endif
