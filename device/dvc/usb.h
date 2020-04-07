#ifndef USB_H_INCLUDED
#define USB_H_INCLUDED

/*---------------------------------------------------------
  Usb.h
---------------------------------------------------------*/

#include "xmain.h"

void InitUsb(void);
void SetupUsb(void);
void DoUsb(void);

typedef struct {
  BYTE Address;
	BIT  CableConnected: 1;
  BIT  Connected: 1;
  BIT  DeviceConfigured: 1;
} TUsbFlags;

extern NOINIT TUsbFlags UsbFlags;

BOOL UsbIsInserted(void);

#endif
