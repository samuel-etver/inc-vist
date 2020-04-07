/*---------------------------------------------------------
  Usb.c
---------------------------------------------------------*/

#include "usb.h"
#include "usb_pd.h"

NOINIT TUsbFlags UsbFlags;

//---------------------------------------------------------
void InitUsb(void)
{
  UsbFlags.Address = 0xFF;
  UsbFlags.CableConnected = 0;
  UsbFlags.Connected = 0;
  UsbFlags.DeviceConfigured = 0;

  InitUsb_pd();
}

//---------------------------------------------------------
void SetupUsb(void)
{
  SetupUsb_pd();
}

//---------------------------------------------------------
void DoUsb(void)
{
  DoUsb_pd();
}

//---------------------------------------------------------
BOOL UsbIsInserted(void)
{
  return TO_BOOL(UsbFlags.CableConnected);
}
