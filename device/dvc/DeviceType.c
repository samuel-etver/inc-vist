/*---------------------------------------------------------
  DeviceType.c
---------------------------------------------------------*/

#include "DeviceType.h"
#include "lcd.h"
#include "cfg.h"
#include "menu.h"

NOINIT TDeviceType DeviceType;

static const WORD DeviceTypeResIds[] = {
  793, 791, 795,
  TEXTRESOURCE_EOF
};

//---------------------------------------------------------
void InitDeviceType(void)
{
  LoadFirstProducedDeviceType();
}

//---------------------------------------------------------
void ShowDeviceTypeIni(void)
{
  PrgVerb = VB_DEVICETYPE;
  TempByte = (BYTE)DeviceType;

  LcdDrawBegin();
  LcdDrawWorkspaceWithLangCaption(787);
  LcdMenuFactoryCreateEasy(DeviceTypeResIds, (BYTE)DeviceType);
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowDeviceType(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    DeviceType = (TDeviceType)LcdMenu.Selected;
    if((BYTE)DeviceType != TempByte) {
      SaveDeviceType();
      MenuDeviceTypeChanged();
    }
  }

  LcdMenuDo(NULL);
}

//---------------------------------------------------------
void LoadDeviceType(void)
{
  BYTE b = CfgReadByte(CFG_DEVICETYPE);
  if(b < DEVICETYPE_COUNT)
    DeviceType = (TDeviceType)b;
}

//---------------------------------------------------------
void SaveDeviceType(void)
{
  CfgWriteByte(CFG_DEVICETYPE, (BYTE)DeviceType);
}

//---------------------------------------------------------
void LoadFirstProducedDeviceType(void)
{
  DeviceType = dtIncVist;
}

