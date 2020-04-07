/*---------------------------------------------------------
  AboutDevice.c
---------------------------------------------------------*/

#include "AboutDevice.h"
#include "lcd.h"
#include "utils.h"
#include "DeviceType.h"

//---------------------------------------------------------
void ShowAboutDeviceIni(void)
{
  BYTE n;

  PrgVerb = VB_ABOUTDEVICE;

  LcdDrawBegin();

  LcdDrawWorkspace();

  n = LoadLangResource(LcdText, 461);
  WordToStrWithLeadingZeroes(LcdText + n, PRODUCT_MODEL, 2);
  LcdDrawCaText(LCD_W/2, 120, LcdText, n + 2);

  LcdSetTextColor(LCD_RGB_TO_COLOR(188, 0, 255));
  LcdDrawCaLangText(LCD_W/2, 160, 463);
  LcdDrawCaText(LCD_W/2, 160 + LcdFontHeight + 2, (BYTE*)BUILD_DATE_STR,
   BUILD_DATE_STR_LEN);

  AboutDrawDevice(24);

  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowAboutDevice(void)
{
  ProcessStandardKeyDownActions();
}

//---------------------------------------------------------
void AboutDrawDevice(BYTE y)
{
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdSetFont(LCD_CAPTION_FONT);
  LcdDrawCaText(LCD_W/2, y + 30, LcdText, AboutGetDeviceNameStr(LcdText));
}

//---------------------------------------------------------
BYTE AboutGetDeviceNameStr(BYTE* buff)
{
  WORD id;
  
  switch(DeviceType) {
    case dtInc:     id = 793; break;
    case dtIncVist: id = 791; break;
    case dtVist:    id = 795; break;
    default:        id = 0;
  }
  
  return LoadLangResource(buff, id);
}
