/*---------------------------------------------------------
  about.c
---------------------------------------------------------*/

#include "AboutManufacturer.h"
#include "AboutDevice.h"
#include "lcd.h"
#include <string.h>

//---------------------------------------------------------
void ShowAboutManufacturerIni(void)
{
  PrgVerb = VB_ABOUTMANUFACTURER;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(FONT08);
  LcdDrawCaLangText(LCD_W/2, 56, 117);
  LcdSetTextColor(LcdWebLinkColor);
  LcdDrawCaResText(LCD_W/2, 72, 442);
  LcdSetFont(FONT16);
  LcdSetTextColor(LcdTextColorDef);
  LcdDrawCaText(LCD_W/2, 98, LcdText, AboutGetDeviceNameStr(LcdText));
  LcdSetFont(FONT10);
  LcdSetTextColor(LcdEvenRowTextColorDef);
  strcpy((char*)LcdText + LoadLangResource(LcdText, 318), SOFTWARE_VERSION);
  LcdDrawCaText(LCD_W/2, 136, LcdText, (BYTE)strlen((char*)LcdText));
  
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowAboutManufacturer(void)
{
  ProcessStandardKeyDownActions();
}
