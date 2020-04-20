/*---------------------------------------------------------
  splash.c
---------------------------------------------------------*/

#include "splash.h"
#include "lcd.h"
#include "SupplySource.h"
#include "AboutDevice.h"

//---------------------------------------------------------
void ShowSplashIni(void)
{
  TempWord = 0;

  PrgVerb = VB_SPLASH;

  LcdApplyTheme();

  LcdDrawBegin();

  LcdDrawBackground();

  AboutDrawDevice(24);

  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowSplash(void)
{
  BYTE n;
  int charge_level;

  if(PrgFlags.CentiSec)
    switch(++TempWord) {
      case 15:
        if(SupplySourceMeasureDone != TRUE) {
          TempWord--;
          break;
        }
        LcdApplyTheme();
        LcdDrawBegin();
        LcdSetFont(LCD_ITEM_FONT);
        charge_level = (int)(100.0f*GetSupplySourceChargeLevel());
        n = (BYTE)sprintf((char*)LcdText, (const char*)GetLangResource(229),
         (int)charge_level);
        LcdDrawCaText(LCD_W/2, 160, LcdText, n);
        LcdDrawEnd();
        break;
      case 30:
        PrgVerb = VB_MENU_INI;
  }
}
