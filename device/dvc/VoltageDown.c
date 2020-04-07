/*---------------------------------------------------------
  VoltageDown.c
---------------------------------------------------------*/

#include "VoltageDown.h"
#include "lcd.h"
#include "light.h"
#include "keybrd.h"
#include "SupplySource.h"
#include "system.h"
#include "usb.h"

static NOINIT FLOAT32 Voltage;
static NOINIT BYTE VoltageDownClk;
static NOINIT BYTE VoltageDownCnt;

//---------------------------------------------------------
void InitVoltageDown(void)
{
  VoltageDownClk = 0;
  VoltageDownCnt = 0;
  PrgFlags.LowSupplySource = 0;
}

//---------------------------------------------------------
void DoVoltageDown(void)
{
  BOOL f = FALSE;
  
  if(PrgVerb == VB_VOLTAGEDOWN)
    KeyUp = KB_NOKEY;

  if(PrgFlags.CentiSec)
    if(++VoltageDownClk == 50) {
      VoltageDownClk = 0;
      Voltage = SupplySourceVoltage;
      f = TRUE;
    }
  
  if(f == FALSE) return;

  if(PrgVerb != VB_VOLTAGEDOWN) {
    BOOL low_supply_source = FALSE;
    if(Voltage <= 3.4f && UsbIsInserted() == FALSE)
      low_supply_source = TRUE;
    if(low_supply_source != (PrgFlags.LowSupplySource ? TRUE : FALSE)) {
      LcdRepaint();
      PrgFlags.LowSupplySource = low_supply_source == TRUE ? 1 : 0;
    }

    if(UsbIsInserted() == TRUE) return;

    if(Voltage <= 3.2f) {
      if(++VoltageDownCnt == 3) 
        PrgVerb = VB_VOLTAGEDOWN_INI;
    }
    else
      VoltageDownCnt = 0;
  }
}

//---------------------------------------------------------
void ShowVoltageDownIni(void)
{
  BYTE n;
  
  PrgVerb = VB_VOLTAGEDOWN;
  TempWord = 0;
  LightOff();
  
  LcdDrawBegin();
  
  LcdDrawWorkspace();
  
  LcdSetFont(LCD_CAPTION_FONT);

  LcdDrawCaLangText(LCD_W/2, 50, 122);
  
  n = LoadLangResource(LcdText, 227);
  SupplySourceVoltageToStr(LcdText + 4, Voltage);
  LcdDrawCaText(LCD_W/2, 120, LcdText, n);
  
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowVoltageDown(void)
{
  if(PrgFlags.CentiSec)
    if(++TempWord == 100)
      Halt();
}
