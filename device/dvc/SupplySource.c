/*---------------------------------------------------------
  SupplySource.h
---------------------------------------------------------*/

#include "SupplySource.h"
#include "SupplySource_pd.h"
#include "measure_pd.h"
#include "lcd.h"
#include "keybrd.h"
#include "language.h"
#include "cfg.h"
#include "system.h"
#include "adc.h"
#include "utils.h"
#include "usb.h"
#include "AutoOff.h"
#include "Accelerometer.h"
#include "delay.h"

NOINIT FLOAT32 SupplySourceVoltage;
NOINIT BOOL SupplySourceMeasureDone;
static NOINIT BYTE SupplySourceMeasureStage;
static NOINIT TDelay SupplySourceDelay;

static void SupplySourceMeasureOn(void);
static void SupplySourceMeasureOff(void);

//---------------------------------------------------------
void InitSupplySource(void)
{
  InitSupplySource_pd();
  PrgFlags.Charging = 0;
  SupplySourceMeasureStage = 0;
  SupplySourceMeasureDone = FALSE;
  SupplySourceMeasureOff();
  SupplySourceVoltage = 10.0f;
}

//---------------------------------------------------------
void DoSupplySource(void)
{
  switch(SupplySourceMeasureStage) {
    case 0:
      SupplySourceDelay = DelayBegin();
      SupplySourceMeasureStage++;
    case 1:
      if(DelayIsEnded(SupplySourceDelay, SYS_CONVERT_MCS(100000)) == TRUE) {
        SupplySourceMeasureStage = 20;
      }
      break;
      
    case 10:
      SupplySourceDelay = DelayBegin();
      SupplySourceMeasureStage++;
    case 11:
      if(DelayIsEnded(SupplySourceDelay, SYS_CONVERT_MCS(10000000)) == TRUE) {
        SupplySourceMeasureStage = 20;
      }
      break;
    
    case 20:
      SupplySourceMeasureOn();
      SupplySourceDelay = DelayBegin();
      SupplySourceMeasureStage++;
    case 21:
      if(DelayIsEnded(SupplySourceDelay, SYS_CONVERT_MCS(500000)) == TRUE) {        
        SupplySourceVoltage = GetSupplySourceVoltage_pd();
        SupplySourceMeasureOff();
        SupplySourceMeasureDone = TRUE;
        SupplySourceMeasureStage = 10;
      }
  }
  
  
  
  if(PrgFlags.CentiSec) {
    PrgFlags.Charging = 0;

    if(UsbIsInserted() == TRUE) {
      if(SupplySourceIsCharging() == TRUE) {
        PrgFlags.Charging = 1;
        ClearAutoOff();
      }
    }
  }
}

//---------------------------------------------------------
void ShowSupplySourceIni(void)
{
  PrgVerb = VB_SUPPLYSOURCE;

  LcdDrawBegin();

  LcdDrawWorkspaceWithLangCaption(87);

  LcdSetFontDef();
  LcdSetTextColorDef();
  LcdDrawCaLangText(LCD_W/2, 140, 15);

  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowSupplySource(void)
{
  ProcessStandardKeyDownActions();

  if(LcdFlags.Draw) {
    BYTE n;
    TLcdRect rect;

    LcdDrawBegin();

    rect.Left   = 0;
    rect.Right  = LCD_W - 1;
    rect.Bottom = (rect.Top = 180) + LcdFontHeight;
    n = LoadLangResource(LcdText, 227);
    SupplySourceVoltageToStr(LcdText + 4, SupplySourceVoltage);
    LcdDrawCaRectText(&rect, 0, LcdText, n);

    LcdDrawEnd();
  }
}

//---------------------------------------------------------
void SupplySourceVoltageToStr(BYTE* buff, FLOAT32 voltage)
{
  WordToStrAsFloatWithLeadingSpaces(buff,
   voltage < 0 ? 0 : (WORD)(100.0f*voltage), 4, 2);
}

//---------------------------------------------------------
TSupplySource SupplySourceGet(void)
{
  return ssAccumulator;
}

//---------------------------------------------------------
FLOAT32 GetSupplySourceChargeLevel(void)
{
  FLOAT32 level = SupplySourceVoltage*(1.00f - 0.01f)/(4.2f - 3.3f) +
   (0.01f - 3.3f*(1.00f - 0.01f)/(4.2f - 3.3f));

  if(level < 0.01f) return 0.01f;
  if(level > 1.00f) return 1.00f;

  return level;
}

//---------------------------------------------------------
BOOL SupplySourceIsCharging(void)
{
  return SupplySourceIsCharging_pd();
}

//---------------------------------------------------------
static void SupplySourceMeasureOn(void)
{
  MeasurePowerOn_pd(BIT1);
}

//---------------------------------------------------------
static void SupplySourceMeasureOff(void)
{
  MeasurePowerOff_pd(BIT1);
}
