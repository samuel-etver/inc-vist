/*---------------------------------------------------------
  FirstProduced.h
---------------------------------------------------------*/

#include "FirstProduced.h"
#include "lcd.h"
#include "language.h"
#include "keybrd.h"
#include "mem.h"
#include "sound.h"
#include "Ask.h"
#include "Autooff.h"
#include "SupplySource.h"
#include "Theme.h"
#include "menu.h"
#include "KeybrdSound.h"

#include "DeviceType.h"
#include "IonCalibration.h"
#include "MeasuringTract.h"
#include "MeasureHardwareTest.h"
#include "ZeroCalibration.h"
#include "IncParams.h"
#include "IncMeasureMode.h"
#include "VistSensor.h"
#include "VistParams.h"

static void FirstProducedSet(void);

//---------------------------------------------------------
void ShowFirstProducedIni(void)
{
  PrgVerb = VB_FIRSTPRODUCED;

  TempWord = 488;
  TempByte = 1;

  ShowAskIni();
}

//---------------------------------------------------------
void ShowFirstProduced(void)
{
  ShowAsk();

  if(KeyDown == KB_MEASURE || KeyDown == KB_MENU)
    if(TempBool == TRUE)
      FirstProducedSet();
}

//---------------------------------------------------------
static void FirstProducedSet(void)
{
  LcdDrawBegin();
  LcdDrawWorkspace();
  LcdDrawCaLangText(LCD_W/2, LCD_H/2 - LcdFontHeight/2, 484);
  LcdDrawEnd();

  LoadFirstProducedLanguage();
  LoadFirstProducedAutoOff();
  LoadFirstProducedTheme();
  LoadFirstProducedKeybrdSound();
  LoadFirstProducedDeviceType();
  LoadFirstProducedIonCalibration();
  LoadFirstProducedMeasuringTract();
  LoadFirstProducedMeasureHardwareTest();
  LoadFirstProducedZeroCalibration();
  LoadFirstProducedIncParams();
  LoadFirstProducedIncMeasureMode();
  LoadFirstProducedVistSensor();
  LoadFirstProducedVistParams();

  while(KeyFlags.PlayingClick) DoSound();

  MemClear();

  SaveLanguage();
  SaveKeybrdSound();
  SaveAutoOff();
  SaveDeviceType();
  SaveIonCalibration();
  SaveMeasureHardwareTest();
  SaveMeasuringTract();
  SaveZeroCalibration();
  SaveIncParams();
  SaveIncMeasureMode();
  SaveVistSensor();
  SaveVistParams();

  ThemeChange();
  MenuDeviceTypeChanged();
}
