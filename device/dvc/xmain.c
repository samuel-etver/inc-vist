/*---------------------------------------------------------
  main.c
---------------------------------------------------------*/

#include "xmain.h"
#include "system.h"
#include "power.h"
#include "adc.h"
#include "iic.h"
#include "keybrd.h"
#include "lcd.h"
#include "sound.h"
#include "light.h"
#include "ManualOff.h"
#include "loadcfg.h"
#include "splash.h"
#include "menu.h"
#include "measure.h"
#include "language.h"
#include "calendar.h"
#include "SupplySource.h"
#include "VoltageDown.h"
#include "AboutDevice.h"
#include "AboutManufacturer.h"
#include "AutoOff.h"
#include "password.h"
#include "FlashDump.h"
#include "usb.h"
#include "FirstProduced.h"
#include "ChipCheck.h"
#include "Status.h"
#include "Theme.h"
#include "KeybrdSound.h"
#include "Accelerometer.h"
#include "DeviceType.h"
#include "IonCalibration.h"
#include "MeasuringTract.h"
#include "MeasureHardwareTest.h"
#include "ZeroCalibration.h"
#include "IncParams.h"
#include "IncMemory.h"
#include "IncClearMem.h"
#include "IncArchive.h"
#include "IncTest.h"
#include "IncMeasureMode.h"
#include "VistMemory.h"
#include "VistClearMem.h"
#include "VistArchive.h"
#include "VistTest.h"
#include "VistSensor.h"
#include "VistParams.h"
#include "VistCalibration.h"

NOINIT BOOL TempBool;
NOINIT BYTE TempByte;
NOINIT WORD TempWord;
NOINIT DWORD TempDword;
NOINIT INT8 TempInt8;
NOINIT FLOAT32 TempFloat;
NOINIT void* TempPtr;
NOINIT BYTE TempBuff[TEMP_BUFF_SIZE];
NOINIT BOOL DebugBool;
NOINIT INT32 DebugInt32;
NOINIT FLOAT32 DebugFloat;
NOINIT TProgramVerb PrgVerb;
NOINIT TProgramFlags PrgFlags;

static void DoVerb(void);

//---------------------------------------------------------
void xmain(void)
{
  MainInit();
  MainSetup();
  for(;;)
    MainDo();
}

//---------------------------------------------------------
void MainInit(void)
{
  PrgVerb = VB_CHIPCHECK_INI;
  InitSystem();
  InitPower();
  InitMenu();
  InitMeasure();
  InitLanguage();
  InitStatus();
  InitFlashDump();
  InitCalendar();
  InitSupplySource();
  InitVoltageDown();
  InitChipCheck();
  InitPassword();
  InitTheme();
  InitKeybrdSound();

  InitDeviceType();
  InitIonCalibration();
  InitMeasuringTract();
  InitMeasureHardwareTest();
  InitZeroCalibration();
  InitIncParams();
  InitIncMeasureMode();
  InitVistSensor();
  InitVistParams();
  InitVistCalibration();

  InitManualOff();
  InitAutoOff();
  InitAdc();
  InitKeybrd();
  InitSound();
  InitIic();
  InitUsb();
  InitLight();
  InitAccelerometer();
  InitLcd();
  InitLight();
}

//---------------------------------------------------------
void MainSetup(void)
{
  SetFastFrequency();
  SetupCalendar();
  SetupLcd();
  SetupLight();
  SetupKeybrd();
  SetupAccelerometer();
  SetupUsb();
}

//---------------------------------------------------------
void MainDo(void)
{
  DoTicks();
  DoKeybrd();
  DoSound();
  DoManualOn();
  DoManualOff();
  DoAccelerometer();
  DoSupplySource();
  DoAutoOff();
  DoLight();
  DoLcd();
  DoVerb();
  DoStatus();
  DoVoltageDown();
  DoUsb();
}

//---------------------------------------------------------
static void DoVerb(void)
{
  switch(PrgVerb) {
    case VB_LOADCFG:
      LoadCfg(); break;
    case VB_CHIPCHECK_INI:
      ShowChipCheckIni();
    case VB_CHIPCHECK:
      ShowChipCheck(); break;
    case VB_SPLASH_INI:
      ShowSplashIni();
    case VB_SPLASH:
      ShowSplash(); break;
    case VB_MENU_INI:
      ShowMenuIni();
    case VB_MENU:
      ShowMenu(); break;
    case VB_MEASURE_INI:
      ShowMeasureIni();
    case VB_MEASURE:
      ShowMeasure(); break;
    case VB_LANGUAGE_INI:
      ShowLanguageIni();
    case VB_LANGUAGE:
      ShowLanguage(); break;
    case VB_CALENDAR_INI:
      ShowCalendarIni();
    case VB_CALENDAR:
      ShowCalendar(); break;
    case VB_SUPPLYSOURCE_INI:
      ShowSupplySourceIni();
    case VB_SUPPLYSOURCE:
      ShowSupplySource(); break;
    case VB_VOLTAGEDOWN_INI:
      ShowVoltageDownIni();
    case VB_VOLTAGEDOWN:
      ShowVoltageDown(); break;
    case VB_ABOUTDEVICE_INI:
      ShowAboutDeviceIni();
    case VB_ABOUTDEVICE:
      ShowAboutDevice(); break;
    case VB_ABOUTMANUFACTURER_INI:
      ShowAboutManufacturerIni();
    case VB_ABOUTMANUFACTURER:
      ShowAboutManufacturer(); break;
    case VB_PASSWORD_INI:
      ShowPasswordIni();
    case VB_PASSWORD:
      ShowPassword(); break;
    case VB_FLASHDUMP_INI:
      ShowFlashDumpIni();
    case VB_FLASHDUMP:
      ShowFlashDump(); break;
    case VB_FIRSTPRODUCED_INI:
      ShowFirstProducedIni();
    case VB_FIRSTPRODUCED:
      ShowFirstProduced(); break;
    case VB_THEME_INI:
      ShowThemeIni();
    case VB_THEME:
      ShowTheme(); break;
    case VB_KEYBRDSOUND_INI:
      ShowKeybrdSoundIni();
    case VB_KEYBRDSOUND:
      ShowKeybrdSound(); break;
    case VB_AUTOOFF_INI:
      ShowAutoOffIni();
    case VB_AUTOOFF:
      ShowAutoOff(); break;
    case VB_DEVICETYPE_INI:
      ShowDeviceTypeIni();
    case VB_DEVICETYPE:
      ShowDeviceType(); break;
    case VB_IONCALIBRATION_INI:
      ShowIonCalibrationIni();
    case VB_IONCALIBRATION:
      ShowIonCalibration(); break;
    case VB_MEASURINGTRACT_INI:
      ShowMeasuringTractIni();
    case VB_MEASURINGTRACT:
      ShowMeasuringTract(); break;
    case VB_MEASUREHARDWARETEST_INI:
      ShowMeasureHardwareTestIni();
    case VB_MEASUREHARDWARETEST:
      ShowMeasureHardwareTest(); break;
    case VB_ZEROCALIBRATION_INI:
      ShowZeroCalibrationIni();
    case VB_ZEROCALIBRATION:
      ShowZeroCalibration(); break;

    case VB_INC_PARAMS_INI:
      ShowIncParamsIni();
    case VB_INC_PARAMS:
      ShowIncParams(); break;
    case VB_INC_MEMORY_INI:
      ShowIncMemoryIni();
    case VB_INC_MEMORY:
      ShowIncMemory(); break;
    case VB_INC_CLEARMEM_INI:
      ShowIncClearMemIni();
    case VB_INC_CLEARMEM:
      ShowIncClearMem(); break;
    case VB_INC_ARCHIVE_INI:
      ShowIncArchiveIni();
    case VB_INC_ARCHIVE:
      ShowIncArchive(); break;
    case VB_INC_TEST_INI:
      ShowIncTestIni();
    case VB_INC_TEST:
      ShowIncTest(); break;
    case VB_INC_MEASUREMODE_INI:
      ShowIncMeasureModeIni();
    case VB_INC_MEASUREMODE:
      ShowIncMeasureMode(); break;

    case VB_VIST_MEMORY_INI:
      ShowVistMemoryIni();
    case VB_VIST_MEMORY:
      ShowVistMemory(); break;
    case VB_VIST_CLEARMEM_INI:
      ShowVistClearMemIni();
    case VB_VIST_CLEARMEM:
      ShowVistClearMem(); break;
    case VB_VIST_ARCHIVE_INI:
      ShowVistArchiveIni();
    case VB_VIST_ARCHIVE:
      ShowVistArchive(); break;
    case VB_VIST_TEST_INI:
      ShowVistTestIni();
    case VB_VIST_TEST:
      ShowVistTest(); break;
    //case VB_VIST_SENSOR_INI:
    //  ShowVistSensorIni();
    //case VB_VIST_SENSOR:
    //  ShowVistSensor(); break;
    case VB_VIST_PARAMS_INI:
      ShowVistParamsIni();
    case VB_VIST_PARAMS:
      ShowVistParams(); break;
    case VB_VIST_CALIBRATION_INI:
      ShowVistCalibrationIni();
    case VB_VIST_CALIBRATION:
      ShowVistCalibration(); break;

    default:;
  }
}

//---------------------------------------------------------
BOOL ProcessStandardKeyDownActions(void)
{
  if(KeyDown == KB_MEASURE) {
    PrgVerb = VB_MEASURE_INI;
    return TRUE;
  }

  if(KeyDown == KB_MENU) {
    PrgVerb = VB_MENU_INI;
    return TRUE;
  }

  return FALSE;
}
