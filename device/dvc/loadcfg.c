/*---------------------------------------------------------
  loadcfg.c
---------------------------------------------------------*/

#include "loadcfg.h"
#include "language.h"
#include "SupplySource.h"
#include "AutoOff.h"
#include "Theme.h"
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

//---------------------------------------------------------
void LoadCfg(void)
{
  PrgVerb = VB_CHIPCHECK_INI;
  LoadCfgLoad();
}

//---------------------------------------------------------
void LoadCfgLoad(void)
{
  LoadLanguage();
  LoadAutoOff();
  LoadTheme();
  LoadKeybrdSound();
  LoadDeviceType();
  LoadIonCalibration();
  LoadMeasuringTract();
  LoadMeasureHardwareTest();
  LoadZeroCalibration();
  LoadIncParams();
  LoadIncMeasureMode();
  LoadVistSensor();
  LoadVistParams();
}
