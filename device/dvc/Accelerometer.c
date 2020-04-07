/*---------------------------------------------------------
  Accelerometer.c
---------------------------------------------------------*/

#include "Accelerometer.h"
#include "adxl343.h"
#include "ChipCheck.h"

NOINIT BOOL AccelerometerActive;
static NOINIT BYTE AccelerometerTicks;

//---------------------------------------------------------
void InitAccelerometer(void)
{
  Adxl343Init();
  AccelerometerTicks = 0;
  AccelerometerActive = TRUE;
}

//---------------------------------------------------------
void SetupAccelerometer(void)
{
  if(AccelerometerCheckSlave() == TRUE) {
    ChipCheckFlags.AccelerometerAvailable = 1;
    Adxl343Setup();
  }
}

//---------------------------------------------------------
void DoAccelerometer(void)
{
  if(!PrgFlags.CentiSec) return;
  if(PrgVerb < VB_MENU) return;
  if(!ChipCheckFlags.AccelerometerAvailable) return;

  if(++AccelerometerTicks == 5) {
    AccelerometerTicks = 0;
    switch(PrgVerb) {
      case VB_MEASURE:
      case VB_INC_TEST:
      case VB_VIST_TEST:
      case VB_MEASURINGTRACT:
      case VB_MEASUREHARDWARETEST:
      case VB_VIST_CALIBRATION:
        AccelerometerActive = TRUE;
        break;
      default:
        AccelerometerActive = Adxl343IsActive();
    }
  }
}

//---------------------------------------------------------
BOOL AccelerometerCheckSlave(void)
{
  return Adxl343CheckSlave();
}
