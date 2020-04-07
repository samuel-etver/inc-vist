/*---------------------------------------------------------
  EndSensors_pd.c
---------------------------------------------------------*/

#include "EndSensors_pd.h"

BOOL EndSensorsUp = FALSE;
BOOL EndSensorsDown = FALSE;

//---------------------------------------------------------
void EndSensorsMeasureOnEnter_pd(void)
{
}

//---------------------------------------------------------
BOOL EndSensorsIsUp_pd(void)
{
  return EndSensorsUp;
}

//---------------------------------------------------------
BOOL EndSensorsIsDown_pd(void)
{
  return EndSensorsDown;
}

