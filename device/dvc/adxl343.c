/*---------------------------------------------------------
  adxl343.c
---------------------------------------------------------*/

#include "adxl343.h"

//---------------------------------------------------------
void Adxl343Init(void)
{
  Adxl343Init_pd();
}

//---------------------------------------------------------
void Adxl343Setup(void)
{
  Adxl343Setup_pd();
}

//---------------------------------------------------------
BOOL Adxl343IsActive(void)
{
  return Adxl343IsActive_pd();
}

//---------------------------------------------------------
BYTE Adxl343ReadStatus(void)
{
  return Adxl343ReadStatus_pd();
}

//---------------------------------------------------------
BOOL Adxl343CheckSlave(void)
{
  return Adxl343CheckSlave_pd();
}
