/*---------------------------------------------------------
  Tunes.c
---------------------------------------------------------*/

#include "Tunes.h"
#include "lcd.h"
#include "keybrd.h"
#include "measure.h"
#include "Sensor.h"
#include "utils.h"

//---------------------------------------------------------
BYTE TunesItemToStr(BYTE* buff, FLOAT32 value)
{
  BYTE n;
  
  if(value < 1000.0f)
    n = (BYTE)sprintf((char*)buff, "%7.3f", value);
  else if(value < 10000.0f)
    n = (BYTE)sprintf((char*)buff, "%7.2f", value);
  else if(value < 100000.0f)
    n = (BYTE)sprintf((char*)buff, "%7.1f", value);
  else {
    n = (BYTE)sprintf((char*)buff, "%.0f", value);
    if(n > 7)
      memset(buff, '*', n = 7);
  }
  
  return n;
}
