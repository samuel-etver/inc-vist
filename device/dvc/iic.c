/*---------------------------------------------------------
  iic.c
---------------------------------------------------------*/

#include "iic.h"
#include "iic_pd.h"
#include "delay.h"
#include "system.h"
#include "light.h"

NOINIT TIicFlags IicFlags;

NOINIT TIicStartWriteProc IicStartWrite;
NOINIT TIicStopWriteProc IicStopWrite;
NOINIT TIicStartReadProc IicStartRead;
NOINIT TIicStopReadProc IicStopRead;
NOINIT TIicWriteByteProc IicWriteByte;
NOINIT TIicReadByteProc IicReadByte;

//---------------------------------------------------------
void InitIic(void)
{
  InitIic_pd();
}

//---------------------------------------------------------
void IicSetFastFrequencyBefore(void)
{
}

//---------------------------------------------------------
void IicSetNormalFrequencyAfter(void)
{
}  

//---------------------------------------------------------
BOOL IicCheckSlave(BYTE slave)
{
  IicStartWrite(slave, 0, 0);
  IicStopWrite();    
  return TO_BOOL(IicFlags.Ack);
}

//---------------------------------------------------------
BOOL IicIsSoft(void)
{
  return IicIsSoft_pd();
}

//---------------------------------------------------------
void IicSetSoft(void)
{
  IicSetSoft_pd();
}
