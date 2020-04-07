/*---------------------------------------------------------
  iic_pd.c
---------------------------------------------------------*/

#include "iic_pd.h"
#include "iic.h"
#include "system.h"
#include "delay.h"

#define IIC_PORT        GPIOB
#define IIC_SDA_PIN     GPIO_PIN_7
#define IIC_SCL_PIN     GPIO_PIN_8
#define IIC_SDA_MASK    GPIO_MASK_PIN_7
#define IIC_SCL_MASK    GPIO_MASK_PIN_8
#define IIC_SDA_OUT     GPIO_OUT_PIN_7
#define IIC_SCL_OUT     GPIO_OUT_PIN_8
#define IIC_SDA_IN      GPIO_IN_PIN_7
#define IIC_SCL_IN      GPIO_IN_PIN_8

static void IicSoftStartWrite(BYTE slave, DWORD address, BYTE address_size);
static void IicSoftStopWrite(void);
static void IicSoftWriteByte(BYTE b);
static void IicSoftStartRead(BYTE slave, DWORD address, BYTE address_size);
static void IicSoftStopRead(void);
static BYTE IicSoftReadByte(void);
static BYTE IicSoftReadLastByte(void);
static void IicSoftSclLow(void);
static void IicSoftSclHigh(void);
static void IicSoftSdaLow(void);
static void IicSoftSdaHigh(void);
static void IicSoftSdaIn(void);
static void IicSoftSdaOut(void);
static BOOL IicSoftIsSdaHigh(void);
static void IicSoftReset(void);
static void IicSoftDelay(void);

//---------------------------------------------------------
void InitIic_pd(void)
{
  IicSetSoft_pd();
}

//---------------------------------------------------------
static void IicSoftReset(void)
{
  BYTE i;

  IicSoftSdaIn();

  for(i = 0; i < 100; i++) {
    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    if(IicSoftIsSdaHigh() == TRUE)
      break;
    IicSoftSclLow();
  }

  IicSoftSclHigh();
}

//---------------------------------------------------------
static void IicSoftStartWrite(BYTE slave, DWORD address,
  BYTE address_size)
{
  IicSoftSdaOut();
  IicSoftDelay();
  IicSoftDelay();
  IicSoftSclLow();
  IicSoftSdaHigh();
  IicSoftDelay();
  IicSoftDelay();
  IicSoftSclHigh();
  IicSoftDelay();
  IicSoftSdaLow();
  IicSoftDelay();
  IicSoftSclLow();
  IicSoftDelay();
  IicSoftSdaIn();

  IicSoftWriteByte(slave);

  if(address_size >= 4)
    IicSoftWriteByte(address >> 24);
  if(address_size >= 3)
    IicSoftWriteByte(address >> 16);
  if(address_size >= 2)
    IicSoftWriteByte(address >> 8);
  if(address_size >= 1)
    IicSoftWriteByte(address);
}

//---------------------------------------------------------
static void IicSoftStopWrite(void)
{
  for(;;) {
    IicSoftSdaOut();
    IicSoftDelay();
    IicSoftSclLow();
    IicSoftSdaLow();
    IicSoftDelay();
    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    IicSoftDelay();
    IicSoftSdaHigh();
    IicSoftDelay();
    IicSoftSdaIn();
    if(IicSoftIsSdaHigh() == TRUE)
      break;
  }
}

//---------------------------------------------------------
static void IicSoftWriteByte(BYTE b)
{
  BYTE i;

  IicSoftSdaOut();

  for(i = 0; i < 8; i++) {
    if(b & 0x80)
      IicSoftSdaHigh();
    else
      IicSoftSdaLow();

    b <<= 1;

    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    IicSoftSclLow();
  }

  IicSoftSdaIn();
  IicSoftDelay();
  IicSoftSclHigh();  
  IicSoftDelay();
  IicFlags.Ack = IicSoftIsSdaHigh() == FALSE;
  IicSoftSclLow();
  IicSoftDelay();
}

//---------------------------------------------------------
static void IicSoftStopRead(void)
{
  IicSoftReadLastByte();

  for(;;) {
    IicSoftSdaOut();
    IicSoftDelay();
    IicSoftSclLow();
    IicSoftSdaLow();
    IicSoftDelay();
    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    IicSoftDelay();
    IicSoftSdaHigh();
    IicSoftDelay();
    IicSoftSdaIn();
    if(IicSoftIsSdaHigh() == TRUE)
      break;
  }
}

//---------------------------------------------------------
static BYTE IicSoftReadByte(void)
{
  BYTE i;
  BYTE b;

  IicSoftSdaIn();

  b = 0;

  for(i = 0; i < 8; i++) {
    b <<= 1;
    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    if(IicSoftIsSdaHigh() == TRUE)
      b |= 0x01;
    IicSoftSclLow();
  }

  IicSoftDelay();
  IicSoftSdaOut();
  IicSoftSdaLow();
  IicSoftDelay();
  IicSoftSclHigh();
  IicSoftDelay();
  IicSoftSclLow();
  IicSoftDelay();
  IicSoftSdaIn();

  return b;
}

//---------------------------------------------------------
static void IicSoftStartRead(BYTE slave, DWORD address, BYTE address_size)
{
  IicSoftStartWrite(slave, address, address_size);
  IicSoftReset();
  IicSoftStartWrite(slave | 0x01, 0, 0);
}

//---------------------------------------------------------
static BYTE IicSoftReadLastByte(void)
{
  BYTE i;
  BYTE b;

  IicSoftSdaIn();

  b = 0;

  for(i = 0; i < 8; i++) {
    b <<= 1;
    IicSoftDelay();
    IicSoftSclHigh();
    IicSoftDelay();
    if(IicSoftIsSdaHigh() == TRUE)
      b |= 0x01;
    IicSoftSclLow();
  }

  IicSoftDelay();
  IicSoftSdaIn();

  return b;
}

//---------------------------------------------------------
static void IicSoftSclLow(void)
{
  GPIO_SET_OUT_LOW(IIC_PORT, IIC_SCL_PIN);
  GPIO_SET_MODE(IIC_PORT, IIC_SDA_MASK, IIC_SCL_OUT);
  GPIO_SET_OUT_LOW(IIC_PORT, IIC_SCL_PIN);
}

//---------------------------------------------------------
static void IicSoftSclHigh(void)
{
  GPIO_SET_MODE(IIC_PORT, IIC_SCL_MASK, IIC_SCL_IN);
}

//---------------------------------------------------------
static void IicSoftSdaLow(void)
{
  GPIO_SET_OUT_LOW(IIC_PORT, IIC_SDA_PIN);
  GPIO_SET_MODE(IIC_PORT, IIC_SDA_MASK, IIC_SDA_OUT);
  GPIO_SET_OUT_LOW(IIC_PORT, IIC_SDA_PIN);
}

//---------------------------------------------------------
static void IicSoftSdaHigh(void)
{
  GPIO_SET_MODE(IIC_PORT, IIC_SDA_MASK, IIC_SDA_IN);
}

//---------------------------------------------------------
static void IicSoftSdaIn(void)
{
  GPIO_SET_MODE(IIC_PORT, IIC_SDA_MASK, IIC_SDA_IN);
}

//---------------------------------------------------------
static void IicSoftSdaOut(void)
{
}

//---------------------------------------------------------
static BOOL IicSoftIsSdaHigh(void)
{
  if(GPIO_IS_IN_HIGH(IIC_PORT, IIC_SDA_PIN))
    return TRUE;
  return FALSE;
}

//---------------------------------------------------------
static void IicSoftDelay(void)
{
  Delay(SYS_CONVERT_MCS(1.3));
}

//---------------------------------------------------------
BOOL IicIsSoft_pd(void)
{
  return TO_BOOL(IicStartWrite == IicSoftStartWrite);
}

//---------------------------------------------------------
void IicSetSoft_pd(void)
{
  IicStartWrite = IicSoftStartWrite;
  IicStopWrite = IicSoftStopWrite;
  IicWriteByte = IicSoftWriteByte;
  IicStartRead = IicSoftStartRead;
  IicStopRead = IicSoftStopRead;
  IicReadByte = IicSoftReadByte;
}
