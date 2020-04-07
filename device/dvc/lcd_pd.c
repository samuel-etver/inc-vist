/*---------------------------------------------------------
  lcd_pd.c TO28240320-A6TMN-019
---------------------------------------------------------*/

#include "lcd_pd.h"
#include "lcd.h"
#include "system.h"
#include "delay.h"

#define LCD_RES_PORT  GPIOA
#define LCD_RES_PIN   GPIO_PIN_8
#define LCD_CSX_PORT  GPIOA
#define LCD_CSX_PIN   GPIO_PIN_4
#define LCD_DCX_PORT  GPIOB
#define LCD_DCX_PIN   GPIO_PIN_0

static const BYTE LcdInitCmd0[] = {0x3A, 0x55};
static const BYTE LcdInitCmd1[] = {0xF6, 0x01, 0x33};
static const BYTE LcdInitCmd2[] = {0xB5, 0x04, 0x04, 0x0A, 0x14};
static const BYTE LcdInitCmd3[] = {0x35, 0x00};
static const BYTE LcdInitCmd4[] = {0xCF, 0x00, 0xEA, 0xF0};
static const BYTE LcdInitCmd5[] = {0xED, 0x64, 0x03, 0x12, 0x81};
static const BYTE LcdInitCmd6[] = {0xE8, 0x85, 0x00, 0x78};
static const BYTE LcdInitCmd7[] = {0xCB, 0x39, 0x2C, 0x00, 0x33, 0x06};
static const BYTE LcdInitCmd8[] = {0xF7, 0x20};
static const BYTE LcdInitCmd9[] = {0xEA, 0x00, 0x00};
static const BYTE LcdInitCmd10[] = {0xC0, 0x21};
static const BYTE LcdInitCmd11[] = {0xC1, 0x10};
static const BYTE LcdInitCmd12[] = {0xC5, 0x4F, 0x38};
static const BYTE LcdInitCmd13[] = {0x36, 0x08};
static const BYTE LcdInitCmd14[] = {0xB1, 0x00, 0x13};
static const BYTE LcdInitCmd15[] = {0xB6, 0x0A, 0xA2};
static const BYTE LcdInitCmd16[] = {0xF2, 0x02};
static const BYTE LcdInitCmd17[] = {0xE0, 0x0F, 0x38, 0x24, 0x0C, 0x10, 0x08,
 0x55, 0x75, 0x33, 0x06, 0x0E, 0x00, 0x0C, 0x09, 0x08};
static const BYTE LcdInitCmd18[] = {0xE1, 0x00, 0x0F, 0x12, 0x05, 0x11, 0x04,
 0x33, 0x24, 0x4e, 0x04, 0x0f, 0x0e, 0x35, 0x38, 0x0f};
static const BYTE LcdInitCmd19[] = {0x11};

static const BYTE* LcdInitCmds[] = {
  LcdInitCmd0,
  LcdInitCmd1,
  LcdInitCmd2,
  LcdInitCmd3,
  LcdInitCmd4,
  LcdInitCmd5,
  LcdInitCmd6,
  LcdInitCmd7,
  LcdInitCmd8,
  LcdInitCmd9,
  LcdInitCmd10,
  LcdInitCmd11,
  LcdInitCmd12,
  LcdInitCmd13,
  LcdInitCmd14,
  LcdInitCmd15,
  LcdInitCmd16,
  LcdInitCmd17,
  LcdInitCmd18,
  LcdInitCmd19,
 // LcdInitCmd20,
  0
};
static const BYTE LcdInitCmdLens[] = {
  ARRAY_LENGTH(LcdInitCmd0),
  ARRAY_LENGTH(LcdInitCmd1),
  ARRAY_LENGTH(LcdInitCmd2),
  ARRAY_LENGTH(LcdInitCmd3),
  ARRAY_LENGTH(LcdInitCmd4),
  ARRAY_LENGTH(LcdInitCmd5),
  ARRAY_LENGTH(LcdInitCmd6),
  ARRAY_LENGTH(LcdInitCmd7),
  ARRAY_LENGTH(LcdInitCmd8),
  ARRAY_LENGTH(LcdInitCmd9),
  ARRAY_LENGTH(LcdInitCmd10),
  ARRAY_LENGTH(LcdInitCmd11),
  ARRAY_LENGTH(LcdInitCmd12),
  ARRAY_LENGTH(LcdInitCmd13),
  ARRAY_LENGTH(LcdInitCmd14),
  ARRAY_LENGTH(LcdInitCmd15),
  ARRAY_LENGTH(LcdInitCmd16),
  ARRAY_LENGTH(LcdInitCmd17),
  ARRAY_LENGTH(LcdInitCmd18),
  ARRAY_LENGTH(LcdInitCmd19),
 // ARRAY_LENGTH(LcdInitCmd20),
};

static BYTE LcdWriteReadDataByte(BYTE b);
static BYTE LcdReadByte(void);

//---------------------------------------------------------
void InitLcd_pd(void)
{
}

//---------------------------------------------------------
void LcdPowerOn_pd(void)
{
  WORD i;

  __HAL_SPI_ENABLE(&hspi1);

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);

  GPIO_SET_OUT_HIGH(LCD_RES_PORT, LCD_RES_PIN);
  Delay(SYS_CONVERT_MCS(1000));
  GPIO_SET_OUT_LOW(LCD_RES_PORT, LCD_RES_PIN);
  Delay(SYS_CONVERT_MCS(14000));
  GPIO_SET_OUT_HIGH(LCD_RES_PORT, LCD_RES_PIN);
  Delay(SYS_CONVERT_MCS(120000));

  i = 0;
  do {
    LcdWriteCmd((BYTE*)LcdInitCmds[i], LcdInitCmdLens[i]);
  } while(LcdInitCmds[++i]);
  Delay(SYS_CONVERT_MCS(10000));

  LcdSetBgColor(LCD_RGB_TO_COLOR(0,0,0));
  LcdDrawBackground();

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(0x29); // Display on
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);
}

//---------------------------------------------------------
__ramfunc void LcdWriteCmdByte(BYTE b)
{
  GPIO_SET_OUT_LOW(LCD_DCX_PORT, LCD_DCX_PIN);
  LcdWriteDataByte(b);
  GPIO_SET_OUT_HIGH(LCD_DCX_PORT, LCD_DCX_PIN);
}

//---------------------------------------------------------
__ramfunc void LcdWriteDataByte(BYTE b)
{
  (void)SPI1->DR;
  SPI1->DR = b;
  while((SPI1->SR & 0x80));
  while(!(SPI1->SR & 0x01));
}

//---------------------------------------------------------
static BYTE LcdWriteReadDataByte(BYTE b)
{
  (void)SPI1->DR;
  SPI1->DR = b;
  while((SPI1->SR & 0x80));
  while(!(SPI1->SR & 0x01));
  return SPI1->DR;
}

//---------------------------------------------------------
DWORD LcdReadStatus(void)
{
  DWORD dw;

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(0x09);
  LcdReadByte();
  *((BYTE*)&dw + 3) = LcdReadByte();
  *((BYTE*)&dw + 2) = LcdReadByte();
  *((BYTE*)&dw + 1) = LcdReadByte();
  *((BYTE*)&dw + 0) = LcdReadByte();
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);

  return dw;
}

//---------------------------------------------------------
DWORD LcdReadPowerMode(void)
{
  DWORD dw;
  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(0x0A);
  LcdReadByte();
  dw = LcdReadByte();
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);
  return dw;
}

//---------------------------------------------------------
DWORD LcdReadIndentification(void)
{
  DWORD dw = 0;

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(0x04);
  LcdReadByte();
  *((BYTE*)&dw + 2) = LcdReadByte();
  *((BYTE*)&dw + 1) = LcdReadByte();
  *((BYTE*)&dw + 0) = LcdReadByte();
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);

  return dw;
}

//---------------------------------------------------------
void LcdWriteCmd(BYTE* cmd, BYTE n)
{
  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(*cmd++);
  while(--n)
    LcdWriteDataByte(*cmd++);
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);
}

//---------------------------------------------------------
__ramfunc void LcdBeginDrawRegion(WORD x, WORD y, WORD w, WORD h)
{
  WORD x1 = x + w - 1;
  WORD y1 = y + h - 1;

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);
  LcdWriteCmdByte(0x2A);
  LcdWriteDataByte(x  >> 8); LcdWriteDataByte(x);
  LcdWriteDataByte(x1 >> 8); LcdWriteDataByte(x1);

  LcdWriteCmdByte(0x2B);
  LcdWriteDataByte(y  >> 8); LcdWriteDataByte(y);
  LcdWriteDataByte(y1 >> 8); LcdWriteDataByte(y1);

  LcdWriteCmdByte(0x2C);
}

//---------------------------------------------------------
__ramfunc void LcdEndDrawRegion(void)
{
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);
}

//---------------------------------------------------------
void LcdBeginReadRegion(WORD x, WORD y, WORD w, WORD h)
{
  WORD x1 = x + w - 1;
  WORD y1 = y + h - 1;

  GPIO_SET_OUT_LOW(LCD_CSX_PORT, LCD_CSX_PIN);

  LcdWriteCmdByte(0x2A);
  LcdWriteDataByte(x  >> 8); LcdWriteDataByte(x);
  LcdWriteDataByte(x1 >> 8); LcdWriteDataByte(x1);

  LcdWriteCmdByte(0x2B);
  LcdWriteDataByte(y  >> 8); LcdWriteDataByte(y);
  LcdWriteDataByte(y1 >> 8); LcdWriteDataByte(y1);

  LcdWriteCmdByte(0x2E);
}

//---------------------------------------------------------
void LcdEndReadRegion(void)
{
  GPIO_SET_OUT_HIGH(LCD_CSX_PORT, LCD_CSX_PIN);
}

//---------------------------------------------------------
DWORD LcdGetPixel_pd(void)
{
  BYTE buff[4];
  DWORD dw;
  LcdReadByte();
  buff[0] = LcdReadByte() << 1;
  buff[1] = LcdReadByte() << 1;
  buff[2] = LcdReadByte() << 1;
  buff[3] = 0;
  memcpy(&dw, buff, 4);
  return dw;
}

//---------------------------------------------------------
void LcdGetPixels_pd(BYTE* buff, WORD n)
{
  WORD i;

  LcdReadByte();

  for(i = 0; i < n; i++) {
    *buff++ = LcdReadByte() << 1;
    *buff++ = LcdReadByte() << 1;
    *buff++ = LcdReadByte() << 1;
  }
}

//---------------------------------------------------------
static BYTE LcdReadByte(void)
{
  return LcdWriteReadDataByte(0xFF);
}

//---------------------------------------------------------
void LcdDrawBegin_pd(void)
{
}

//---------------------------------------------------------
void LcdDrawEnd_pd(void)
{
}

