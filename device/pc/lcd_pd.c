/*---------------------------------------------------------
  lcd_pd.c
---------------------------------------------------------*/

#include "lcd_pd.h"
#include "lcd.h"
#include <stdlib.h>

TColor LcdMem[LCD_MEM_SIZE];
static WORD LcdRegX0;
static WORD LcdRegY0;
static WORD LcdRegW;
static WORD LcdRegH;
static DWORD LcdRegBytesIndex;
static DWORD LcdRegBytesCount;

static INT32 LcdXYtoIndex(INT32 x, INT32 y);

//---------------------------------------------------------
void InitLcd_pd(void)
{
  memset(LcdMem, 0, sizeof(LcdMem));
  LcdRegX0 = 0;
  LcdRegY0 = 0;
  LcdRegW  = LCD_W;
  LcdRegH  = LCD_H;
  LcdRegBytesIndex = 0;
  LcdRegBytesCount = 2*LcdRegW*LcdRegH;
}

//---------------------------------------------------------
void LcdPowerOn_pd(void)
{
}

//---------------------------------------------------------
__ramfunc void LcdWriteCmdByte(BYTE b)
{
}

//---------------------------------------------------------
__ramfunc void LcdWriteDataByte(BYTE b)
{
  DWORD x;
  DWORD y;
  DWORD index;

  if(LcdRegBytesCount == 0) return;
  if(LcdRegW == 0) return;

  x = (LcdRegBytesIndex/sizeof(TColor))%LcdRegW + LcdRegX0;
  y = (LcdRegBytesIndex/sizeof(TColor))/LcdRegW + LcdRegY0;

  index = sizeof(TColor)*(x + y*LCD_W);
  if(LcdRegBytesIndex & 1)
    index++;
  if(index < sizeof(TColor)*LCD_MEM_SIZE)
    ((BYTE*)LcdMem)[index] = b;

  if(++LcdRegBytesIndex == LcdRegBytesCount)
    LcdRegBytesIndex = 0;
}

//---------------------------------------------------------
void LcdWriteCmd(BYTE* cmd, BYTE n)
{
}

//---------------------------------------------------------
void LcdBeginDrawRegion(WORD x, WORD y, WORD w, WORD h)
{
  LcdRegX0 = x;
  LcdRegY0 = y;
  LcdRegW  = w;
  LcdRegH  = h;
  LcdRegBytesCount = w*h*sizeof(TColor);
  LcdRegBytesIndex = 0;
}

//---------------------------------------------------------
void LcdEndDrawRegion(void)
{
}

//---------------------------------------------------------
static INT32 LcdXYtoIndex(INT32 x, INT32 y)
{
  if(x < 0 || x >= LCD_W) return -1;
  if(y < 0 || y >= LCD_H) return -1;
  return x + y*LCD_W;
}

//---------------------------------------------------------
DWORD LcdGetPixel_pd(void)
{
  INT32 index = LcdXYtoIndex(LcdRegX0, LcdRegY0);
  if(index < 0)
    return 0;
  return LcdMem[index];
}

//---------------------------------------------------------
void LcdDrawBegin_pd(void)
{
}

//---------------------------------------------------------
void LcdDrawEnd_pd(void)
{
}

//---------------------------------------------------------
void LcdBeginReadRegion(WORD x, WORD y, WORD w, WORD h)
{
  LcdBeginDrawRegion(x, y, w, h);
}

//---------------------------------------------------------
void LcdEndReadRegion(void)
{
}

//---------------------------------------------------------
void LcdGetPixels_pd(BYTE* buff, WORD n)
{
}
