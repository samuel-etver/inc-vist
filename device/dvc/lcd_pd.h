#ifndef LCD_PD_H_INCLUDED
#define LCD_PD_H_INCLUDED

/*---------------------------------------------------------
  lcd_pd.h
---------------------------------------------------------*/

#include "xmain.h"
#include "lcd.h"

void InitLcd_pd(void);
void LcdPowerOn_pd(void);
__ramfunc void LcdWriteCmdByte(BYTE b);
__ramfunc void LcdWriteDataByte(BYTE b);
void LcdWriteCmd(BYTE* cmd, BYTE n);
DWORD LcdReadStatus(void);
DWORD LcdReadPowerMode(void);
DWORD LcdReadIndentification(void);
__ramfunc void LcdBeginDrawRegion(WORD x, WORD y, WORD w, WORD h);
__ramfunc void LcdEndDrawRegion(void);
void LcdBeginReadRegion(WORD x, WORD y, WORD w, WORD h);
void LcdEndReadRegion(void);
DWORD LcdGetPixel_pd(void);
void LcdGetPixels_pd(BYTE* buff, WORD n);
void LcdDrawBegin_pd(void);
void LcdDrawEnd_pd(void);

#ifdef PD_PC

#define LCD_MEM_SIZE  (LCD_W*LCD_H)

extern TColor LcdMem[LCD_MEM_SIZE];

#else

#endif

#endif
