/*---------------------------------------------------------
  Memory.c
---------------------------------------------------------*/

#include "Memory.h"
#include "lcd.h"
#include "mem.h"
#include "utils.h"
#include "keybrd.h"

#define MEMORY_TABLE_ROWS   3
#define MEMORY_TABLE_COLS   2
#define MEMORY_TABLE_Y      120
#define MemoryTableX        TempWord

static const WORD MemoryRowTitleIds[] = {
  250, 252, 254
};

static WORD MemoryGetCellW(WORD index);
static WORD MemoryGetCellX(WORD index);
static WORD MemoryGetCellH(WORD index);
static WORD MemoryGetCellY(WORD index);
static WORD MemoryGetTableW(void);
static WORD MemoryGetTableH(void);
static void MemoryDrawCell(WORD row, WORD col, WORD value, WORD digits);

NOINIT WORD MemoryTotal;
NOINIT WORD MemoryUsed;

//---------------------------------------------------------
void ShowMemoryIni(void)
{
  WORD i;
  WORD table_w = MemoryGetTableW();
  WORD table_h = MemoryGetTableH();
  TLcdRect rect;

  LcdDrawBegin();

  LcdDrawWorkspaceWithLangCaption(211);

  MemoryTableX = (LCD_W - table_w)/2;
  LcdSetFgColor(LCD_RGB_TO_COLOR(0x88, 0x88, 0x88));
  for(i = 0; i <= MEMORY_TABLE_ROWS; i++)
    LcdDrawHorzLine(MemoryTableX, MemoryGetCellY(i), table_w);
  for(i = 0; i <= MEMORY_TABLE_COLS; i++)
    LcdDrawVertLine(MemoryGetCellX(i), MEMORY_TABLE_Y, table_h);

  LcdSetFont(FONT10);
  LcdSetBgColor(LCD_RGB_TO_COLOR(0x00, 0x44, 0x00));
  LcdSetTextColor(LCD_RGB_TO_COLOR(0xFF, 0xFF, 0xFF));
  rect.Left  = MemoryTableX + 1;
  rect.Right = rect.Left + MemoryGetCellW(0) - 1;
  for(i = 0; i < MEMORY_TABLE_ROWS; i++) {
    rect.Top    = MemoryGetCellY(i) + 1;
    rect.Bottom = rect.Top + MemoryGetCellH(i) - 1;
    LcdDrawRaRectLangText(&rect, 4,
     (rect.Bottom - rect.Top - LcdFontHeight + 1)/2 + 2,
     MemoryRowTitleIds[i]);
  }

  LcdSetBgColor(LcdBgColorDef);
  LcdSetTextColor(LcdTextColorDef);
  MemoryDrawCell(0, 1, MemoryTotal, 4);
  MemoryDrawCell(1, 1, MemoryUsed,  4);
  MemoryDrawCell(2, 1, (WORD)(MemoryTotal - MemoryUsed),  4);

  LcdDrawEnd();
}

//---------------------------------------------------------
BOOL ShowMemory(void)
{
  ProcessStandardKeyDownActions();
  return TO_BOOL(KeyDown == KB_MENU);
}

//---------------------------------------------------------
static WORD MemoryGetCellW(WORD index)
{
  switch(index) {
    case 0: return 110;
    case 1: return 72;
  }
  return 0;
}

//---------------------------------------------------------
static WORD MemoryGetCellX(WORD index)
{
  WORD i;
  WORD x = MemoryTableX;
  for(i = 0; i < index; i++)
    x += MemoryGetCellW(i);
  return x;
}

//---------------------------------------------------------
static WORD MemoryGetCellH(WORD index)
{
  switch(index) {
    case 0: return 40;
    case 1:
    case 2: return 30;
  }
  return 0;
}

//---------------------------------------------------------
static WORD MemoryGetCellY(WORD index)
{
  WORD i;
  WORD y = MEMORY_TABLE_Y;
  for(i = 0; i < index; i++)
    y += MemoryGetCellH(i);
  return y;
}

//---------------------------------------------------------
static WORD MemoryGetTableW(void)
{
  WORD i;
  WORD w = 0;
  for(i = 0; i < MEMORY_TABLE_COLS; i++)
    w += MemoryGetCellW(i);
  return w;
}

//---------------------------------------------------------
static WORD MemoryGetTableH(void)
{
  WORD i;
  WORD h = 0;
  for(i = 0; i < MEMORY_TABLE_ROWS; i++)
    h += MemoryGetCellH(i);
  return h;
}

//---------------------------------------------------------
static void MemoryDrawCell(WORD row, WORD col, WORD value, WORD digits)
{
  TLcdRect rect;
  WordToStrWithLeadingSpaces(LcdText, value, digits);
  rect.Left   = MemoryGetCellX(col) + 1;
  rect.Right  = rect.Left + MemoryGetCellW(col) - 1;
  rect.Top    = MemoryGetCellY(row) + 1;
  rect.Bottom = rect.Top + MemoryGetCellH(row) - 1;
  LcdDrawRaRectText(&rect, 4, (rect.Bottom - rect.Top - LcdFontHeight)/2 + 2,
   LcdText, digits);
}
