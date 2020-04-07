/*---------------------------------------------------------
  MemDump.c
---------------------------------------------------------*/

#include "FlashDump.h"
#include "lcd.h"
#include "keybrd.h"
#include "flash.h"

#define FLASHDUMP_COLS          4
#define FLASHDUMP_ROWS          12

static NOINIT DWORD FlashDumpAddress;
static NOINIT BYTE FlashDumpRow;
static NOINIT BYTE FlashDumpCol;
static NOINIT BYTE FlashDumpEditPos;

typedef enum {
  FDV_VIEWDUMP_INI,
  FDV_VIEWDUMP,
  FDV_EDITADDRESS_INI,
  FDV_EDITADDRESS
} TFlashDumpVerb;

static void FlashDumpViewDumpIni(void);
static void FlashDumpViewDump(void);
static void FlashDumpEditAddressIni(void);
static void FlashDumpEditAddress(void);

static BYTE* FlashDumpGetText(void* o);
static WORD FlashDumpGetSpinEditX(void);
static WORD FlashDumpGetSpinEditXi(WORD i);
static void FlashDumpDrawAddr(void);

//---------------------------------------------------------
void InitFlashDump(void)
{
  FlashDumpAddress = 0;
  FlashDumpRow = 0;
  FlashDumpEditPos = 0;
}

//---------------------------------------------------------
void ShowFlashDumpIni(void)
{
  PrgVerb = VB_FLASHDUMP;
  TempWord = FDV_VIEWDUMP_INI;
}

//---------------------------------------------------------
void ShowFlashDump(void)
{
  ProcessStandardKeyDownActions();

  switch(TempWord) {
    case FDV_VIEWDUMP_INI:
      FlashDumpViewDumpIni();
    case FDV_VIEWDUMP:
      FlashDumpViewDump(); break;
    case FDV_EDITADDRESS_INI:
      FlashDumpEditAddressIni();
    case FDV_EDITADDRESS:
      FlashDumpEditAddress();
  }
}

//---------------------------------------------------------
static void FlashDumpViewDumpIni(void)
{
  TempWord = FDV_VIEWDUMP;

  FlashDumpCol = FlashDumpAddress & 0x03;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdDrawLangHint(471, 0, 0);

  LcdDrawEnd();

  TempBool = TRUE;
}

//---------------------------------------------------------
static void FlashDumpViewDump(void)
{
  if(KeyDown == KB_F1)
    TempWord = FDV_EDITADDRESS_INI;

  if(KeyDown == KB_UP)
    if(FlashDumpAddress >= FLASHDUMP_COLS) {
      FlashDumpAddress -= FLASHDUMP_COLS;
      if(FlashDumpRow)
        FlashDumpRow--;
      TempBool = TRUE;
    }

  if(KeyDown == KB_DOWN)
    if(FlashDumpAddress < FlashGetSize() - FLASHDUMP_COLS) {
      FlashDumpAddress += FLASHDUMP_COLS;
      if(FlashDumpRow < FLASHDUMP_ROWS - 1)
        FlashDumpRow++;
      TempBool = TRUE;
    }

  if(KeyDown == KB_LEFT)
    if(FlashDumpAddress) {
      FlashDumpAddress--;
      if(FlashDumpCol-- == 0) {
        FlashDumpCol = FLASHDUMP_COLS - 1;
        if(FlashDumpRow)
          FlashDumpRow--;
      }
      TempBool = TRUE;
    }

  if(KeyDown == KB_RIGHT)
    if(FlashDumpAddress < FlashGetSize() - 1) {
      FlashDumpAddress++;
      if(FlashDumpCol++ == FLASHDUMP_COLS - 1) {
        FlashDumpCol = 0;
        if(FlashDumpRow < FLASHDUMP_ROWS - 1)
          FlashDumpRow++;
      }
      TempBool = TRUE;
    }

  if(KeyDown == KB_PLUS)
    if(FlashDumpAddress >= FLASHDUMP_COLS*FLASHDUMP_ROWS) {
      FlashDumpAddress -= FLASHDUMP_COLS*FLASHDUMP_ROWS;
      TempBool = TRUE;
    }

  if(KeyDown == KB_MINUS)
    if(FlashDumpAddress < FlashGetSize() - FLASHDUMP_COLS*FLASHDUMP_ROWS) {
      FlashDumpAddress += FLASHDUMP_COLS*FLASHDUMP_ROWS;
      TempBool = TRUE;
    }

  if(TempBool == TRUE) {
    WORD row;
    WORD col;
    WORD y = 38;
    DWORD addr = FlashDumpAddress - FLASHDUMP_COLS*FlashDumpRow - FlashDumpCol;
    TLcdRect addr_rect;
    TLcdRect cell_rect;
    WORD cell_width;

    TempBool = FALSE;

    FlashAddress = addr;

    LcdDrawBegin();

    LcdSetFont(FONT10);

    cell_width = LcdGetTextWidth((BYTE*)"00 ", 3);

    for(row = 0; row < FLASHDUMP_ROWS; row++) {
      LcdSetTextColor(((addr/4) & 1) ? LcdEvenRowTextColorDef :
       LcdTextColorDef);
      LcdSetBgColorDef();
      addr_rect.Right =
       (addr_rect.Left = 8) + LcdGetTextWidth((BYTE*)"00000:X", 7);
      addr_rect.Bottom = (addr_rect.Top = y) + LcdFontHeight;
      sprintf((char*)LcdText, "%05X:", (unsigned int)addr);
      LcdDrawLaRectText(&addr_rect, 0, 0, LcdText, 6);

      cell_rect.Left   = 90;
      cell_rect.Top    = addr_rect.Top;
      cell_rect.Bottom = addr_rect.Bottom;

      for(col = 0; col < FLASHDUMP_COLS; col++) {
        if(row == FlashDumpRow && col == FlashDumpCol) {
          LcdSetTextColor(LcdBgColorDef);
          LcdSetBgColor(LcdTextColorDef);
        }
        else {
          LcdSetTextColor(((addr/4) & 1) ? LcdEvenRowTextColorDef :
           LcdTextColorDef);
          LcdSetBgColorDef();
        }

        cell_rect.Right = cell_rect.Left + cell_width;

        sprintf((char*)LcdText, "%02X", (unsigned int)FlashReadByte());
        LcdDrawCaRectText(&cell_rect, 0, LcdText, 2);

        cell_rect.Left += 38;
      }

      y += LcdFontHeight;

      addr += 4;
    }

    LcdDrawEnd();
  }
}

//---------------------------------------------------------
static void FlashDumpEditAddressIni(void)
{
  TempWord = FDV_EDITADDRESS;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdDrawLangCaption(469);

  LcdSetFont(LCD_ITEM_FONT);

  LcdSpinEditCreate(NULL);
  LcdSpinEdit.Edit.TextAlignment = lcdHaCenter;
  LcdSpinEdit.Edit.GetText = FlashDumpGetText;
  LcdSpinEdit.Max = 0x0F;
  LcdSpinEdit.W = 22;
  LcdSpinEdit.X = FlashDumpGetSpinEditX();
  LcdSpinEdit.Y = 140;
  LcdSpinEditSetup(NULL);
  LcdSpinEditDraw(NULL);

  FlashDumpDrawAddr();

  LcdDrawLangHint(959, 0, 0);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void FlashDumpEditAddress(void)
{
  if(KeyDown == KB_F1)
    TempWord = FDV_VIEWDUMP_INI;

  if(KeyDown == KB_MEASURE || KeyDown == KB_MENU || KeyDown == KB_F1) {
    if(FlashDumpAddress >= FlashGetSize() - FLASHDUMP_ROWS*FLASHDUMP_COLS) {
      FlashDumpAddress = FlashGetSize() - 1;
      FlashDumpRow = FLASHDUMP_ROWS - 1;
      FlashDumpCol = FLASHDUMP_COLS - 1;
    }
    FlashDumpCol = FlashDumpAddress & 0xFFFFFFFC;
  }

  if(KeyDown == KB_LEFT) {
    if(FlashDumpEditPos-- == 0)
      FlashDumpEditPos = 4;
    TempBool = TRUE;
  }

  if(KeyDown == KB_RIGHT) {
    if(++FlashDumpEditPos == 5)
      FlashDumpEditPos = 0;
    TempBool = TRUE;
  }

  if(KeyDown == KB_PLUS) {
    DWORD mask = 0x0F << (4*(4 - FlashDumpEditPos));
    FlashDumpAddress = (FlashDumpAddress & ~mask) |
      (mask & (FlashDumpAddress + (1 << (4*(4 - FlashDumpEditPos)))));
  }

  if(KeyDown == KB_MINUS) {
    DWORD mask = 0x0F << (4*(4 - FlashDumpEditPos));
    FlashDumpAddress = (FlashDumpAddress & ~mask) |
      (mask & (FlashDumpAddress - (1 << (4*(4 - FlashDumpEditPos)))));
  }

  LcdSpinEditDo(NULL);

  if(TempBool == TRUE) {
    TempBool = FALSE;

    LcdDrawBegin();

    LcdSpinEditUnmap(NULL);
    FlashDumpDrawAddr();
    LcdSpinEdit.X = FlashDumpGetSpinEditX();
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdDrawEnd();
  }
}

//---------------------------------------------------------
static BYTE* FlashDumpGetText(void* o)
{
  sprintf((char*)LcdText + 1, "%05X", (unsigned int)FlashDumpAddress);
  LcdText[0] = 1;
  LcdText[1] = LcdText[FlashDumpEditPos + 1];
  return LcdText;
}

//---------------------------------------------------------
static WORD FlashDumpGetSpinEditX(void)
{
  return FlashDumpGetSpinEditXi(FlashDumpEditPos);
}

//---------------------------------------------------------
static WORD FlashDumpGetSpinEditXi(WORD i)
{
  return (LCD_W/2 - (5*LcdSpinEdit.W)/2) + i*LcdSpinEdit.W;
}

//---------------------------------------------------------
static void FlashDumpDrawAddr(void)
{
  WORD i;
  TLcdRect rect;

  sprintf((char*)LcdText, "%05X", (unsigned int)FlashDumpAddress);

  LcdSetFont(LCD_ITEM_FONT);
  LcdSetTextColorDef();

  rect.Bottom = (rect.Top = LcdSpinEdit.Y + 15) + LcdFontHeight;

  for(i = 0; i < 5; i++) {
    rect.Right = (rect.Left = FlashDumpGetSpinEditXi(i)) + LcdSpinEdit.W;
    if(FlashDumpEditPos != i)
      LcdDrawCaRectText(&rect, 0, LcdText + i, 1);
  }
}
