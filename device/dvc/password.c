/*---------------------------------------------------------
  password.c
---------------------------------------------------------*/

#include "password.h"
#include "lcd.h"
#include "keybrd.h"
#include "menu.h"

#define PASSWORD_STR_LENGTH     14

static NOINIT BYTE PasswordStr[PASSWORD_STR_LENGTH];

//---------------------------------------------------------
void InitPassword(void)
{
  PrgFlags.Password = 0;
}

//---------------------------------------------------------
void ShowPasswordIni(void)
{
  PrgVerb = VB_PASSWORD;
  TempWord = 0;
  memset(PasswordStr, 0, PASSWORD_STR_LENGTH);

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdDrawLangCaption(119);

  LcdDrawEnd();

  TempBool = TRUE;
  TempByte = 0;
}

//---------------------------------------------------------
void ShowPassword(void)
{
  ProcessStandardKeyDownActions();

  if(KeyDown == KB_MENU) {
    WORD n = LoadTextResource(LcdText, 121);
    if(n == TempWord)
      if(memcmp(PasswordStr, LcdText, n) == 0) {
        PrgFlags.Password = 1;
        MenuPasswordEntered();
      }
  }

  if(TempWord < PASSWORD_STR_LENGTH)
    if(KeyDown != KB_NOKEY) {
      switch(KeyDown) {
        case KB_UP:    PasswordStr[TempWord++] = 'A'; break;
        case KB_RIGHT: PasswordStr[TempWord++] = 'B'; break;
        case KB_DOWN:  PasswordStr[TempWord++] = 'C'; break;
        case KB_LEFT:  PasswordStr[TempWord++] = 'D'; break;
        default:       PasswordStr[TempWord++] = 'X';
      }
      TempBool = TRUE;
    }

  if(KeyDown == KB_F1)
    PrgVerb = VB_PASSWORD_INI;

  if(PrgFlags.CentiSec) {
    switch(++TempByte) {
      case 12:
        TempByte = 0;
      case 8:
        TempBool = TRUE;
    }
  }

  if(TempBool == TRUE) {
    WORD i;
    TLcdRect rect;
    WORD letter_w;

    TempBool = FALSE;

    LcdDrawBegin();

    letter_w = LcdGetTextWidth((BYTE*)"*", 1) + 2;

    rect.Bottom = (rect.Top = LCD_H/2 - LcdFontHeight/2) + LcdFontHeight;
    rect.Right  = (rect.Left = LCD_W/2 - (letter_w*PASSWORD_STR_LENGTH)/2) +
     letter_w - 1;

    for(i = 0; i < PASSWORD_STR_LENGTH; i++) {
      if(i < TempWord) {
        LcdDrawCaRectText(&rect, 0, (BYTE*)"*", 1);
      }
      else if(i == TempWord && TempByte < 8) {
        LcdSetTextColor(LcdBgColorDef);
        LcdSetBgColor(LcdTextColorDef);
        LcdDrawCaRectText(&rect, 0, (BYTE*)"-", 1);
        LcdSetTextColorDef();
        LcdSetBgColorDef();
      }
      else {
        LcdDrawCaRectText(&rect, 0, (BYTE*)"-", 1);
      }
      rect.Left  += letter_w;
      rect.Right += letter_w;
    }

    LcdDrawEnd();
  }
}
