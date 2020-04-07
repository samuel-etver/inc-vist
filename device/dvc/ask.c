/*---------------------------------------------------------
  Ask.c
---------------------------------------------------------*/

#include "Ask.h"
#include "lcd.h"
#include "keybrd.h"
#include "language.h"

#define ASK_BUTTON_W      82
#define ASK_BUTTON_X      20
#define ASK_BUTTON_Y      160

static NOINIT TLcdButton AskYesButton;
static NOINIT TLcdButton AskNoButton;

//---------------------------------------------------------
void ShowAskIni(void)
{
  WORD i;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);

  if(TempByte <= 1)
    LcdDrawCaLangText(LCD_W/2, LCD_CAPTION_Y, TempWord);
  else {
    for(i = 0; i < TempByte; i++) {
      LcdDrawCaLangText(LCD_W/2, LCD_CAPTION_Y + i*LcdFontHeight - 4,
       TempWord);
      TempWord += LANG_COUNT;
    }
  }

  LcdButtonCreate(&AskYesButton, 166);
  LcdButtonCreate(&AskNoButton,  168);
  AskYesButton.Rect.Top  = ASK_BUTTON_Y;
  AskYesButton.Rect.Left = ASK_BUTTON_X;
  AskNoButton.Rect.Top   = ASK_BUTTON_Y;
  AskNoButton.Rect.Left  = LCD_W - ASK_BUTTON_X - ASK_BUTTON_W;
  AskNoButton.Selected   = TRUE;
  LcdButtonSetup(&AskYesButton);
  LcdButtonSetup(&AskNoButton);
  AskYesButton.Rect.Right = AskYesButton.Rect.Left + ASK_BUTTON_W;
  AskNoButton.Rect.Right  = AskNoButton.Rect.Left  + ASK_BUTTON_W;
  LcdButtonDraw(&AskYesButton);
  LcdButtonDraw(&AskNoButton);

  LcdDrawEnd();

  TempBool = FALSE;
}

//---------------------------------------------------------
void ShowAsk(void)
{
  ProcessStandardKeyDownActions();

  LcdButtonDo(&AskNoButton);
  LcdButtonDo(&AskYesButton);

  if(KeyDown == KB_LEFT || KeyDown == KB_RIGHT) {
    LcdButtonSelect(&AskNoButton, TempBool);
    TempBool = TempBool == TRUE ? FALSE : TRUE;
    LcdButtonSelect(&AskYesButton, TempBool);
  }
}
