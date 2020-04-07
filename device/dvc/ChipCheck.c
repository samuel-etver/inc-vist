/*---------------------------------------------------------
  ChipCheck.c
---------------------------------------------------------*/

#include "ChipCheck.h"
#include "iic.h"
#include "lcd.h"
#include "keybrd.h"
#include "flash.h"
#include "calendar.h"
#include "Accelerometer.h"
#include "delay.h"

static NOINIT enum {
  CCV_CHECK_FLASH,
  CCV_CHECK_OTHERS,
  CCV_SHOW_ERRORS_INI,
  CCV_SHOW_ERRORS
} ChipCheckVerb;

NOINIT TChipCheckFlags ChipCheckFlags;

static void ChipCheckFlash(void);
static void ChipCheckOthers(void);
static void ChipCheckShowErrorsIni(void);

//---------------------------------------------------------
void InitChipCheck(void)
{
  ChipCheckVerb = CCV_CHECK_FLASH;
  ChipCheckFlags.FlashAvailable         = 0;
  ChipCheckFlags.AccelerometerAvailable = 0;
}

//---------------------------------------------------------
void ShowChipCheckIni(void)
{
  PrgVerb = VB_CHIPCHECK;
}

//---------------------------------------------------------
void ShowChipCheck(void)
{
  switch(ChipCheckVerb) {
    case CCV_CHECK_FLASH:
      ChipCheckFlash(); break;
    case CCV_CHECK_OTHERS:
      ChipCheckOthers(); break;
    case CCV_SHOW_ERRORS_INI:
      ChipCheckShowErrorsIni(); break;
    default:;
  }
}

//---------------------------------------------------------
static void ChipCheckFlash(void)
{
  ChipCheckVerb = CCV_CHECK_OTHERS;
  if(FlashCheckSlave() == TRUE) {
    PrgVerb = VB_LOADCFG;
    ChipCheckFlags.FlashAvailable = 1;
  }
}

//---------------------------------------------------------
static void ChipCheckOthers(void)
{
  Delay(SYS_CONVERT_MCS(100000));
  
  if(AccelerometerCheckSlave() == TRUE)  
    ChipCheckFlags.AccelerometerAvailable = 1;

  if(ChipCheckFlags.FlashAvailable &&
     ChipCheckFlags.AccelerometerAvailable)
    PrgVerb = VB_SPLASH_INI;
  else
    ChipCheckVerb = CCV_SHOW_ERRORS_INI;
}

//---------------------------------------------------------
static void ChipCheckShowErrorsIni(void)
{
#define Y     70
#define X     8
  WORD i;
  WORD row = 0;

  ChipCheckVerb = CCV_SHOW_ERRORS;

  LcdDrawBegin();

  LcdDrawBackground();
  LcdDrawLangCaption(841);

  LcdSetFont(FONT10);

  for(i = 0; i < 2; i++) {
    LcdSetTextColor((row & 1) ? LcdEvenRowTextColorDef : LcdTextColorDef);
    switch(i) {
      case 0:
        if(!ChipCheckFlags.FlashAvailable)
          LcdDrawLaLangText(X, Y + row++*LcdFontHeight, 837);
        break;

      case 1:
        if(!ChipCheckFlags.AccelerometerAvailable)
          LcdDrawLaLangText(X, Y + row*LcdFontHeight, 1008);
    }
  }

  LcdDrawEnd();
#undef Y
#undef X
}
