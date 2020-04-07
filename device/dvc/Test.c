/*---------------------------------------------------------
  test.c
---------------------------------------------------------*/

#include "test.h"
#include "lcd.h"

//---------------------------------------------------------
void TestPreparingIni(void)
{
  LcdDrawBegin();

  LcdDrawWorkspace();
  LcdDrawCaLangText(LCD_W/2, (LCD_H)/3, 909);

  LcdDrawEnd();

  TempWord = 0;
}

//---------------------------------------------------------
BOOL TestPreparing(void)
{
  if(PrgFlags.CentiSec)
    ++TempWord;
  return TO_BOOL(TempWord == 20);
}

//---------------------------------------------------------
void TestMeasuringIni(void)
{
  LcdDrawBegin();

  LcdDrawWorkspaceWithLangCaption(647);

  LcdDrawEnd();

  LcdRepaint();

  TempBool = FALSE;
  TempWord = 0;
}

//---------------------------------------------------------
void TestMeasuring(void)
{
  if(PrgFlags.CentiSec) {
    if(TempWord < 8)
      if(++TempWord == 7)
        TempBool = TRUE;
  }
}

