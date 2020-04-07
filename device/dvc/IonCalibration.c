/*---------------------------------------------------------
  IonCalibration.c
---------------------------------------------------------*/

#include "IonCalibration.h"
#include "language.h"
#include "lcd.h"
#include "cfg.h"
#include "adc.h"
#include "factor.h"

#define IONCALIBRATION_MAX            5000
#define IONCALIBRATION_COMMA          3
#define IONCALIBRATION_SIZE           5

#define IONCALIBRATION_W              100
#define IONCALIBRATION_X0             (IONCALIBRATION_X1 - 6)
#define IONCALIBRATION_X1             110
#define IONCALIBRATION_X2             (IONCALIBRATION_X1 + IONCALIBRATION_W + 6)
#define IONCALIBRATION_Y              (LCD_H/2 - 20)

static NOINIT WORD IonCalibration;
static NOINIT BYTE IonCalibrationPos;

//---------------------------------------------------------
void InitIonCalibration(void)
{
  IonCalibrationPos = 0;
  LoadFirstProducedIonCalibration();
}

//---------------------------------------------------------
void ShowIonCalibrationIni(void)
{
  PrgVerb = VB_IONCALIBRATION;
  LcdRepaint();
}

//---------------------------------------------------------
void ShowIonCalibration(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    IonCalibration    = LcdSpinEdit.Value;
    IonCalibrationPos = LcdSpinEdit.Pos;
    SaveIonCalibration();
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspace();
    LcdDrawLangCaptionN(869, LangId == lRussian ? 3 : 2);

    LcdSpinEditCreate(NULL);
    LcdSpinEdit.Comma = IONCALIBRATION_COMMA;
    LcdSpinEdit.TextLen = IONCALIBRATION_SIZE;
    LcdSpinEdit.Value = IonCalibration;
    LcdSpinEdit.Max = IONCALIBRATION_MAX;
    LcdSpinEdit.X = IONCALIBRATION_X1;
    LcdSpinEdit.Y = IONCALIBRATION_Y;
    LcdSpinEdit.W = IONCALIBRATION_W;
    LcdSpinEdit.Pos = IonCalibrationPos;
    LcdSpinEdit.EnableLR = TRUE;
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdSetLabelTextColorDef();    
    LcdSetFont(LCD_ITEM_FONT);
    LcdDrawRaLangText(IONCALIBRATION_X0, IONCALIBRATION_Y + 15, 801);
    LcdDrawLaLangText(IONCALIBRATION_X2, IONCALIBRATION_Y + 15, 925);

    LcdDrawEnd();
  }
  else
    LcdSpinEditDo(NULL);
}

//---------------------------------------------------------
void LoadIonCalibration(void)
{
  WORD w = CfgReadWord(CFG_IONCALIBRATION);
  if (w <= IONCALIBRATION_MAX)
    IonCalibration = w;
}

//---------------------------------------------------------
void SaveIonCalibration(void)
{
  CfgWriteWord(CFG_IONCALIBRATION, IonCalibration);
}

//---------------------------------------------------------
void LoadFirstProducedIonCalibration(void)
{
  IonCalibration = IONCALIBRATION_NOM;
}

//---------------------------------------------------------
FLOAT32 IonCalibrationGet(void)
{
  FLOAT32 u = IonCalibration*0.001f;
  return u < 1.0f ? 1.0f : u;
}
