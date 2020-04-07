/*---------------------------------------------------------
  ZeroCalibration.c
---------------------------------------------------------*/

#include "ZeroCalibration.h"
#include "IonCalibration.h"
#include "lcd.h"
#include "cfg.h"
#include "MeasureAmplifier.h"
#include "adc.h"
#include "measure.h"
#include "keybrd.h"
#include "utils.h"

#define ZEROCALIBRATION_VALUE_MAX   9999
#define ZEROCALIBRATION_VALUE_NOM   (IONCALIBRATION_NOM/2)

#define ZEROCALIBRATION_VALUE_X1    92
#define ZEROCALIBRATION_VALUE_X2    \
 (ZEROCALIBRATION_VALUE_X1 + ZEROCALIBRATION_VALUE_W + 6)
#define ZEROCALIBRATION_VALUE_Y     120
#define ZEROCALIBRATION_VALUE_W     88
#define ZEROCALIBRATION_Y1          200
#define ZEROCALIBRATION_X2          (LCD_W/2 - ZEROCALIBRATION_W2/2)
#define ZEROCALIBRATION_Y2          (ZEROCALIBRATION_Y1 + 20)
#define ZEROCALIBRATION_W2          100
#define ZEROCALIBRATION_H2          20


static NOINIT WORD ZeroCalibrationValue;
static NOINIT BYTE ZeroCalibrationPos;
static const TLcdRect ZeroCalibrationCurrValueRect = {
  ZEROCALIBRATION_X2,
  ZEROCALIBRATION_Y2,
  ZEROCALIBRATION_X2 + ZEROCALIBRATION_W2,
  ZEROCALIBRATION_Y2 + ZEROCALIBRATION_H2
};

//---------------------------------------------------------
void InitZeroCalibration(void)
{
  ZeroCalibrationPos = 0;
  LoadFirstProducedZeroCalibration();
}

//---------------------------------------------------------
void ShowZeroCalibrationIni(void)
{
  PrgVerb = VB_ZEROCALIBRATION;
  LcdRepaint();
  MeasureOnEnter();
  MeasureAutoAmplifierEnabled = FALSE;
  MeasureAmplifierSetKmin();
}

//---------------------------------------------------------
void ShowZeroCalibration(void)
{
  BOOL f = FALSE;
  FLOAT32 tmp;

  if(ProcessStandardKeyDownActions() == TRUE) {
    ZeroCalibrationValue = LcdSpinEdit.Value;
    ZeroCalibrationPos = LcdSpinEdit.Pos;
    SaveZeroCalibration();
  }

  if(KeyDown == KB_F1) {
    tmp = MeasureAdc*AdcGetK()*1000.0f;
    if (tmp > ZEROCALIBRATION_VALUE_MAX)
      tmp = ZEROCALIBRATION_VALUE_MAX;
    ZeroCalibrationValue = (WORD)tmp;
    LcdSpinEdit.Value = ZeroCalibrationValue;
    f = TRUE;
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspaceWithLangCaption(241);

    LcdSpinEditCreate(NULL);
    LcdSpinEdit.Comma = 3;
    LcdSpinEdit.TextLen = 5;
    LcdSpinEdit.Value = ZeroCalibrationValue;
    LcdSpinEdit.Max = ZEROCALIBRATION_VALUE_MAX;
    LcdSpinEdit.X = ZEROCALIBRATION_VALUE_X1;
    LcdSpinEdit.Y = ZEROCALIBRATION_VALUE_Y;
    LcdSpinEdit.W = ZEROCALIBRATION_VALUE_W;
    LcdSpinEdit.Pos = ZeroCalibrationPos;
    LcdSpinEdit.EnableLR = TRUE;
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdSetLabelTextColorDef();
    LcdSetFont(LCD_ITEM_FONT);
    LcdDrawRaLangText(ZEROCALIBRATION_VALUE_X1 - 6, ZEROCALIBRATION_VALUE_Y + 15,
     679);
    LcdDrawLaLangText(ZEROCALIBRATION_VALUE_X2, ZEROCALIBRATION_VALUE_Y + 15,
     925);

    LcdSetFont(FONT08);
    LcdDrawCaLangText(LCD_W/2, ZEROCALIBRATION_Y1, 247);

    LcdDrawLangHint(245, 0, 0);

    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || LcdFlags.Draw || f == TRUE) {
    BYTE n;
    LcdDrawBegin();

    LcdSetLabelTextColorDef();
    LcdSetFont(FONT10);
    n = UtoStrU(LcdText, MeasureAdc*AdcGetK());
    n += LoadLangResource(LcdText + n, 890);
    LcdDrawCaRectText(&ZeroCalibrationCurrValueRect, 0, LcdText, n);

    if(f == TRUE)
      LcdSpinEditDraw(NULL);

    LcdDrawEnd();
  }
  else
    LcdSpinEditDo(NULL);

  if(PrgVerb != VB_ZEROCALIBRATION)
    MeasureOnExit();
}

//---------------------------------------------------------
void SaveZeroCalibration(void)
{
  CfgWriteWord(CFG_ZEROCALIBRATION_VALUE, ZeroCalibrationValue);
}

//---------------------------------------------------------
void LoadZeroCalibration(void)
{
  WORD w = CfgReadWord(CFG_ZEROCALIBRATION_VALUE);
  if(w <= ZEROCALIBRATION_VALUE_MAX)
    ZeroCalibrationValue = w;
}

//---------------------------------------------------------
void LoadFirstProducedZeroCalibration(void)
{
  ZeroCalibrationValue = ZEROCALIBRATION_VALUE_NOM;
}

//---------------------------------------------------------
WORD ZeroCalibrationGetCode(void)
{
  WORD code = (WORD)(ZeroCalibrationValue*0.001f/AdcGetK());
  if(code > ADC_CODE_MAX)
    return ADC_CODE_MAX;
  return code;
}


