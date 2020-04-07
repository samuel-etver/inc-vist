/*---------------------------------------------------------
  MeasureHardwareTest.c
---------------------------------------------------------*/

#include "MeasureHardwareTest.h"
#include "MeasureAmplifier.h"
#include "HardwareFilter.h"
#include "measure.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"
#include "utils.h"

#define MEASUREHARDWARETEST_K_MAX         10U
#define MEASUREHARDWARETEST_K_NOM         0U

#define MEASUREHARDWARETEST_F_MIN         1U
#define MEASUREHARDWARETEST_F_MAX         9999U
#define MEASUREHARDWARETEST_F_NOM         100U

#define MEASUREHARDWARETEST_X1            80U
#define MEASUREHARDWARETEST_X2            172U
#define MEASUREHARDWARETEST_Y1            50U
#define MEASUREHARDWARETEST_Y2            90U
#define MEASUREHARDWARETEST_Y3            130U
#define MEASUREHARDWARETEST_Y4            175U
#define MEASUREHARDWARETEST_Y5            200U
#define MEASUREHARDWARETEST_Y6            225U
#define MEASUREHARDWARETEST_Y7            250U
#define MEASUREHARDWARETEST_Y8            275U

static const WORD MeasureHardwareTestLabelIds[] = {
  295, 293, 297, 617, 613, 677, 687, 689
};
static const WORD MeasureHardwareTestUnitIds[] = {
    0, 657,   0, 659, 657, 855,   0,   0
};
static const WORD MeasureHardwareTestLabelYs[] = {
  MEASUREHARDWARETEST_Y1,
  MEASUREHARDWARETEST_Y2,
  MEASUREHARDWARETEST_Y3,
  MEASUREHARDWARETEST_Y4,
  MEASUREHARDWARETEST_Y5,
  MEASUREHARDWARETEST_Y6,
  MEASUREHARDWARETEST_Y7,
  MEASUREHARDWARETEST_Y8
};

static NOINIT BYTE MeasureHardwareTestTab;
static NOINIT BYTE MeasureHardwareTestK;
static NOINIT WORD MeasureHardwareTestF;
static NOINIT TMeasureParam MeasureHardwareTestParam;
static NOINIT BYTE MeasureHardwareTestFPos;

static void MeasureHardwareTestGetEdit(void);
static void MeasureHardwareTestSetEdit(void);
static BYTE MeasureHardwareTestKtoStr(BYTE* buff, BYTE index);
static BYTE* MeasureHardwareTestKGetText(void* o);
static WORD MeasureHardwareTestGetParamIndex(void);

//---------------------------------------------------------
void InitMeasureHardwareTest(void)
{
  MeasureHardwareTestTab = 0;
  MeasureHardwareTestFPos = 0;
  LoadFirstProducedMeasureHardwareTest();
}

//---------------------------------------------------------
void ShowMeasureHardwareTestIni(void)
{
  PrgVerb = VB_MEASUREHARDWARETEST;
  MeasureOnEnter();
  MeasureAutoAmplifierEnabled = FALSE;
  MeasureAmplifierSetKbyIndex(MeasureHardwareTestK);
  HardwareFilterSetFrequency(MeasureHardwareTestF);
  MeasureSetParamOn(MeasureHardwareTestParam);
  LcdRepaint();
}

//---------------------------------------------------------
void ShowMeasureHardwareTest(void)
{
  BYTE tab_changed = FALSE;
  TLcdSpinEdit* se = &LcdSpinEdit;
  WORD i;
  BYTE n;
  BOOL redraw;
  BOOL changed;

  MeasureMeasure();

  if(ProcessStandardKeyDownActions() == TRUE) {
    MeasureHardwareTestGetEdit();
    SaveMeasureHardwareTest();
  }

  if(KeyDown == KB_F1) {
    switch(MeasureHardwareTestParam) {
      case mpSpp:
        MeasureHardwareTestParam = mpVrms;
        break;
      case mpVrms:
        MeasureHardwareTestParam = mpAamp;
        break;
      case mpAamp:
        MeasureHardwareTestParam = mpSpp;
      default:
        ;
    }
    MeasureSetParamOn(MeasureHardwareTestParam);
  }

  if(KeyDown == KB_UP || KeyDown == KB_DOWN) {
    MeasureHardwareTestGetEdit();
    if(++MeasureHardwareTestTab == 2)
      MeasureHardwareTestTab = 0;
    tab_changed = TRUE;
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspace();

    LcdSetFont(FONT10);

    for(i = 0; i < ARRAY_LENGTH(MeasureHardwareTestLabelIds); i++) {
      if(i & 1)
        LcdSetEvenRowTextColorDef();
      else
        LcdSetTextColorDef();
      LcdDrawRaLangText(MEASUREHARDWARETEST_X1, MeasureHardwareTestLabelYs[i],
       MeasureHardwareTestLabelIds[i]);
      LcdDrawLaLangText(MEASUREHARDWARETEST_X2 + 4U,
       MeasureHardwareTestLabelYs[i], MeasureHardwareTestUnitIds[i]);
    }

    LcdSpinEditCreate(se);
    MeasureHardwareTestSetEdit();

    LcdDrawLangHint(128, 0, 0);

    LcdDrawEnd();
  }
  else if(tab_changed == TRUE) {
    LcdDrawBegin();
    LcdSpinEditUnmap(se);
    MeasureHardwareTestSetEdit();
    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || tab_changed == TRUE) {
    LcdDrawBegin();

    LcdSetFont(FONT10);
    LcdSetLetterSpacing(se->Edit.LetterSpacing);

    for(i = 0; i < 2; i++) {
      if(i != MeasureHardwareTestTab) {
        if(i & 1)
          LcdSetEvenRowTextColorDef();
        else
          LcdSetTextColorDef();
        switch(i) {
          case 0:
            n = FloatToStr(LcdText,
             MeasureAmplifierGetKbyIndex(MeasureHardwareTestK), 0);
            break;
          default:
            WordToStrWithLeadingZeroes(LcdText, MeasureHardwareTestF, n = 4);
            n = StripDecimal(LcdText, n);
        }
        LcdDrawRaText(MEASUREHARDWARETEST_X2 - 2U, MeasureHardwareTestLabelYs[i],
         LcdText, n);
      }
    }

    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || LcdFlags.Draw) {
    redraw = TO_BOOL(LcdFlags.Repaint);

    LcdDrawBegin();

    LcdSetFont(FONT10);

    changed = redraw;
    if(TriggerSetValueWord(&MeasureTrigger1, MeasureHardwareTestParam) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = LoadLangResource(LcdText, 303 + MeasureHardwareTestGetParamIndex());
      LcdRect3.Right = MEASUREHARDWARETEST_X2;
      LcdRect3.Top   = MEASUREHARDWARETEST_Y3;
      LcdSetEvenRowTextColorDef();
      LcdDrawRaAutoSizedRectText(&LcdRect3, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueFloat(&MeasureTTrigger, MeasureT) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = MeasureTtoStr(LcdText, MeasureT, TRUE);
      MeasureTRect.Right = MEASUREHARDWARETEST_X2;
      MeasureTRect.Top   = MEASUREHARDWARETEST_Y4;
      LcdSetEvenRowTextColorDef();
      LcdDrawRaAutoSizedRectText(&MeasureTRect, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueFloat(&MeasureFTrigger, MeasureF) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = MeasureFtoStr(LcdText, MeasureF, TRUE);
      MeasureFRect.Right = MEASUREHARDWARETEST_X2;
      MeasureFRect.Top   = MEASUREHARDWARETEST_Y5;
      LcdSetTextColorDef();
      LcdDrawRaAutoSizedRectText(&MeasureFRect, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueFloat(&MeasureNoiseTrigger, MeasureNoise) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = MeasureNoiseToStr(LcdText, MeasureNoise, TRUE);
      MeasureNoiseRect.Right = MEASUREHARDWARETEST_X2;
      MeasureNoiseRect.Top   = MEASUREHARDWARETEST_Y6;
      LcdSetEvenRowTextColorDef();
      LcdDrawRaAutoSizedRectText(&MeasureNoiseRect, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueWord(&MeasureTrigger1, MeasureAdcValueMin) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = WordToStr(LcdText, MeasureAdcValueMin);
      LcdRect1.Right = MEASUREHARDWARETEST_X2;
      LcdRect1.Top   = MEASUREHARDWARETEST_Y7;
      LcdSetTextColorDef();
      LcdDrawRaAutoSizedRectText(&LcdRect1, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueWord(&MeasureTrigger2, MeasureAdcValueMax) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = WordToStr(LcdText, MeasureAdcValueMax);
      LcdRect2.Right = MEASUREHARDWARETEST_X2;
      LcdRect2.Top   = MEASUREHARDWARETEST_Y8;
      LcdSetEvenRowTextColorDef();
      LcdDrawRaAutoSizedRectText(&LcdRect2, LcdText, n, redraw);
    }

    LcdDrawEnd();
  }

  LcdSpinEditDo(se);

  if(LcdFlags.Draw) {
    WORD old_f = MeasureHardwareTestF;
    BYTE old_k = MeasureHardwareTestK;

    MeasureHardwareTestGetEdit();

    if(old_f != MeasureHardwareTestF)
      HardwareFilterSetFrequency(MeasureHardwareTestF);
    if(old_k != MeasureHardwareTestK)
      MeasureAmplifierSetKbyIndex(MeasureHardwareTestK);
  }

  if(PrgVerb != VB_MEASUREHARDWARETEST)
    MeasureOnExit();
}

//---------------------------------------------------------
void LoadMeasureHardwareTest(void)
{
  WORD w;

  w = CfgReadByte(CFG_MEASUREHARDWARETEST_K);
  if(w <= MEASUREHARDWARETEST_K_MAX)
    MeasureHardwareTestK = (BYTE)w;

  w = CfgReadWord(CFG_MEASUREHARDWARETEST_F);
  if(w >= MEASUREHARDWARETEST_F_MIN && w <= MEASUREHARDWARETEST_F_MAX)
    MeasureHardwareTestF = w;

  w = CfgReadByte(CFG_MEASUREHARDWARETEST_PARAM);
  if(w < MEASURE_PARAMS_COUNT)
    MeasureHardwareTestParam = (TMeasureParam)w;
}

//---------------------------------------------------------
void SaveMeasureHardwareTest(void)
{
  CfgWriteByte(CFG_MEASUREHARDWARETEST_K, MeasureHardwareTestK);
  CfgWriteWord(CFG_MEASUREHARDWARETEST_F, MeasureHardwareTestF);
  CfgWriteByte(CFG_MEASUREHARDWARETEST_PARAM, MeasureHardwareTestParam);
}

//---------------------------------------------------------
void LoadFirstProducedMeasureHardwareTest(void)
{
  MeasureHardwareTestK = MEASUREHARDWARETEST_K_NOM;
  MeasureHardwareTestF = MEASUREHARDWARETEST_F_NOM;
  MeasureHardwareTestParam = mpSpp;
}

//---------------------------------------------------------
static void MeasureHardwareTestGetEdit(void)
{
  WORD w = (WORD)LcdSpinEdit.Value;
  switch(MeasureHardwareTestTab) {
    case 0:
      MeasureHardwareTestK = (BYTE)w;
      break;
    case 1:
      MeasureHardwareTestF = w;
      MeasureHardwareTestFPos = LcdSpinEdit.Pos;
  }
}

//---------------------------------------------------------
static void MeasureHardwareTestSetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;
  WORD y;
  WORD w = 60U;
  WORD val;
  WORD min;
  WORD max;
  BYTE txt_len;
  BYTE* (*get_text)(void*) = NULL;
  BYTE pos = 0;

  switch(MeasureHardwareTestTab) {
    case 0:
      y = MEASUREHARDWARETEST_Y1;
      min = 0;
      max = MeasureAmplifierGetIndexMax();
      val = MeasureHardwareTestK;
      txt_len = 3U;
      get_text = MeasureHardwareTestKGetText;
      break;
    default:
      y = MEASUREHARDWARETEST_Y2;
      min = MEASUREHARDWARETEST_F_MIN;
      max = MEASUREHARDWARETEST_F_MAX;
      val = MeasureHardwareTestF;
      txt_len = 4U;
      pos = MeasureHardwareTestFPos;
  }
  se->X = MEASUREHARDWARETEST_X2 - w;
  se->Y = y - 15U;
  se->W = w;
  se->Min = min;
  se->Max = max;
  se->Value = val;
  se->TextLen = txt_len;
  se->Edit.Font = FONT10;
  se->Edit.Balloon = LcdEditSmallBalloon;
  se->GetText = get_text;
  se->Wrap = TRUE;
  se->EnableLR = TRUE;
  se->Pos = pos;

  LcdSpinEditSetup(se);
  LcdSpinEditDraw(se);
}

//---------------------------------------------------------
static BYTE MeasureHardwareTestKtoStr(BYTE* buff, BYTE index)
{
  return FloatToStr(buff, MeasureAmplifierGetKbyIndex(index), 0);
}

//---------------------------------------------------------
static BYTE* MeasureHardwareTestKGetText(void* o)
{
  LcdText[0] = MeasureHardwareTestKtoStr(LcdText + 1, LcdSpinEdit.Value);
  return LcdText;
}

//---------------------------------------------------------
static WORD MeasureHardwareTestGetParamIndex(void)
{
  switch(MeasureHardwareTestParam) {
    case mpVrms: return 1;
    case mpAamp: return 2;
    default:     return 0;
  }
}
