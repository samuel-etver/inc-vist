/*---------------------------------------------------------
  MeasuringTract.c
---------------------------------------------------------*/

#include "MeasuringTract.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"
#include "HardwareFilter.h"
#include "MeasureAmplifier.h"
#include "IonCalibration.h"
#include "adc.h"
#include "factor.h"
#include "language.h"
#include <math.h>

#define MEASURINGTRACT_FS_MIN         1U
#define MEASURINGTRACT_FS_MAX         9999U
#define MEASURINGTRACT_FS_NOM         100U

#define MEASURINGTRACT_X1             70U
#define MEASURINGTRACT_X2             (MEASURINGTRACT_X1 + 4U)
#define MEASURINGTRACT_X2_KS          (MEASURINGTRACT_X2 + 2)
#define MEASURINGTRACT_X2_FS          (MEASURINGTRACT_X2 + 68U)
#define MEASURINGTRACT_X3             200U

#define MEASURINGTRACT_K_Y            35U
#define MEASURINGTRACT_F_Y            65U
#define MEASURINGTRACT_UPP_Y          95U
#define MEASURINGTRACT_X_A0_Y         135U
#define MEASURINGTRACT_X_A1_Y         175U
#define MEASURINGTRACT_FS_Y           215U

static const WORD MeasuringTractLabelIds[] = {
  884, 886, 888, 892, 931, 923
};
static const WORD MeasuringTractUnitIds[] = {
    0, 657, 925,   0,   0, 657
};

static const BYTE MeasuringTractAsAddr[] = {
  CFG_MEASURINGTRACT_S_A0,
  CFG_MEASURINGTRACT_S_A1
};
static const BYTE MeasuringTractAvAddr[] = {
  CFG_MEASURINGTRACT_V_A0,
  CFG_MEASURINGTRACT_V_A1
};
static const BYTE MeasuringTractAaAddr[] = {
  CFG_MEASURINGTRACT_A_A0,
  CFG_MEASURINGTRACT_A_A1
};

static const WORD MeasuringTractLabelYs[] = {
  MEASURINGTRACT_K_Y,
  MEASURINGTRACT_F_Y,
  MEASURINGTRACT_UPP_Y,
  MEASURINGTRACT_X_A0_Y,
  MEASURINGTRACT_X_A1_Y,
  MEASURINGTRACT_FS_Y,
};

NOINIT FLOAT32 MeasuringTractAs[MEASURINGTRACT_X_FACTORS_COUNT];
NOINIT FLOAT32 MeasuringTractAv[MEASURINGTRACT_X_FACTORS_COUNT];
NOINIT FLOAT32 MeasuringTractAa[MEASURINGTRACT_X_FACTORS_COUNT];
NOINIT WORD MeasuringTractFs;
static NOINIT TMeasureParam MeasuringTractParam;
#define COUNT MEASURINGTRACT_X_FACTORS_COUNT
#define SIZE  MEASURINGTRACT_X_FACTOR_SIZE
NOINIT BYTE MeasuringTractAsFactor[COUNT][SIZE];
NOINIT BYTE MeasuringTractAvFactor[COUNT][SIZE];
NOINIT BYTE MeasuringTractAaFactor[COUNT][SIZE];
#undef COUNT
#undef SIZE
static NOINIT BYTE MeasuringTractKsPos;
static NOINIT BYTE MeasuringTractFsPos;
static NOINIT BYTE MeasuringTractTab;

static void MeasuringTractSetEdit(void);
static void MeasuringTractGetEdit(void);
static BYTE MeasuringTractUppToStr(BYTE* buff, FLOAT32 value);
static BYTE MeasuringTractFsToStr(BYTE* buff, WORD value);
static WORD MeasuringTractGetParamIndex(void);

//---------------------------------------------------------
void InitMeasuringTract(void)
{
  MeasuringTractTab = 0;
  MeasuringTractKsPos = 0;
  MeasuringTractFsPos = 0;
  MeasuringTractParam = mpSpp;
  LoadFirstProducedMeasuringTract();
}

//---------------------------------------------------------
void ShowMeasuringTractIni(void)
{
  PrgVerb = VB_MEASURINGTRACT;
  MeasureOnEnter();
  MeasureSetParamOn(MeasuringTractParam);
  LcdRepaint();
}

//---------------------------------------------------------
void ShowMeasuringTract(void)
{
  BOOL tab_changed = FALSE;
  TLcdSpinEdit* se = &LcdSpinEdit;
  BYTE n;
  BOOL redraw;
  BOOL changed;
  WORD i;
  FLOAT32 k;

  MeasureMeasure();

  if(ProcessStandardKeyDownActions() == TRUE) {
    MeasuringTractGetEdit();
    MeasuringTractCalc();
    SaveMeasuringTract();
  }

  if(KeyDown == KB_UP) {
    MeasuringTractGetEdit();
    if(MeasuringTractTab-- == 0)
      MeasuringTractTab = 2;
    tab_changed = TRUE;
  }

  if(KeyDown == KB_DOWN) {
    MeasuringTractGetEdit();
    if(++MeasuringTractTab == 3)
      MeasuringTractTab = 0;
    tab_changed = TRUE;
  }

  if(KeyDown == KB_F1) {
    switch(MeasuringTractParam) {
      case mpSpp:
        MeasuringTractParam = mpVrms;
        break;
      case mpVrms:
        MeasuringTractParam = mpAamp;
        break;
      case mpAamp:
        MeasuringTractParam = mpSpp;
      default:
        ;
    }
    MeasureSetParamOn(MeasuringTractParam);
    LcdRepaint();
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspace();

    LcdSetFont(FONT10);

    for(i = 0; i < ARRAY_LENGTH(MeasuringTractLabelIds); i++) {
      if(i & 1)
        LcdSetEvenRowTextColorDef();
      else
        LcdSetTextColorDef();
      LcdDrawRaLangText(MEASURINGTRACT_X1, MeasuringTractLabelYs[i],
       MeasuringTractLabelIds[i] +
       (MeasuringTractLabelIds[i] == 931 ?
        LANG_COUNT*(WORD)MeasuringTractGetParamIndex() : 0U) +
       (MeasuringTractLabelIds[i] == 892 ?
        LANG_COUNT*(WORD)MeasuringTractGetParamIndex() : 0U));
      LcdDrawLaLangText(MEASURINGTRACT_X3 + 4U,
       MeasuringTractLabelYs[i], MeasuringTractUnitIds[i]);
    }

    LcdSpinEditCreate(se);
    MeasuringTractSetEdit();

    LcdDrawLangHint(128, 0, 0);

    LcdDrawEnd();
  }
  else if(tab_changed == TRUE) {
    LcdDrawBegin();
    LcdSpinEditUnmap(se);
    MeasuringTractSetEdit();
    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || tab_changed == TRUE) {
    LcdDrawBegin();

    LcdSetFont(FONT10);
    LcdSetLetterSpacing(se->Edit.LetterSpacing);

    if(MeasuringTractTab != 0) {
      LcdSetEvenRowTextColorDef();
      switch(MeasuringTractParam) {
        case mpSpp:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAsFactor[0]); break;
        case mpVrms:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAvFactor[0]); break;
        case mpAamp:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAaFactor[0]); break;
        default:
          n = 0;
      }
      LcdDrawLaFactor(MEASURINGTRACT_X2_KS + 2U, MEASURINGTRACT_X_A0_Y, LcdText, n);
    }

    if(MeasuringTractTab != 1) {
      LcdSetTextColorDef();
      switch(MeasuringTractParam) {
        case mpSpp:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAsFactor[1]); break;
        case mpVrms:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAvFactor[1]); break;
        case mpAamp:
          n = MeasuringTractAxToStr(LcdText, MeasuringTractAaFactor[1]); break;
        default:
          n = 0;
      }
      LcdDrawLaFactor(MEASURINGTRACT_X2_KS + 2U, MEASURINGTRACT_X_A1_Y, LcdText, n);
    }

    if(MeasuringTractTab != 2) {
      LcdSetEvenRowTextColorDef();
      LcdDrawRaText(MEASURINGTRACT_X3, MEASURINGTRACT_FS_Y, LcdText,
       MeasuringTractFsToStr(LcdText, MeasuringTractFs));
    }

    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || LcdFlags.Draw) {
    FLOAT32 upp;

    redraw = TO_BOOL(LcdFlags.Repaint);

    LcdDrawBegin();

    LcdSetFont(FONT10);

    k = MeasureAmplifierGetK();
    changed = redraw;
    if(TriggerSetValueFloat(&MeasureTrigger1, k) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = FloatToStr(LcdText, k, 1);
      LcdRect1.Right = MEASURINGTRACT_X3;
      LcdRect1.Top   = MEASURINGTRACT_K_Y;
      LcdSetTextColorDef();
      LcdDrawRaAutoSizedRectText(&LcdRect1, LcdText, n, redraw);
    }

    changed = redraw;
    if(TriggerSetValueFloat(&MeasureFTrigger, MeasureF) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = MeasureFtoStr(LcdText, MeasureF, TRUE);
      MeasureFRect.Right = MEASURINGTRACT_X3;
      MeasureFRect.Top   = MEASURINGTRACT_F_Y;
      LcdSetEvenRowTextColorDef();
      LcdDrawRaAutoSizedRectText(&MeasureFRect, LcdText, n, redraw);
    }


    upp = fabs((INT16)MeasureAdcValueMax - (INT16)MeasureAdcValueMin)*
     IonCalibrationGet()/ADC_CODE_MAX;
    changed = redraw;
    if(TriggerSetValueFloat(&MeasureTrigger2, upp) == TRUE) changed = TRUE;
    if(changed == TRUE) {
      n = MeasuringTractUppToStr(LcdText, upp);
      LcdRect2.Right = MEASURINGTRACT_X3;
      LcdRect2.Top   = MEASURINGTRACT_UPP_Y;
      LcdSetTextColorDef();
      LcdDrawRaAutoSizedRectText(&LcdRect2, LcdText, n, redraw);
    }
  }

  LcdSpinEditDo(se);

  if(LcdFlags.Draw) {
    WORD old_f = MeasuringTractFs;

    MeasuringTractGetEdit();

    if(old_f != MeasuringTractFs)
      HardwareFilterSetFrequency(MeasuringTractFs);
  }

  if(PrgVerb != VB_MEASURINGTRACT) {
    MeasureOnExit();
  }
}

//---------------------------------------------------------
void LoadMeasuringTract(void)
{
  WORD i;
  WORD w = CfgReadWord(CFG_MEASURINGTRACT_FS);
  if(w <= MEASURINGTRACT_FS_MAX) {
#if MEASURINGTRACT_FS_MIN != 0
    if(w >= MEASURINGTRACT_FS_MIN)
      MeasuringTractFs = w;
#else
    MeasuringTractFs = w;
#endif
  }

  for(i = 0; i < MEASURINGTRACT_X_FACTORS_COUNT; i++) {
    CfgReadFactor(MeasuringTractAsAddr[i], MeasuringTractAsFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
    CfgReadFactor(MeasuringTractAvAddr[i], MeasuringTractAvFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
    CfgReadFactor(MeasuringTractAaAddr[i], MeasuringTractAaFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
  }
  MeasuringTractCalc();
}

//---------------------------------------------------------
void SaveMeasuringTract(void)
{
  WORD i;
  CfgWriteWord(CFG_MEASURINGTRACT_FS, MeasuringTractFs);
  for(i = 0; i < MEASURINGTRACT_X_FACTORS_COUNT; i++) {
    CfgWriteFactor(MeasuringTractAsAddr[i], MeasuringTractAsFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE);
    CfgWriteFactor(MeasuringTractAvAddr[i], MeasuringTractAvFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE);
    CfgWriteFactor(MeasuringTractAaAddr[i], MeasuringTractAaFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE);
  }
}

//---------------------------------------------------------
void LoadFirstProducedMeasuringTract(void)
{
  memcpy(MeasuringTractAsFactor[0], "+0000+00", MEASURINGTRACT_X_FACTOR_SIZE);
  memcpy(MeasuringTractAsFactor[1], "+1000+00", MEASURINGTRACT_X_FACTOR_SIZE);
  memcpy(MeasuringTractAvFactor, MeasuringTractAsFactor,
   MEASURINGTRACT_X_FACTORS_COUNT*MEASURINGTRACT_X_FACTOR_SIZE);
  memcpy(MeasuringTractAaFactor, MeasuringTractAsFactor,
   MEASURINGTRACT_X_FACTORS_COUNT*MEASURINGTRACT_X_FACTOR_SIZE);
  MeasuringTractFs = MEASURINGTRACT_FS_NOM;
}

//---------------------------------------------------------
static void MeasuringTractSetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;
  BYTE txt_len;
  INT32 min;
  INT32 max;
  INT32 val;
  BYTE pos;
  BYTE comma = 0;
  WORD y;
  BYTE* exp = NULL;
  WORD x;
  TColor txt_color;
  WORD w;

  switch(MeasuringTractTab) {
    case 0:
    case 1:
      x = MEASURINGTRACT_X2_KS + 2U;
      y = !MeasuringTractTab ? MEASURINGTRACT_X_A0_Y : MEASURINGTRACT_X_A1_Y;
      w = MEASURINGTRACT_X3 - x + 2U;
      txt_len = MEASURINGTRACT_X_FACTOR_SIZE + 2;
      min = 1;
      max = 9999;
      val = 1;
      pos = 0;
      comma = MEASURINGTRACT_X_FACTOR_COMMA;
      pos = MeasuringTractKsPos;
      switch(MeasuringTractParam) {
        case mpSpp:  exp = MeasuringTractAsFactor[MeasuringTractTab]; break;
        case mpVrms: exp = MeasuringTractAvFactor[MeasuringTractTab]; break;
        case mpAamp: exp = MeasuringTractAaFactor[MeasuringTractTab];
        default:;
      }
      txt_color = LcdEvenRowTextColorDef;
      break;
    default:
      x = MEASURINGTRACT_X2_FS;
      y = MEASURINGTRACT_FS_Y;
      w = MEASURINGTRACT_X3 - x;
      txt_len = 4;
      min = MEASURINGTRACT_FS_MIN;
      max = MEASURINGTRACT_FS_MAX;
      val = MeasuringTractFs;
      pos = MeasuringTractFsPos;
      txt_color = LcdTextColorDef;
      break;
  }

  se->InactiveTextColor = txt_color;
  se->W                 = w;
  se->X                 = x;
  se->Y                 = y - 15U;
  se->TextLen           = txt_len;
  se->Min               = min;
  se->Max               = max;
  se->Value             = val;
  se->EnableLR          = TRUE;
  se->Pos               = pos;
  se->Comma             = comma;
  se->Wrap              = FALSE;
  se->Exp               = exp;
  se->Edit.Font         = FONT10;
  se->Edit.Balloon      = LcdEditSmallBalloon;

  LcdSpinEditSetup(se);
  if(MeasuringTractTab == 0)
    LcdSpinEditUnmap(se);
  LcdSpinEditDraw(se);
}

//---------------------------------------------------------
static void MeasuringTractGetEdit(void)
{
  switch(MeasuringTractTab) {
    case 0:
    case 1:
      MeasuringTractKsPos = LcdSpinEdit.Pos;
      break;
    default:
      MeasuringTractFsPos = (BYTE)LcdSpinEdit.Pos;
      MeasuringTractFs = LcdSpinEdit.Value;
  }
}

//---------------------------------------------------------
static BYTE MeasuringTractUppToStr(BYTE* buff, FLOAT32 value)
{
  BYTE n;
  if(value > 99.999f) {
    memcpy((char*)buff, "**.***", n = 6);
  }
  else {
    n = (BYTE)sprintf((char*)buff, "%6.3f", value);
  }
  return n;
}

//---------------------------------------------------------
BYTE MeasuringTractAxToStr(BYTE* buff, BYTE* factor)
{
  return FactorToStr(buff, factor, MEASURINGTRACT_X_FACTOR_SIZE,
   MEASURINGTRACT_X_FACTOR_COMMA);
}

//---------------------------------------------------------
static BYTE MeasuringTractFsToStr(BYTE* buff, WORD value)
{
  WordToStrWithLeadingSpaces(LcdText, value, 4U);
  return 4U;
}

//---------------------------------------------------------
void MeasuringTractCalc(void)
{
  WORD i;
  for(i = 0; i < MEASURINGTRACT_X_FACTORS_COUNT; i++) {
    MeasuringTractAs[i] = FactorToFloat(MeasuringTractAsFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
    MeasuringTractAv[i] = FactorToFloat(MeasuringTractAvFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
    MeasuringTractAa[i] = FactorToFloat(MeasuringTractAaFactor[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
  }
}

//---------------------------------------------------------
static WORD MeasuringTractGetParamIndex(void)
{
  switch(MeasuringTractParam) {
    case mpVrms: return 1;
    case mpAamp: return 2;
    default:     return 0;
  }
}
