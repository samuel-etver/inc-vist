/*---------------------------------------------------------
  VistCalibration.c
---------------------------------------------------------*/

#include "VistCalibration.h"
#include "lcd.h"
#include "keybrd.h"
#include "VistMeasure.h"
#include "VistParams.h"
#include "VistSensor.h"
#include "MeasuringTract.h"
#include "MeasureAmplifier.h"
#include "measure.h"
#include "language.h"
#include "utils.h"
#include "factor.h"

#define VIST_CALIBRATION_EDIT_X1      77U
#define VIST_CALIBRATION_EDIT_X2      (VIST_CALIBRATION_EDIT_X1 + 8U)
#define VIST_CALIBRATION_EDIT_X3      212U
#define VIST_CALIBRATION_EDIT_A0_Y    140U
#define VIST_CALIBRATION_EDIT_A1_Y    220U

#define VIST_CALIBRATION_STEP_X1     64
#define VIST_CALIBRATION_STEP_X2     170
#define VIST_CALIBRATION_STEP_Y1     100
#define VIST_CALIBRATION_STEP_Y2     (VIST_CALIBRATION_STEP_Y1 + 38)
#define VIST_CALIBRATION_STEP_Y3     (VIST_CALIBRATION_STEP_Y1 + 2*38)
#define VIST_CALIBRATION_STEP_Y4     (VIST_CALIBRATION_STEP_Y1 + 3*38)
#define VIST_CALIBRATION_STEP_Y5     (VIST_CALIBRATION_STEP_Y1 + 4*38)

static const WORD VistCalibrationLabelIds[] = {
  892, 931
};

static const WORD VistCalibrationLabelYs[] = {
  VIST_CALIBRATION_EDIT_A0_Y, VIST_CALIBRATION_EDIT_A1_Y,
};

static const WORD VistCalibrationParamIds[] = {
  937, 860, 862
};

static NOINIT enum {
  VCV_EDIT_INI,
  VCV_EDIT,
  VCV_STEP1_DO_INI,
  VCV_STEP1_DO,
  VCV_STEP1_EDIT_INI,
  VCV_STEP1_EDIT,
  VCV_STEP2_DO_INI,
  VCV_STEP2_DO,
  VCV_STEP2_EDIT_INI,
  VCV_STEP2_EDIT,
  VCV_STEP2_ERROR_INI,
  VCV_STEP2_ERROR,
  VCV_DONE_INI,
  VCV_DONE
} VistCalibrationVerb;

static NOINIT TMeasureParam VistCalibrationParam;
static NOINIT BYTE VistCalibrationTab;
static NOINIT BYTE VistCalibrationPos;
static NOINIT BYTE VistCalibrationStep;
#define COUNT MEASURINGTRACT_X_FACTORS_COUNT
#define SIZE  MEASURINGTRACT_X_FACTOR_SIZE
static NOINIT BYTE VistCalibrationAsFactor[COUNT][SIZE];
static NOINIT BYTE VistCalibrationAvFactor[COUNT][SIZE];
static NOINIT BYTE VistCalibrationAaFactor[COUNT][SIZE];
static NOINIT FLOAT32 VistCalibrationFloatFactor[COUNT];
#undef COUNT
#undef SIZE
static NOINIT WORD VistCalibrationStepNS[2];
static NOINIT DWORD VistCalibrationStepNV[2];
static NOINIT BYTE VistCalibrationStepNA[2][8];
static NOINIT BYTE VistCalibrationStepNSPos;
static NOINIT BYTE VistCalibrationStepNVPos;
static NOINIT BYTE VistCalibrationStepNAPos;
static NOINIT FLOAT32 VistCalibrationStepNInputU[2];

static void VistCalibrationEditIni(void);
static void VistCalibrationEdit(void);
static void VistCalibrationStep1DoIni(void);
static void VistCalibrationStep1Do(void);
static void VistCalibrationStep1EditIni(void);
static void VistCalibrationStep1Edit(void);
static void VistCalibrationStep2DoIni(void);
static void VistCalibrationStep2Do(void);
static void VistCalibrationStep2EditIni(void);
static void VistCalibrationStep2Edit(void);
static void VistCalibrationStep2ErrorIni(void);
static void VistCalibrationStep2Error(void);
static void VistCalibrationDoneIni(void);
static void VistCalibrationDone(void);
static void VistCalibrationStepNDoIni(void);
static void VistCalibrationStepNDo(void);
static void VistCalibrationStepNEditIni(void);
static void VistCalibrationStepNEdit(void);

static void VistCalibrationGetEdit(void);
static void VistCalibrationSetEdit(void);
static void VistCalibrationStepNSetEdit(void);
static void VistCalibrationStepNGetEdit(void);
static void VistCalibrationSave(void);
static BOOL VistCalibrationHasError(void);
static void VistCalibrationCalc(void);
static void VistCalibrationCalcS(void);
static void VistCalibrationCalcV(void);
static void VistCalibrationCalcA(void);
static void VistCalibrationCalcImpl(FLOAT32* values,
 BYTE result[][MEASURINGTRACT_X_FACTOR_SIZE]);
static void VistCalibrationDrawStepCaption(void);
static WORD VistCalibrationGetParamIndex(void);

//---------------------------------------------------------
void InitVistCalibration(void)
{
  VistCalibrationParam = mpSpp;
  VistCalibrationTab = 0;
  VistCalibrationPos = 0;
  VistCalibrationStepNS[0] = 200;
  VistCalibrationStepNS[1] = 2000;
  VistCalibrationStepNSPos = 0;
  VistCalibrationStepNV[0] = 200;
  VistCalibrationStepNV[1] = 20000;
  VistCalibrationStepNVPos = 0;
  memcpy(VistCalibrationStepNA[0], "+1000+00", 8);
  memcpy(VistCalibrationStepNA[1], "+1000+00", 8);
  VistCalibrationStepNAPos = 0;
}

//---------------------------------------------------------
void ShowVistCalibrationIni(void)
{
  PrgVerb = VB_VIST_CALIBRATION;
  VistCalibrationVerb = VCV_EDIT_INI;
#define N sizeof(MeasuringTractAsFactor)
  memcpy(VistCalibrationAsFactor, MeasuringTractAsFactor, N);
  memcpy(VistCalibrationAvFactor, MeasuringTractAvFactor, N);
  memcpy(VistCalibrationAaFactor, MeasuringTractAaFactor, N);
#undef N

  MeasureOnEnter();
  MeasureSetParamOn(VistCalibrationParam);
}

//---------------------------------------------------------
void ShowVistCalibration(void)
{
  ProcessStandardKeyDownActions();

  MeasureMeasure();

  switch(VistCalibrationVerb) {
    case VCV_EDIT_INI:
      VistCalibrationEditIni();
    case VCV_EDIT:
      VistCalibrationEdit(); break;
    case VCV_STEP1_DO_INI:
      VistCalibrationStep1DoIni();
    case VCV_STEP1_DO:
      VistCalibrationStep1Do(); break;
    case VCV_STEP1_EDIT_INI:
      VistCalibrationStep1EditIni();
    case VCV_STEP1_EDIT:
      VistCalibrationStep1Edit(); break;
    case VCV_STEP2_DO_INI:
      VistCalibrationStep2DoIni();
    case VCV_STEP2_DO:
      VistCalibrationStep2Do(); break;
    case VCV_STEP2_EDIT_INI:
      VistCalibrationStep2EditIni();
    case VCV_STEP2_EDIT:
      VistCalibrationStep2Edit(); break;
    case VCV_STEP2_ERROR_INI:
      VistCalibrationStep2ErrorIni();
    case VCV_STEP2_ERROR:
      VistCalibrationStep2Error(); break;
    case VCV_DONE_INI:
      VistCalibrationDoneIni();
    case VCV_DONE:
      VistCalibrationDone();
  }

  if(PrgVerb != VB_VIST_CALIBRATION) {
    MeasureOnExit();
    VistCalibrationSave();
  }
}

//---------------------------------------------------------
static void VistCalibrationEditIni(void)
{
  VistCalibrationVerb = VCV_EDIT;
  LcdRepaint();
}

//---------------------------------------------------------
static void VistCalibrationEdit(void)
{
  BOOL tab_changed = FALSE;
  TLcdSpinEdit* se = &LcdSpinEdit;
  WORD i;
  WORD n;

  if(ProcessStandardKeyDownActions() == TRUE) {
    VistCalibrationGetEdit();
#   define N sizeof(MeasuringTractAsFactor)
    memcpy(MeasuringTractAsFactor, VistCalibrationAsFactor, N);
    memcpy(MeasuringTractAvFactor, VistCalibrationAvFactor, N);
    memcpy(MeasuringTractAaFactor, VistCalibrationAaFactor, N);
#   undef N
    MeasuringTractCalc();
  }

  if(KeyDown == KB_F1) {
    VistCalibrationGetEdit();
    switch(VistCalibrationParam) {
      case mpSpp:  VistCalibrationParam = mpVrms; break;
      case mpVrms: VistCalibrationParam = mpAamp; break;
      case mpAamp: VistCalibrationParam = mpSpp;
      default:;
    }
    MeasureSetParamOn(VistCalibrationParam);
    LcdRepaint();
  }

  if(KeyDown == KB_F2) {
    PrgVerb = VB_MENU_INI;
  }

  if(KeyDown == KB_F3) {
    VistCalibrationGetEdit();
    VistCalibrationVerb = VCV_STEP1_DO_INI;
  }

  if(KeyDown == KB_UP || KeyDown == KB_DOWN) {
    VistCalibrationGetEdit();
    if(++VistCalibrationTab == 2)
      VistCalibrationTab = 0;
    tab_changed = TRUE;
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspaceWithLangCaption(858 + LANG_COUNT*VistCalibrationGetParamIndex());
    LcdSetTextColorDef();
    LcdSetFont(FONT10);

    for(i = 0; i < ARRAY_LENGTH(VistCalibrationLabelIds); i++) {
      LcdDrawRaLangText(VIST_CALIBRATION_EDIT_X1, VistCalibrationLabelYs[i],
       VistCalibrationLabelIds[i] +
       (VistCalibrationLabelIds[i] == 931 ?
        LANG_COUNT*(WORD)VistCalibrationGetParamIndex() : 0U) +
       (VistCalibrationLabelIds[i] == 892 ?
        LANG_COUNT*(WORD)VistCalibrationGetParamIndex() : 0U));
    }

    LcdSpinEditCreate(se);
    VistCalibrationSetEdit();

    LcdDrawLangHint(128, 955, 913);

    LcdDrawEnd();
  }
  else if(tab_changed == TRUE) {
    LcdDrawBegin();
    LcdSpinEditUnmap(se);
    VistCalibrationSetEdit();
    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || tab_changed == TRUE) {
    LcdSetLetterSpacing(se->Edit.LetterSpacing);

    if(VistCalibrationTab != 0) {
      LcdSetEvenRowTextColorDef();
      switch(VistCalibrationParam) {
        case mpSpp:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAsFactor[0]); break;
        case mpVrms:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAvFactor[0]); break;
        case mpAamp:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAaFactor[0]); break;
        default:
          n = 0;
      }
      LcdDrawLaFactor(VIST_CALIBRATION_EDIT_X2 + 2U, VIST_CALIBRATION_EDIT_A0_Y,
       LcdText, n);
    }

    if(VistCalibrationTab != 1) {
      LcdSetTextColorDef();
      switch(VistCalibrationParam) {
        case mpSpp:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAsFactor[1]); break;
        case mpVrms:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAvFactor[1]); break;
        case mpAamp:
          n = MeasuringTractAxToStr(LcdText, VistCalibrationAaFactor[1]); break;
        default:
          n = 0;
      }
      LcdDrawLaFactor(VIST_CALIBRATION_EDIT_X2 + 2U, VIST_CALIBRATION_EDIT_A1_Y,
       LcdText, n);
    }
  }

  LcdSpinEditDo(se);
}

//---------------------------------------------------------
static void VistCalibrationStep1DoIni(void)
{
  VistCalibrationVerb = VCV_STEP1_DO;
  VistCalibrationStep = 1;
  VistCalibrationStepNDoIni();
}

//---------------------------------------------------------
static void VistCalibrationStep1Do(void)
{
  VistCalibrationStepNDo();

  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_EDIT_INI;
  }

  if(KeyDown == KB_F3 && MeasurePeriodAvailable == TRUE) {
    VistCalibrationVerb = VCV_STEP1_EDIT_INI;
    switch(VistCalibrationParam) {
      case mpSpp:
        VistCalibrationStepNInputU[0] = MeasureU2amp;
        break;
      case mpVrms:
        VistCalibrationStepNInputU[0] = MeasureUavr;
        break;
      case mpAamp:
        VistCalibrationStepNInputU[0] = MeasureUamp;
        break;
      default:
        ;
    }
  }
}

//--------------------------------------------------------
static void VistCalibrationStep1EditIni(void)
{
  VistCalibrationVerb = VCV_STEP1_EDIT;
  VistCalibrationStepNEditIni();
}

//---------------------------------------------------------
static void VistCalibrationStep1Edit(void)
{
  VistCalibrationStepNEdit();

  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_STEP1_DO_INI;
  }

  if(KeyDown == KB_F3) {
    VistCalibrationVerb = VCV_STEP2_DO_INI;
  }
}

//---------------------------------------------------------
static void VistCalibrationStep2DoIni(void)
{
  VistCalibrationVerb = VCV_STEP2_DO;
  VistCalibrationStep = 2;
  VistCalibrationStepNDoIni();
}

//---------------------------------------------------------
static void VistCalibrationStep2Do(void)
{
  VistCalibrationStepNDo();

  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_STEP1_EDIT_INI;
  }

  if(KeyDown == KB_F3 && MeasurePeriodAvailable == TRUE) {
    VistCalibrationVerb = VCV_STEP2_EDIT_INI;
    switch(VistCalibrationParam) {
      case mpSpp:
        VistCalibrationStepNInputU[1] = MeasureU2amp;
        break;
      case mpVrms:
        VistCalibrationStepNInputU[1] = MeasureUavr;
        break;
      case mpAamp:
        VistCalibrationStepNInputU[1] = MeasureUamp;
        break;
      default:
        ;
    }
  }
}

//--------------------------------------------------------
static void VistCalibrationStep2EditIni(void)
{
  VistCalibrationVerb = VCV_STEP2_EDIT;
  VistCalibrationStepNEditIni();
}

//---------------------------------------------------------
static void VistCalibrationStep2Edit(void)
{
  VistCalibrationStepNEdit();

  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_STEP2_DO_INI;
  }

  if(KeyDown == KB_F3) {
    if(VistCalibrationHasError() == TRUE)
      VistCalibrationVerb = VCV_STEP2_ERROR_INI;
    else
      VistCalibrationVerb = VCV_DONE_INI;
  }
}

//---------------------------------------------------------
static void VistCalibrationStep2ErrorIni(void)
{
  WORD i;

  VistCalibrationVerb = VCV_STEP2_ERROR;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LCD_RGB_TO_COLOR(255, 0, 0));
  LcdDrawCaLangText(LCD_W/2, LCD_CAPTION_Y, 17);

  LcdSetTextColorDef();
  LcdSetFont(FONT11);
  for(i = 0; i < 2; i++) {
    LcdDrawCaLangText(LCD_W/2, 120U + LcdFontHeight*i, 237U + i*LANG_COUNT);
  }

  LcdDrawLangHint(0, 877, 0);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistCalibrationStep2Error(void)
{
  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_STEP2_EDIT_INI;
  }
}

//---------------------------------------------------------
static void VistCalibrationDoneIni(void)
{
  WORD i;

  VistCalibrationCalc();

  VistCalibrationVerb = VCV_DONE;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);

  for(i = 0; i < 2U; i++)
    LcdDrawCaLangText(LCD_W/2, 100U + i*40U, 130U + LANG_COUNT*i);

  LcdDrawLangHint(0, 877, 875);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistCalibrationDone(void)
{
  if(KeyDown == KB_F2) {
    VistCalibrationVerb = VCV_STEP2_EDIT_INI;
  }

  if(KeyDown == KB_F3) {
    VistCalibrationVerb = VCV_EDIT_INI;
  }
}

//---------------------------------------------------------
#define X1 VIST_CALIBRATION_STEP_X1
#define X2 VIST_CALIBRATION_STEP_X2
#define Y1 VIST_CALIBRATION_STEP_Y1
#define Y2 VIST_CALIBRATION_STEP_Y2
#define Y3 VIST_CALIBRATION_STEP_Y3
#define Y4 VIST_CALIBRATION_STEP_Y4
#define Y5 VIST_CALIBRATION_STEP_Y5

static void VistCalibrationStepNDoIni(void)
{
  WORD id;
  
  LcdDrawBegin();

  LcdDrawWorkspace();
  VistCalibrationDrawStepCaption();

  LcdSetLabelTextColorDef();
  LcdSetFont(LCD_ITEM_FONT);
  LcdDrawRaResText(X1 - 6,  Y1, 617);
  LcdDrawLaLangText(X2 + 6, Y1, 659);
  LcdDrawRaResText(X1 - 6,  Y2, 655);
  LcdDrawLaLangText(X2 + 6, Y2, 657);
  LcdDrawRaLangText(X1 - 6, Y3, 677);
  LcdDrawLaResText(X2 + 6,  Y3, 855);
  LcdDrawRaResText(X1 - 6,  Y4, 857);
  LcdDrawRaLangText(X1 - 6, Y5, 679);
  LcdDrawLaLangText(X2 + 6, Y5, 925);

  LcdDrawLangHint(0, 877, 875);

  LcdSetTextColorDef();
  LcdSetFont(FONT08);
  switch(VistCalibrationParam) {
    case mpSpp: 
      id = 314; 
      break;
    case mpVrms:
      id = 316;
      break;
    case mpAamp:
      id = 312;
      break;
    default:
      id = 0;
  }
  LcdDrawCaLangText(LCD_W/2, 282, id);

  LcdDrawEnd();

  LcdRepaint();
}

//---------------------------------------------------------
static void VistCalibrationStepNDo(void)
{
  switch(VistCalibrationParam) {
    case mpSpp:
      VistCalibrationStepNInputU[VistCalibrationStep - 1] = MeasureU2amp;
      break;
    case mpVrms:
      VistCalibrationStepNInputU[VistCalibrationStep - 1] = MeasureUavr;  
      break;
    case mpAamp:
      VistCalibrationStepNInputU[VistCalibrationStep - 1] = MeasureUamp;
      break;
    default:
      ;
  }

  
  if(LcdFlags.Repaint || LcdFlags.Draw) {
    BYTE n;
    TLcdRect r = {X1, Y1, X2, Y1};
    FLOAT32 u;

    LcdDrawBegin();

    r.Bottom = (r.Top = Y1) + LcdFontHeight;
    n = MeasureTtoStr(LcdText, MeasureT, MeasurePeriodAvailable);
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    n = MeasureFtoStr(LcdText, MeasureF, MeasurePeriodAvailable);
    r.Bottom = (r.Top = Y2) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    n = MeasureNoiseToStr(LcdText, MeasureNoise, MeasurePeriodAvailable);
    r.Bottom = (r.Top = Y3) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    WordToStrWithLeadingSpaces(LcdText, (WORD)MeasureAmplifierGetK(), n = 4);
    n = StripDecimal(LcdText, n = 4);
    r.Bottom = (r.Top = Y4) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    u = VistCalibrationStepNInputU[VistCalibrationStep - 1];
    if(u < 0.0f)
      u = 0.0f;
    if(u > 99.9999f)
      memcpy(LcdText, "**.****", n = 7);
    if(u > 9.99999f)
      n = sprintf((char*)LcdText, "%7.4f", u);
    else
      n = sprintf((char*)LcdText, "%7.5f", u);
    r.Bottom = (r.Top = Y5) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);

    LcdDrawEnd();
  }
}
#undef X1
#undef X2
#undef Y1
#undef Y2
#undef Y3
#undef Y4
#undef Y5

//---------------------------------------------------------
static void VistCalibrationStepNEditIni(void)
{
  WORD y;
  WORD units_id;
  WORD label_id;

  LcdDrawBegin();

  LcdDrawWorkspace();
  VistCalibrationDrawStepCaption();

  LcdDrawLangHint(0, 877, 875);

  LcdSpinEditCreate(&LcdSpinEdit);
  VistCalibrationStepNSetEdit();
  LcdSpinEditSetup(&LcdSpinEdit);
  LcdSpinEditDraw(&LcdSpinEdit);

  LcdSetLabelTextColorDef();
  y = LcdSpinEdit.Y + 15U;
  switch(VistCalibrationParam) {
    case mpSpp:
      units_id = 176U;
      label_id = 665U;
      break;
    case mpVrms:
      units_id = 713U;
      label_id = 666U;
      break;
    case mpAamp:
      units_id = 717U;
      label_id = 667U;
      break;
    default:
      units_id = 0U;
      label_id = 0U;
  }
  LcdDrawLaLangText(LcdSpinEdit.X + LcdSpinEdit.W + 4U, y, units_id);
  LcdDrawRaResText(LcdSpinEdit.X - 4U, y, label_id);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistCalibrationStepNEdit(void)
{
  VistCalibrationStepNGetEdit();
  LcdSpinEditDo(NULL);
}

//---------------------------------------------------------
static void VistCalibrationGetEdit(void)
{
  VistCalibrationPos = LcdSpinEdit.Pos;
}

//---------------------------------------------------------
static void VistCalibrationSetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;
  BYTE txt_len;
  INT32 min;
  INT32 max;
  INT32 val = 1;
  BYTE pos;
  BYTE comma = 0;
  WORD y;
  BYTE* exp = NULL;
  WORD x;
  WORD w;

  x = VIST_CALIBRATION_EDIT_X2 + 2U;
  y = !VistCalibrationTab ?
   VIST_CALIBRATION_EDIT_A0_Y : VIST_CALIBRATION_EDIT_A1_Y;
  w = VIST_CALIBRATION_EDIT_X3 - x + 2U;
  txt_len = MEASURINGTRACT_X_FACTOR_SIZE + 2;
  min = 1;
  max = 9999;
  pos = VistCalibrationPos;
  comma = MEASURINGTRACT_X_FACTOR_COMMA;

  switch(VistCalibrationParam) {
    case mpSpp:  exp = VistCalibrationAsFactor[VistCalibrationTab]; break;
    case mpVrms: exp = VistCalibrationAvFactor[VistCalibrationTab]; break;
    case mpAamp: exp = VistCalibrationAaFactor[VistCalibrationTab]; break;
    default:;
  }

  se->InactiveTextColor = LcdEvenRowTextColorDef;
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
  if(VistCalibrationTab == 0)
    LcdSpinEditUnmap(se);
  LcdSpinEditDraw(se);
}

//---------------------------------------------------------
static void VistCalibrationStepNGetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;

  switch(VistCalibrationParam) {
    case mpSpp:
      VistCalibrationStepNS[VistCalibrationStep - 1] = se->Value;
      VistCalibrationStepNSPos = se->Pos;
      break;
    case mpVrms:
      VistCalibrationStepNV[VistCalibrationStep - 1] = se->Value;
      VistCalibrationStepNVPos = se->Pos;
      break;
    case mpAamp:
      VistCalibrationStepNAPos = se->Pos;
      break;
    default:;
  }
}

//---------------------------------------------------------
static void VistCalibrationStepNSetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;
  BYTE txt_len;
  INT32 min = 1;
  INT32 max;
  INT32 val = 1;
  BYTE pos;
  BYTE comma = 0;
  WORD y;
  BYTE* exp = NULL;
  WORD x;
  WORD w;

  x = VIST_CALIBRATION_EDIT_X2 + 2U;
  y = LCD_H/2;
  w = VIST_CALIBRATION_EDIT_X3 - x + 2U;
  txt_len = MEASURINGTRACT_X_FACTOR_SIZE + 2;
  min = 1;
  max = 9999;
  pos = VistCalibrationPos;
  comma = MEASURINGTRACT_X_FACTOR_COMMA;

  switch(VistCalibrationParam) {
    case mpSpp:
      max = 9999;
      comma = 0;
      txt_len = 4;
      pos = VistCalibrationStepNSPos;
      w = 68;
      x = 88;
      val = VistCalibrationStepNS[VistCalibrationStep - 1];
      break;
    case mpVrms:
      max = 999999;
      comma = 3;
      txt_len = 7;
      pos = VistCalibrationStepNVPos;
      w = 92;
      x = 64;
      val = VistCalibrationStepNV[VistCalibrationStep - 1];
      break;
    case mpAamp:
      max = 9999;
      comma = 3;
      txt_len = 8U + 2U;
      pos = VistCalibrationStepNAPos;
      exp = VistCalibrationStepNA[VistCalibrationStep - 1];
      w = 126;
      x = 54;
    default:;
  }

  se->InactiveTextColor = LcdEvenRowTextColorDef;
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
  LcdSpinEditDraw(se);
}

//---------------------------------------------------------
static void VistCalibrationSave(void)
{
  SaveMeasuringTract();
}

//---------------------------------------------------------
static BOOL VistCalibrationHasError(void)
{
  return TO_BOOL(VistCalibrationStepNInputU[0] == VistCalibrationStepNInputU[1]);
}

//---------------------------------------------------------
static void VistCalibrationCalc(void)
{
  switch(VistCalibrationParam) {
    case mpSpp:  VistCalibrationCalcS(); break;
    case mpVrms: VistCalibrationCalcV(); break;
    case mpAamp: VistCalibrationCalcA(); break;
    default: ;
  }
}

//---------------------------------------------------------
static void VistCalibrationCalcS(void)
{
  FLOAT32 values[] = {
    /*0.001f**/VistCalibrationStepNS[0],
    /*0.001f**/VistCalibrationStepNS[1],
  };
  VistCalibrationCalcImpl(values, VistCalibrationAsFactor);
}

//---------------------------------------------------------
static void VistCalibrationCalcV(void)
{
  FLOAT32 values[] = {
    0.001f*VistCalibrationStepNV[0],
    0.001f*VistCalibrationStepNV[1]
  };
  VistCalibrationCalcImpl(values, VistCalibrationAvFactor);
}

//---------------------------------------------------------
static void VistCalibrationCalcA(void)
{
  FLOAT32 values[2];
  WORD i;
  for(i = 0; i < 2; i++) {
    values[i] = FactorToFloat(VistCalibrationStepNA[i],
     MEASURINGTRACT_X_FACTOR_SIZE, MEASURINGTRACT_X_FACTOR_COMMA);
  }
  VistCalibrationCalcImpl(values, VistCalibrationAaFactor);
}

//---------------------------------------------------------
static void VistCalibrationCalcImpl(FLOAT32* values,
 BYTE result[][MEASURINGTRACT_X_FACTOR_SIZE])
{
  WORD i;

  VistCalibrationFloatFactor[1] = (values[1] - values[0])/
   (VistCalibrationStepNInputU[1] - VistCalibrationStepNInputU[0]);
  VistCalibrationFloatFactor[0] = values[0] -
   VistCalibrationFloatFactor[1]*VistCalibrationStepNInputU[0];

  for(i = 0; i < 2; i++) {
    FactorFromFloat(result[i], MEASURINGTRACT_X_FACTOR_SIZE,
     MEASURINGTRACT_X_FACTOR_COMMA, VistCalibrationFloatFactor[i]);
  }
}

//---------------------------------------------------------
static void VistCalibrationDrawStepCaption(void)
{
  BYTE n = (BYTE)sprintf((char*)LcdText, (const char*)GetLangResource(174),
   (int)VistCalibrationStep);
  LcdDrawCaption(LcdText, n);

  LcdSetFont(FONT08);
  LcdSetTextColorDef();
  LcdDrawCaLangText(LCD_W/2, 78,
   VistCalibrationParamIds[VistCalibrationGetParamIndex()]);
}

//---------------------------------------------------------
static WORD VistCalibrationGetParamIndex(void)
{
  switch(VistCalibrationParam) {
    case mpVrms: return 1;
    case mpAamp: return 2;
    default:     return 0;
  }
}
