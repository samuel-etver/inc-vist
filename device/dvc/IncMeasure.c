/*---------------------------------------------------------
  IncMeasure.c
---------------------------------------------------------*/

#include "IncMeasure.h"
#include "IncParams.h"
#include "IncMeasureMode.h"
#include "VistMeasure.h"
#include "DeviceType.h"
#include "measure.h"
#include "keybrd.h"
#include "language.h"
#include "mem.h"
#include "utils.h"

static NOINIT enum {
  IMV_PREPARE_INI,
  IMV_PREPARE,
  IMV_WAIT_INI,
  IMV_WAIT_CONTINUE,
  IMV_WAIT,
  IMV_DO_INI,
  IMV_DO,
  IMV_RESULT_INI,
  IMV_RESULT,
  IMV_NOISE_INI,
  IMV_NOISE
} IncMeasureVerb;

NOINIT FLOAT32 IncMeasureT;
NOINIT FLOAT32 IncMeasureF;
NOINIT FLOAT32 IncMeasureSigma;
NOINIT FLOAT32 IncMeasureDeltaL;
NOINIT FLOAT32 IncMeasureEpsilon;
NOINIT TTrigger IncMeasureSigmaTrigger;
NOINIT TTrigger IncMeasureDeltaLTrigger;
NOINIT TTrigger IncMeasureEpsilonTrigger;
NOINIT TLcdRect IncMeasureLengthRect;
NOINIT TLcdRect IncMeasureDiameterRect;
NOINIT TLcdRect IncMeasureSigmaRect;
NOINIT TLcdRect IncMeasureDeltaLRect;
NOINIT TLcdRect IncMeasureEpsilonRect;
static WORD IncMeasureTicks;

static void IncMeasurePrepareIni(void);
static void IncMeasurePrepare(void);
static void IncMeasureWaitIni(void);
static void IncMeasureWaitContinue(void);
static void IncMeasureWait(void);
static void IncMeasureDoIni(void);
static void IncMeasureDo(void);
static void IncMeasureResultIni(void);
static void IncMeasureResult(void);
static void IncMeasureNoiseIni(void);
static void IncMeasureNoise(void);

static void IncMeasureDraw(BOOL redraw);
static void IncMeasureDrawResult(void);
static void IncMeasureLoadNumber(void);
static void IncMeasureSave(void);

static BYTE IncMeasureLengthToStr(BYTE* buff, WORD length);
static BYTE IncMeasureDiameterToStr(BYTE* buff, BYTE diameter);
static BYTE IncMeasureSigmaToStr(BYTE* buff, FLOAT32 sigma, BOOL f);
static BYTE IncMeasureEpsilonToStr(BYTE* buff, FLOAT32 epsilon, BOOL f);
static BYTE IncMeasureDeltaLtoStr(BYTE* buff, FLOAT32 delta, BOOL f);

//---------------------------------------------------------
void ShowIncMeasureIni(void)
{
  if(DeviceType == dtIncVist && IncMeasureMode == incMmVist) {
    ShowVistMeasureIni();
  }
  else {
    IncMeasureVerb = IMV_PREPARE_INI;
    IncMeasureT = 0.0f;
    IncMeasureF = 0.0f;
    IncMeasureSigma = 0.0f;
    IncMeasureDeltaL = 0.0f;
    IncMeasureEpsilon = 0.0f;
    MeasureFilterFOff();
  }
}

//---------------------------------------------------------
void ShowIncMeasure(void)
{
  if(DeviceType == dtIncVist && IncMeasureMode == incMmVist) {
    ShowVistMeasure();
  }
  else {
    if(KeyDown == KB_MENU) {
      PrgVerb = VB_MENU_INI;
    }

    if(KeyDown == KB_F3) {
      PrgVerb = VB_INC_ARCHIVE_INI;
    }

    switch(IncMeasureVerb) {
      case IMV_PREPARE_INI:
        IncMeasurePrepareIni();
      case IMV_PREPARE:
        IncMeasurePrepare(); break;
      case IMV_WAIT_INI:
        IncMeasureWaitIni();
      case IMV_WAIT_CONTINUE:
        IncMeasureWaitContinue();
      case IMV_WAIT:
        IncMeasureWait(); break;
      case IMV_DO_INI:
        IncMeasureDoIni();
      case IMV_DO:
        IncMeasureDo(); break;
      case IMV_RESULT_INI:
        IncMeasureResultIni();
      case IMV_RESULT:
        IncMeasureResult(); break;
      case IMV_NOISE_INI:
        IncMeasureNoiseIni();
      case IMV_NOISE:
        IncMeasureNoise();
    }
  }
}

//---------------------------------------------------------
static void IncMeasurePrepareIni(void)
{
  IncMeasureVerb = IMV_PREPARE;
  TempWord = 0;
  MeasureDrawWait();
}

//---------------------------------------------------------
static void IncMeasurePrepare(void)
{
  if(PrgFlags.CentiSec)
    if(++TempWord == 15)
      IncMeasureVerb = IMV_WAIT_INI;
}

//---------------------------------------------------------
static void IncMeasureWaitIni(void)
{
  IncMeasureVerb = IMV_WAIT_CONTINUE;
  LcdRepaint();
}

//---------------------------------------------------------
static void IncMeasureWaitContinue(void)
{
  IncMeasureVerb = IMV_WAIT;
}

//---------------------------------------------------------
static void IncMeasureWait(void)
{
  if(KeyDown == KB_MEASURE)
    IncMeasureVerb = IMV_DO_INI;

  if(KeyDown == KB_F1) {
    MeasureFlags.Result = 0;
    MeasureReset();
    IncMeasureVerb = IMV_WAIT_CONTINUE;
  }

  if(LcdFlags.Repaint)
    IncMeasureDraw(TRUE);
  else if(LcdFlags.Draw)
    IncMeasureDraw(FALSE);
}

//---------------------------------------------------------
static void IncMeasureDoIni(void)
{
  IncMeasureVerb = IMV_DO;
  IncMeasureLoadNumber();
  IncMeasureTicks = 0;
//  MeasureReset();
}

//---------------------------------------------------------
static void IncMeasureDo(void)
{
  FLOAT32 tmp;
  if(PrgFlags.CentiSec) {
    if(MeasureF > 0.0f && MeasureNoise < 50.0f) {
      IncMeasureVerb = IMV_RESULT_INI;
      IncMeasureT = (MeasureTResult = MeasureT);
      IncMeasureF = (MeasureFResult = MeasureF);
      MeasureNoiseResult = MeasureNoise;
      IncMeasureSigma = 0.0f;
      if(IncParamsLength > 0.0f) {
        tmp = IncParamsLength*0.001f*IncMeasureF
         - 12.5f*IncParamsDiameter/IncParamsLength;
        if(tmp > 0.0f) {
          IncMeasureSigma = tmp*tmp*3.2f;
        }
      }
      IncMeasureEpsilon = 0.0f;
      if(IncParamsTension > 0.0f) {
        IncMeasureEpsilon = (IncMeasureSigma/IncParamsTension - 1.0f)*100.0f;
      }
      IncMeasureDeltaL = 0.0f;
      if(IncParamsTension > 0.0f) {
        IncMeasureDeltaL = (IncParamsTension - IncMeasureSigma)/
         (3.2f*IncMeasureF*IncMeasureF);
      }
    }
    else if(++IncMeasureTicks == 30U) {
      IncMeasureVerb = IMV_NOISE_INI;
      MeasureFlags.Result = 0;
    }
  }

  if(KeyDown == KB_F1) {
    MeasureReset();
  }

  if(LcdFlags.Draw) {
    IncMeasureDraw(FALSE);
  }
}

//---------------------------------------------------------
static void IncMeasureResultIni(void)
{
  IncMeasureVerb = IMV_RESULT;
  MeasureSetTime();
  MeasureFlags.Result = 1;
  IncMeasureDrawResult();
}

//---------------------------------------------------------
static void IncMeasureResult(void)
{
  switch(KeyDown) {
    case KB_MENU:
    case KB_MEASURE:
    case KB_F3:
      MeasureTResult = IncMeasureT;
      MeasureFResult = IncMeasureF;
      IncMeasureSave();
      IncMeasureVerb = IMV_DO_INI;
      break;
    case KB_F1:
      MeasureFlags.Result = 0;
      IncMeasureVerb = IMV_WAIT_CONTINUE;
  }
}

//---------------------------------------------------------
static void IncMeasureNoiseIni(void)
{
  WORD i;

  IncMeasureVerb = IMV_NOISE;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(FONT20);
  LcdSetTextColor(MeasureErrorMessageColor);
  LcdDrawCaLangText(LCD_W/2, 60, 148);

  LcdSetTextColorDef();
  LcdSetFont(FONT12);
  for(i = 0; i < 2U; i++) {
    LcdDrawCaLangText(LCD_W/2, 140U + i*LcdFontHeight, 150U +i*LANG_COUNT);
  }

  LcdDrawLangHint(685, 0, 0);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void IncMeasureNoise(void)
{
  switch(KeyDown) {
    case KB_MEASURE:
      IncMeasureVerb = IMV_DO_INI;
      IncMeasureDraw(TRUE);
      break;
    case KB_F1:
      IncMeasureVerb = IMV_WAIT_INI;
  }
}

//---------------------------------------------------------
static void IncMeasureDraw(BOOL redraw)
{
  BOOL changed;
  BOOL result = TO_BOOL(MeasureFlags.Result);

  LcdDrawBegin();

  if(redraw == TRUE) {
    LcdDrawWorkspace();

    IncMeasureDrawLength(IncParamsLength, redraw);
    IncMeasureDrawDiameter(IncParamsDiameter, redraw);

    LcdSetFont(FONT08);
    LcdSetTextColor(MeasureNumberColor);
    LcdDrawCaLangText(LCD_W/2, 120, 301);
    MeasureGaugeCreate(136);

    IncMeasureDrawUnits(207);

    LcdDrawLangHint(685, 0, 715);
  }
  else {
    MeasureGaugeSetValue(MeasureSignalLevel, FALSE);
  }

  changed = redraw;
  if(TriggerSetValueWord(&MeasureNumberTrigger, MeasureNumber) == TRUE) changed = TRUE;
  if(changed == TRUE)
    MeasureDrawNumber(MeasureNumber, redraw);

  changed = redraw;
  if(TriggerSetValueFloat(&MeasureFTrigger, IncMeasureF) == TRUE) changed = TRUE;
  if(TriggerSetValueBool(&MeasureFTrigger, result) == TRUE) changed = TRUE;
  if(changed == TRUE)
    IncMeasureDrawF(74, IncMeasureF, result, redraw);

  changed = redraw;
  if(TriggerSetValueFloat(&IncMeasureSigmaTrigger, IncMeasureSigma) == TRUE) changed = TRUE;
  if(TriggerSetValueBool(&IncMeasureSigmaTrigger, result) == TRUE) changed = TRUE;
  if(changed == TRUE)
    IncMeasureDrawSigma(168, IncMeasureSigma, result, redraw);

  changed = redraw;
  if(TriggerSetValueFloat(&IncMeasureDeltaLTrigger, IncMeasureDeltaL) == TRUE) changed = TRUE;
  if(TriggerSetValueBool(&IncMeasureDeltaLTrigger, result) == TRUE) changed = TRUE;
  if(changed == TRUE)
    IncMeasureDrawDeltaL(236, IncMeasureDeltaL, result, redraw);

  changed = redraw;
  if(TriggerSetValueFloat(&IncMeasureEpsilonTrigger, IncMeasureEpsilon) == TRUE) changed = TRUE;
  if(TriggerSetValueBool(&IncMeasureEpsilonTrigger, result) == TRUE) changed = TRUE;
  if(changed == TRUE)
    IncMeasureDrawEpsilon(267, IncMeasureEpsilon, result, redraw);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void IncMeasureDrawResult(void)
{
  IncMeasureDraw(FALSE);
}

//---------------------------------------------------------
static void IncMeasureLoadNumber(void)
{
  MeasureLoadNumber();
}

//---------------------------------------------------------
static void IncMeasureSave(void)
{
  MemAddCell();
}

//---------------------------------------------------------
void IncMeasureReset(void)
{
  IncMeasureSigma = 0.0f;
}

//---------------------------------------------------------
void IncMeasureDrawLength(WORD length, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 223);
  n += IncMeasureLengthToStr(LcdText + n, length);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 266);

  IncMeasureLengthRect.Top = LCD_STATUS_H + 2;
  IncMeasureLengthRect.Left = 1;
  LcdSetTextColor(MeasureNumberColor);
  LcdSetFont(FONT10);
  LcdDrawLaAutoSizedRectText(&IncMeasureLengthRect, LcdText, n, redraw);
}

//---------------------------------------------------------
static BYTE IncMeasureLengthToStr(BYTE* buff, WORD length)
{
  return StripDecimal(buff, IncParamsLengthToStr(buff, length));
}

//---------------------------------------------------------
void IncMeasureDrawDiameter(BYTE diameter, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 39);
  n += IncMeasureDiameterToStr(LcdText + n, diameter);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 711);

  IncMeasureDiameterRect.Top = LCD_STATUS_H + 2 + 20;
  IncMeasureDiameterRect.Left = 1;
  LcdSetTextColor(MeasureNumberColor);
  LcdSetFont(FONT10);
  LcdDrawLaAutoSizedRectText(&IncMeasureDiameterRect, LcdText, n, redraw);
}

//---------------------------------------------------------
static BYTE IncMeasureDiameterToStr(BYTE* buff, BYTE diameter)
{
  return StripDecimal(buff, IncParamsDiameterToStr(buff, diameter));
}

//---------------------------------------------------------
void IncMeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw)
{
  MeasureDrawF(y, freq, f, redraw);
}

//---------------------------------------------------------
void IncMeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw)
{
  MeasureDrawV(y, v, f, redraw);
}

//---------------------------------------------------------
static BYTE IncMeasureSigmaToStr(BYTE* buff, FLOAT32 sigma, BOOL f)
{
  BYTE n;

  if(f == FALSE) {
    buff[0] = '?';
    return 1;
  }

  if(IncParamsUnits == 1) {
    sigma *= (1.0f/0.0980665f);
  }
  if(sigma < 0.0f)
    sigma = 0.0f;
  if(sigma >= 100000.0f)
    memset(buff, '*', n = 5);
  else {
    if(sigma > 10000.0f)
      n = 0;
    else if(sigma >= 1000.0f)
      n = 1;
    else if(sigma > 100.0f)
      n = 2;
    else
      n = 3;
    n = FloatToStr(buff, sigma, n);
  }

  return n;
}

//---------------------------------------------------------
void IncMeasureDrawSigma(WORD y, FLOAT32 sigma, BOOL f, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 134);
  n += IncMeasureSigmaToStr(LcdText + n, sigma, f);

  IncMeasureSigmaRect.Top = y;
  LcdSetTextColor(MeasureSigmaColor);
  LcdSetFont(FONT20);
  LcdDrawCaAutoSizedRectText(&IncMeasureSigmaRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void IncMeasureDrawNoise(WORD y, FLOAT32 noise, BOOL f, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 675);
  n += MeasureNoiseToStr(LcdText + n, noise, f);
  memcpy(LcdText + n, " %", 2);
  n += 2;

  MeasureNoiseRect.Top = y;
  LcdSetTextColor(MeasureNoiseColor);
  LcdSetFont(FONT20);
  LcdDrawCaAutoSizedRectText(&MeasureNoiseRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void IncMeasureDrawUnits(WORD y)
{
  MeasureDrawUnits(y, LcdText,
   LoadLangResource(LcdText, 21 + LANG_COUNT*IncParamsUnits));
}

//---------------------------------------------------------
static BYTE IncMeasureEpsilonToStr(BYTE* buff, FLOAT32 epsilon, BOOL f)
{
  if(f == FALSE) {
    buff[0] = '?';
    return 1;
  }

  if(epsilon < -100.0f)
    epsilon = -100.0f;
  else if(epsilon > 100.0f)
    epsilon = 100.0f;

  return FloatToStr(buff, epsilon, 0);
}

//---------------------------------------------------------
static BYTE IncMeasureDeltaLtoStr(BYTE* buff, FLOAT32 delta, BOOL f)
{
  if(f == FALSE) {
    buff[0] = '?';
    return 1;
  }

  if(delta < 0.0f)
    delta = 0.0f;

  if(delta < 999.999f) {
    return FloatToStr(buff, delta, 3);
  }
  memcpy(buff, "***.***", 7);
  return 7;
}

//---------------------------------------------------------
void IncMeasureDrawDeltaL(WORD y, FLOAT32 delta, BOOL f, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 691);
  n += IncMeasureDeltaLtoStr(LcdText + n, delta, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 332);

  IncMeasureDeltaLRect.Top = y;
  LcdSetTextColor(MeasureSigmaColor);
  LcdSetFont(FONT12);
  LcdDrawCaAutoSizedRectText(&IncMeasureDeltaLRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void IncMeasureDrawEpsilon(WORD y, FLOAT32 epsilon, BOOL f, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 693);
  n += IncMeasureEpsilonToStr(LcdText + n, epsilon, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 855);

  IncMeasureEpsilonRect.Top = y;
  LcdSetTextColor(MeasureNoiseColor);
  LcdSetFont(FONT12);
  LcdDrawCaAutoSizedRectText(&IncMeasureEpsilonRect, LCD_W/2, LcdText, n, redraw);
}

