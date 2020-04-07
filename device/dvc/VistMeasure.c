/*---------------------------------------------------------
  VistMeasure.c
---------------------------------------------------------*/

#include "VistMeasure.h"
#include "measure.h"
#include "keybrd.h"
#include "mem.h"
#include "VistParams.h"
#include "language.h"

static NOINIT enum {
  VMV_PREPARE_INI,
  VMV_PREPARE,
  VMV_DO_INI,
  VMV_DO_CONTINUE,
  VMV_DO,
  VMV_RESULT_INI,
  VMV_RESULT
} VistMeasureVerb;
static NOINIT BOOL VistMeasureAvailable;

static void VistMeasurePrepareIni(void);
static void VistMeasurePrepare(void);
static void VistMeasureDoIni(void);
static void VistMeasureDoContinue(void);
static void VistMeasureDo(void);
static void VistMeasureResultIni(void);
static void VistMeasureResult(void);

static void VistMeasureDraw(BOOL redraw);
static void VistMeasureDrawResult(void);
static void VistMeasureLoadNumber(void);
static void VistMeasureSave(void);
static BYTE VistMeasureObjectToStr(BYTE* buff, BYTE obj);
static void VistMeasureDrawDone(BOOL done);

//---------------------------------------------------------
void ShowVistMeasureIni(void)
{
  VistMeasureVerb = VMV_PREPARE_INI;
  VistMeasureAvailable = FALSE;
  VistMeasureSetType();
}

//---------------------------------------------------------
void ShowVistMeasure(void)
{
  if(KeyDown == KB_MENU) {
    PrgVerb = VB_MENU_INI;
  }

  if(KeyDown == KB_F3) {
    PrgVerb = VB_VIST_ARCHIVE_INI;
  }

  switch(VistMeasureVerb) {
    case VMV_PREPARE_INI:
      VistMeasurePrepareIni();
    case VMV_PREPARE:
      VistMeasurePrepare(); break;
    case VMV_DO_INI:
      VistMeasureDoIni();
    case VMV_DO_CONTINUE:
      VistMeasureDoContinue();
    case VMV_DO:
      VistMeasureDo(); break;
    case VMV_RESULT_INI:
      VistMeasureResultIni();
    case VMV_RESULT:
      VistMeasureResult();
  }
}

//---------------------------------------------------------
static void VistMeasurePrepareIni(void)
{
  VistMeasureVerb = VMV_PREPARE;
  TempWord = 0;
  MeasureDrawWait();
}

//---------------------------------------------------------
static void VistMeasurePrepare(void)
{
  if(PrgFlags.CentiSec)
    if(++TempWord == 15)
      VistMeasureVerb = VMV_DO_INI;
}

//---------------------------------------------------------
static void VistMeasureDoIni(void)
{
  VistMeasureVerb = VMV_DO_CONTINUE;
  LcdRepaint();
}

//---------------------------------------------------------
static void VistMeasureDoContinue(void)
{
  VistMeasureVerb = VMV_DO;
  VistMeasureLoadNumber();

  if(!LcdFlags.Repaint) {
    LcdDrawBegin();
    VistMeasureDrawDone(FALSE);
    //VistMeasureDrawType(264, VistParamsType);
    LcdDrawEnd();
  }
}

//---------------------------------------------------------
static void VistMeasureDo(void)
{
  if(KeyDown == KB_F1) {
    MeasureReset();
  }
  
  switch(VistParamsType) {
    case vptVrms: 
      VistMeasureAvailable = TO_BOOL(MeasurePeriodAvailable == TRUE &&
                                     MeasureV >= 0.19f);
      break;
    default:
      VistMeasureAvailable = MeasurePeriodAvailable;
  }

  if(KeyDown == KB_MEASURE && VistMeasureAvailable == TRUE) {
    VistMeasureVerb = VMV_RESULT_INI;
    return;
  }

  MeasureTResult = MeasureT;
  MeasureFResult = MeasureF;
  MeasureNoiseResult = MeasureNoise;
  MeasureSResult = MeasureS;
  MeasureVResult = MeasureV;
  MeasureAResult = MeasureA;

  if(LcdFlags.Repaint) {
    VistMeasureDraw(TRUE);
  }
  else if(LcdFlags.Draw) {
    VistMeasureDraw(FALSE);
  }
}

//---------------------------------------------------------
static void VistMeasureResultIni(void)
{
  VistMeasureVerb = VMV_RESULT;
  MeasureSetTime();
  VistMeasureDrawResult();
}

//---------------------------------------------------------
static void VistMeasureResult(void)
{
  switch(KeyDown) {
    case KB_MENU:
    case KB_MEASURE:
    case KB_F3:
      VistMeasureSave();
    case KB_F1:
      VistMeasureVerb = VMV_DO_CONTINUE;
  }
}

//---------------------------------------------------------
static void VistMeasureDraw(BOOL redraw)
{
  BOOL changed;

  LcdDrawBegin();

  if(redraw == TRUE) {
    LcdDrawWorkspace();

    VistMeasureDrawObject(VistParamsObject, TRUE);

    LcdSetFont(FONT08);
    LcdSetTextColor(MeasureNumberColor);
    LcdDrawCaLangText(LCD_W/2, 112, 301);
    MeasureGaugeCreate(130);

    //VistMeasureDrawType(264, VistParamsType);

    LcdDrawLangHint(685, 0, 715);
  }
  else {
    MeasureGaugeSetValue(MeasureSignalLevel, FALSE);
  }

  changed = redraw;
  if(TriggerSetValueWord(&MeasureNumberTrigger, MeasureNumber) == TRUE)
    changed = TRUE;
  if(changed == TRUE)
    MeasureDrawNumber(MeasureNumber, redraw);

  changed = redraw;
  if(TriggerSetValueFloat(&MeasureFTrigger, MeasureFResult) == TRUE)
    changed = TRUE;
  if(TriggerSetValueFloat(&MeasureFTrigger, VistMeasureAvailable) == TRUE)
    changed = TRUE;
  if(changed == TRUE)
    VistMeasureDrawF(62, MeasureFResult, VistMeasureAvailable, redraw);

  changed = redraw;
  switch(VistParamsType) {
    case vptSpp:
      if(TriggerSetValueFloat(&MeasureSTrigger, MeasureSResult) == TRUE)
        changed = TRUE;
      if(TriggerSetValueBool(&MeasureSTrigger, VistMeasureAvailable) == TRUE)
        changed = TRUE;
      if(changed == TRUE) {
        VistMeasureDrawS(180, MeasureSResult, VistMeasureAvailable, redraw);
      }
      break;
    case vptVrms:
      if(TriggerSetValueFloat(&MeasureVTrigger, MeasureVResult) == TRUE)
        changed = TRUE;
      if(TriggerSetValueBool(&MeasureVTrigger, VistMeasureAvailable) == TRUE)
        changed = TRUE;
      if(changed == TRUE)
        VistMeasureDrawV(180, MeasureVResult, VistMeasureAvailable, redraw);
      break;
    case vptAamp:
      if(TriggerSetValueFloat(&MeasureATrigger, MeasureAResult) == TRUE)
        changed = TRUE;
      if(TriggerSetValueBool(&MeasureATrigger, VistMeasureAvailable) == TRUE)
        changed = TRUE;
      if(changed == TRUE)
        VistMeasureDrawA(180, MeasureAResult, VistMeasureAvailable, redraw);
      break;
    default:
      ;
  }

  changed = redraw;
  if(TriggerSetValueFloat(&MeasureNoiseTrigger, MeasureNoiseResult) == TRUE)
    changed = TRUE;
  if(TriggerSetValueBool(&MeasureNoiseTrigger, VistMeasureAvailable) == TRUE)
    changed = TRUE;
  if(changed == TRUE)
    VistMeasureDrawNoise(222, MeasureNoiseResult, VistMeasureAvailable, redraw);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistMeasureDrawResult(void)
{
  VistMeasureDraw(FALSE);
  LcdDrawBegin();
  VistMeasureDrawDone(TRUE);
  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistMeasureLoadNumber(void)
{
  MeasureLoadNumber();
}

//---------------------------------------------------------
static void VistMeasureSave(void)
{
  MemAddCell();
}

//---------------------------------------------------------
void VistMeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw)
{
  MeasureDrawF(y, freq, f, redraw);
}

//---------------------------------------------------------
void VistMeasureDrawS(WORD y, FLOAT32 s, BOOL f, BOOL redraw)
{
  MeasureDrawS(y, s, f, redraw);
}

//---------------------------------------------------------
void VistMeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw)
{
  MeasureDrawV(y, v, f, redraw);
}

//---------------------------------------------------------
void VistMeasureDrawA(WORD y, FLOAT32 a, BOOL f, BOOL redraw)
{
  MeasureDrawA(y, a, f, redraw);
}

//---------------------------------------------------------
void VistMeasureDrawNoise(WORD y, FLOAT32 noise, BOOL f, BOOL redraw)
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
static BYTE VistMeasureObjectToStr(BYTE* buff, BYTE obj)
{
  if(obj < VIST_PARAMS_OBJECTS_COUNT)
    return LoadLangResource(buff, 528U + LANG_COUNT*obj);
  return 0;
}

//---------------------------------------------------------
void VistMeasureDrawObject(BYTE o, BOOL redraw)
{
  BYTE n = VistMeasureObjectToStr(LcdText, o);
  MeasureObjectRect.Left = 1;
  MeasureObjectRect.Top = LCD_STATUS_H + 2;
  LcdSetTextColor(MeasureNumberColor);
  LcdSetFont(FONT10);
  LcdDrawLaAutoSizedRectText(&MeasureObjectRect, LcdText, n, redraw);
}

//---------------------------------------------------------
static void VistMeasureDrawDone(BOOL done)
{
  MeasureDrawDone(265, done);
}

//---------------------------------------------------------
void VistMeasureSetType(void)
{
  switch(VistParamsType) {
    case vptSpp:  MeasureSetParamOn(mpSpp);  break;
    case vptVrms: MeasureSetParamOn(mpVrms); break;
    case vptAamp: MeasureSetParamOn(mpAamp);
    default:;
  }
}

//---------------------------------------------------------
void VistMeasureDrawType(WORD y, TVistParamsType param)
{
  WORD id = 0;
  WORD x;
  WORD w;
  WORD h;

  switch(param) {
    case vptSpp:
    case vptAamp:
      id = 551;
      break;
    case vptVrms:
      id = 553;
    default:;
  }

  LcdSetFont(FONT10);
  w = LcdGetTextWidth((BYTE*)"0", 1)*3 + 8;
  x = LCD_W/2 - w/2;
  h = LcdFontHeight + 4;
  //LcdSetBgColorDef();

  LcdSetFgColor(LCD_RGB_TO_COLOR(255, 0, 0));//LcdGetBgColor());
  LcdDrawRect(x + 1, y + 1, w - 2, h - 2);
 // LcdSetFgColor(LCD_RGB_TO_COLOR(255,0,0));
  LcdDrawFrame(x, y, w, h);
  LcdSetTextColor(LCD_RGB_TO_COLOR(255,255,255));
  LcdSetBgColor(LCD_RGB_TO_COLOR(255,0,0));
  LcdDrawCaLangText(LCD_W/2, y + 2, id);
}
