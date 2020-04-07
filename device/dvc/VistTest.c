/*---------------------------------------------------------
  VistTest.c
---------------------------------------------------------*/

#include "VistTest.h"
#include "VistMeasure.h"
#include "VistParams.h"
#include "measure.h"
#include "test.h"
#include "lcd.h"
#include "keybrd.h"

#define VIST_TEST_X1     62
#define VIST_TEST_X2     160
#define VIST_TEST_Y1     120
#define VIST_TEST_Y2     (VIST_TEST_Y1 + 54)
#define VIST_TEST_Y3     (VIST_TEST_Y1 + 2*54)

static NOINIT enum {
  VTV_PREPARING_INI,
  VTV_PREPARING,
  VTV_MEASURE_INI,
  VTV_MEASURE
} VistTestVerb;

static void VistTestPreparingIni(void);
static void VistTestPreparing(void);
static void VistTestMeasureIni(void);
static void VistTestMeasure(void);

//---------------------------------------------------------
void ShowVistTestIni(void)
{
  PrgVerb = VB_VIST_TEST;
  VistTestVerb = VTV_PREPARING_INI;
  MeasureOnEnter();
  VistMeasureSetType();
}

//---------------------------------------------------------
void ShowVistTest(void)
{
  ProcessStandardKeyDownActions();

  MeasureMeasure();

  switch(VistTestVerb) {
    case VTV_PREPARING_INI:
      VistTestPreparingIni();
    case VTV_PREPARING:
      VistTestPreparing(); break;
    case VTV_MEASURE_INI:
      VistTestMeasureIni();
    case VTV_MEASURE:
      VistTestMeasure();
  }

  if(PrgVerb != VB_VIST_TEST)
    MeasureOnExit();
}

//---------------------------------------------------------
static void VistTestPreparingIni(void)
{
  VistTestVerb = VTV_PREPARING;
  TestPreparingIni();
}

//---------------------------------------------------------
static void VistTestPreparing(void)
{
  if(TestPreparing() == TRUE)
    VistTestVerb = VTV_MEASURE_INI;
}

//---------------------------------------------------------
static void VistTestMeasureIni(void)
{
  VistTestVerb = VTV_MEASURE;

  TestMeasuringIni();

  LcdDrawBegin();

  LcdSetLabelTextColorDef();
  LcdDrawLaLangText(VIST_TEST_X2 + 6, VIST_TEST_Y1, 657);
  LcdDrawRaResText(VIST_TEST_X1 - 6, VIST_TEST_Y1, 655);
  switch(VistParamsType) {
    case vptSpp:
      LcdDrawRaResText(VIST_TEST_X1 - 6, VIST_TEST_Y2, 665);
      LcdDrawLaLangText(VIST_TEST_X2 + 6, VIST_TEST_Y2, 176);
      break;
    case vptVrms:
      LcdDrawRaResText(VIST_TEST_X1 - 6, VIST_TEST_Y2, 666);
      LcdDrawLaLangText(VIST_TEST_X2 + 6, VIST_TEST_Y2, 713);
      break;
    case vptAamp:
      LcdDrawRaResText(VIST_TEST_X1 - 6, VIST_TEST_Y2, 667);
      LcdDrawLaLangText(VIST_TEST_X2 + 6, VIST_TEST_Y2, 717);
    default:;
  }
  LcdDrawRaLangText(VIST_TEST_X1 - 6, VIST_TEST_Y3, 677);
  LcdDrawLaResText(VIST_TEST_X2 + 6, VIST_TEST_Y3, 855);

  //VistMeasureDrawType(264, VistParamsType);

  LcdDrawLangHint(128, 0, 0);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void VistTestMeasure(void)
{
  TestMeasuring();

  TempBool = MeasurePeriodAvailable;

  if(KeyDown == KB_F1) {
    if(++VistParamsType == VIST_PARAMS_TYPES_COUNT)
      VistParamsType = (TVistParamsType)0;
    VistMeasureSetType();
    VistTestVerb = VTV_MEASURE_INI;
    return;
  }

  if(LcdFlags.Draw || LcdFlags.Repaint) {
    BYTE n;
    TLcdRect r = {VIST_TEST_X1, VIST_TEST_Y1, VIST_TEST_X2, VIST_TEST_Y1};

    LcdDrawBegin();

    r.Bottom += LcdFontHeight;

    n = MeasureFtoStr(LcdText, MeasureF, TempBool);
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    switch(VistParamsType) {
      case vptSpp:
        n = MeasureStoStr(LcdText, MeasureS, TempBool);
        break;
      case vptVrms:
        n = MeasureVtoStr2(LcdText, MeasureV, TempBool);
        break;
      case vptAamp:
        n = MeasureAtoStr(LcdText, MeasureA, TempBool);
        break;
      default:
        n = 0;
    }
    r.Bottom = (r.Top = VIST_TEST_Y2) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    n = MeasureNoiseToStr(LcdText, MeasureNoise, TempBool);
    r.Bottom = (r.Top = VIST_TEST_Y3) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);

    LcdDrawEnd();
  }
}
