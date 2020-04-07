/*---------------------------------------------------------
  IncTest.c
---------------------------------------------------------*/

#include "IncTest.h"
#include "IncMeasure.h"
#include "VistTest.h"
#include "IncMeasureMode.h"
#include "measure.h"
#include "test.h"
#include "lcd.h"
#include "keybrd.h"

#define INC_TEST_X1     68
#define INC_TEST_X2     176
#define INC_TEST_Y1     120
#define INC_TEST_Y2     (INC_TEST_Y1 + 54)
#define INC_TEST_Y3     (INC_TEST_Y1 + 2*54)

static NOINIT enum {
  ITV_PREPARING_INI,
  ITV_PREPARING,
  ITV_MEASURE_INI,
  ITV_MEASURE
} IncTestVerb;

static void IncTestPreparingIni(void);
static void IncTestPreparing(void);
static void IncTestMeasureIni(void);
static void IncTestMeasure(void);

//---------------------------------------------------------
void ShowIncTestIni(void)
{
  if(IncMeasureMode == incMmVist) {
    ShowVistTestIni();
  }
  else {
    PrgVerb = VB_INC_TEST;
    IncTestVerb = ITV_PREPARING_INI;
    MeasureOnEnter();
  }
}

//---------------------------------------------------------
void ShowIncTest(void)
{
  if(IncMeasureMode == incMmVist) {
    ShowVistTest();
  }
  else {
    ProcessStandardKeyDownActions();

    MeasureMeasure();

    switch(IncTestVerb) {
      case ITV_PREPARING_INI:
        IncTestPreparingIni();
      case ITV_PREPARING:
        IncTestPreparing(); break;
      case ITV_MEASURE_INI:
        IncTestMeasureIni();
      case ITV_MEASURE:
        IncTestMeasure();
    }

    if(PrgVerb != VB_INC_TEST)
      MeasureOnExit();
  }
}

//---------------------------------------------------------
static void IncTestPreparingIni(void)
{
  IncTestVerb = ITV_PREPARING;
  TestPreparingIni();
}

//---------------------------------------------------------
static void IncTestPreparing(void)
{
  if(TestPreparing() == TRUE)
    IncTestVerb = ITV_MEASURE_INI;
}

//---------------------------------------------------------
static void IncTestMeasureIni(void)
{
  IncTestVerb = ITV_MEASURE;

  TestMeasuringIni();

  LcdDrawBegin();

  LcdSetLabelTextColorDef();
  LcdDrawRaResText(INC_TEST_X1 - 6, INC_TEST_Y1, 655);
  LcdDrawLaLangText(INC_TEST_X2 + 6, INC_TEST_Y1, 657);
  LcdDrawRaResText(INC_TEST_X1 - 6, INC_TEST_Y2, 656);
  LcdDrawLaLangText(INC_TEST_X2 + 6, INC_TEST_Y2, 659);
  LcdDrawRaLangText(INC_TEST_X1 - 6, INC_TEST_Y3, 677);
  LcdDrawLaResText(INC_TEST_X2 + 6, INC_TEST_Y3, 855);

  LcdDrawEnd();
}

//---------------------------------------------------------
static void IncTestMeasure(void)
{
  TestMeasuring();

  if(LcdFlags.Draw || LcdFlags.Repaint) {
    BYTE n;
    TLcdRect r = {INC_TEST_X1, INC_TEST_Y1, INC_TEST_X2, INC_TEST_Y1};

    LcdDrawBegin();

    r.Bottom += LcdFontHeight;

    n = MeasureFtoStr(LcdText, MeasureF, TempBool);
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    n = MeasureTtoStr(LcdText, MeasureT, TempBool);
    r.Bottom = (r.Top = INC_TEST_Y2) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);
    n = MeasureNoiseToStr(LcdText, MeasureNoise, TempBool);
    r.Bottom = (r.Top = INC_TEST_Y3) + LcdFontHeight;
    LcdDrawRaRectText(&r, 0, 0, LcdText, n);

    LcdDrawEnd();
  }
}
