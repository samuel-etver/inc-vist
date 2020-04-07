/*---------------------------------------------------------
  VistSensor.c
---------------------------------------------------------*/

#include "VistSensor.h"
#include "VistParams.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"
#include "factor.h"
#include <stdlib.h>

#define VISTSENSOR_X                92U
#define VISTSENSOR_K_MAX            99999U

static const WORD VistSensorY[] = {
  100U, 220U
};

static NOINIT DWORD VistSensorK[VIST_PARAMS_OBJECTS_COUNT];
static NOINIT FLOAT32 VistSensorKFloat[VIST_PARAMS_OBJECTS_COUNT];
static NOINIT BYTE VistSensorTab;
static NOINIT BYTE VistSensorPos;

static void VistSensorCalc(void);

//---------------------------------------------------------
void InitVistSensor(void)
{
  VistSensorPos = 0;
  VistSensorTab = 0;
  LoadFirstProducedVistSensor();
}

//---------------------------------------------------------
void ShowVistSensorIni(void)
{
  WORD i;

  PrgVerb = VB_VIST_SENSOR;

  LcdDrawBegin();

  LcdDrawWorkspace();
  LcdDrawLangCaptionY(VistSensorY[0] - 48, 528);
  LcdDrawLangCaptionY(VistSensorY[1] - 48, 530);
  LcdSetFont(LCD_ITEM_FONT);
  LcdSetLabelTextColorDef();
  for(i = 0; i < 2U; i++) {
    LcdDrawRaResText(VISTSENSOR_X - 8U, VistSensorY[i], 857);
  }

  LcdDrawEnd();

  LcdRepaint();
}

//---------------------------------------------------------
void ShowVistSensor(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;

  if(ProcessStandardKeyDownActions() == TRUE) {
    VistSensorPos = LcdSpinEdit.Pos;
    VistSensorK[VistSensorTab] = LcdSpinEdit.Value;
    SaveVistSensor();
  }

  if(KeyDown == KB_UP || KeyDown == KB_DOWN) {
    VistSensorPos = LcdSpinEdit.Pos;
    VistSensorK[VistSensorTab] = LcdSpinEdit.Value;
    VistSensorTab ^= 1U;
    LcdRepaint();
  }

  if(LcdFlags.Repaint) {
    WORD i;
    TLcdRect r;

    LcdDrawBegin();

    LcdSpinEditCreate(se);
    se->X = VISTSENSOR_X;
    se->Y = VistSensorY[VistSensorTab] - 15U;
    se->W = 112;
    se->Comma = 2;
    se->TextLen = 6;
    se->Wrap = TRUE;
    se->EnableLR = TRUE;
    se->Pos = VistSensorPos;
    se->Value = VistSensorK[VistSensorTab];
    se->Max = VISTSENSOR_K_MAX;
    LcdSpinEditSetup(se);
    LcdSpinEditDraw(se);

    LcdSetTextColorDef();
    LcdSetFont(LCD_ITEM_FONT);
    LcdSetFgColor(LcdGetBgColor());
    for(i = 0; i < VIST_PARAMS_OBJECTS_COUNT; i++) {
      if(VistSensorTab != i) {
        DwordToStrAsFloatWithLeadingSpaces(LcdText, VistSensorK[i], 6, 2);
        r.Left = se->X;
        r.Top = VistSensorY[i] - 15U;
        r.Right = r.Left + se->W;
        r.Bottom = r.Top + se->H;
        LcdDrawRaRectText(&r, 0, 15U, LcdText, 6);
      }
    }

    LcdDrawEnd();
  }
  else
    LcdSpinEditDo(se);
}

//---------------------------------------------------------
void LoadVistSensor(void)
{
  /*WORD i;
  DWORD dw;
  for(i = 0; i < VIST_PARAMS_OBJECTS_COUNT; i++) {
    dw = CfgReadDword(CFG_VIST_SENSORFACTOR0 + i*sizeof(DWORD));
    if(dw <= VISTSENSOR_K_MAX)
      VistSensorK[i] = dw;
  }*/
  VistSensorCalc();
}

//---------------------------------------------------------
void SaveVistSensor(void)
{
  WORD i;
  for(i = 0; i < VIST_PARAMS_OBJECTS_COUNT; i++) {
    CfgWriteDword(CFG_VIST_SENSORFACTOR0 + i*sizeof(DWORD), VistSensorK[i]);
  }
  VistSensorCalc();
}

//---------------------------------------------------------
void LoadFirstProducedVistSensor(void)
{
  WORD i;
  for(i = 0; i < VIST_PARAMS_OBJECTS_COUNT; i++) {
    VistSensorK[i] = 100;
  }
}

//---------------------------------------------------------
FLOAT32 VistSensorGetK(WORD i)
{
  return VistSensorKFloat[i];
}

//---------------------------------------------------------
static void VistSensorCalc(void)
{
  WORD i;
  FLOAT32 k;
  for(i = 0; i < VIST_PARAMS_OBJECTS_COUNT; i++) {
    k = VistSensorK[i]*0.01f;
    VistSensorKFloat[i] = k == 0.0f ? 0.0f : (1/k);
  }
}

