/*---------------------------------------------------------
  VistParams.c
---------------------------------------------------------*/

#include "VistParams.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"
#include "language.h"

#define VIST_PARAMS_COUNT             2

static const WORD VistParamsObjectResIds[] = {
  528, 530
};
const WORD VistParamsTypeResIds[] = {
  555, 534, 549
};

NOINIT BYTE VistParamsObject;
NOINIT TVistParamsType VistParamsType;
static NOINIT BYTE VistParamsTab;

static void VistParamsGetEdit(void);
static void VistParamsSetEdit(void);
static WORD VistParamsGetY(void);
static WORD VistParamsGetYbyTab(BYTE tab);
static WORD VistParamsGetW(void);

//---------------------------------------------------------
void InitVistParams(void)
{
  VistParamsTab = 0;
  LoadFirstProducedVistParams();
}

//---------------------------------------------------------
void ShowVistParamsIni(void)
{
  PrgVerb = VB_VIST_PARAMS;
  LcdRepaint();
}

//---------------------------------------------------------
void ShowVistParams(void)
{
  BOOL tab_changed = FALSE;

  if(ProcessStandardKeyDownActions() == TRUE) {
    VistParamsGetEdit();
    SaveVistParams();
  }

  if(KeyDown == KB_UP || KeyDown == KB_DOWN) {
    VistParamsGetEdit();
    if(VistParamsTab-- == 0)
      VistParamsTab = VIST_PARAMS_COUNT - 1;
    tab_changed = TRUE;
  }

  if(LcdFlags.Repaint || tab_changed == TRUE) {
    WORD i;
    WORD y;
    WORD caption_ids[] = {432, 424};
    WORD id;

    LcdDrawBegin();

    if(LcdFlags.Repaint) {
      LcdDrawWorkspace();
      for(i = 0; i < VIST_PARAMS_COUNT; i++)
        LcdDrawLangCaptionY(VistParamsGetYbyTab(i) - 56, caption_ids[i]);
      LcdSpinEditCreate(NULL);
    }
    else {
      LcdSetFgColor(LcdGetBgColor());
      LcdDrawRect(LcdSpinEdit.X, LcdSpinEdit.Y, LcdSpinEdit.W, LcdSpinEdit.H);
    }

    VistParamsSetEdit();
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdSetTextColorDef();

    for(i = 0; i < VIST_PARAMS_COUNT; i++) {
      if(i == VistParamsTab)
        continue;

      y = VistParamsGetYbyTab(i) - 7;

      if(i == 0)
        id = VistParamsObjectResIds[VistParamsObject];
      else
        id = VistParamsTypeResIds[VistParamsType];
      LcdDrawCaLangText(LCD_W/2, y, id);
    }

    LcdDrawEnd();
  }
  else {
    LcdSpinEditDo(NULL);
  }
}

//---------------------------------------------------------
void LoadVistParams(void)
{
  BYTE b = CfgReadByte(CFG_VIST_PARAMSOBJECT);
  if(b < VIST_PARAMS_OBJECTS_COUNT)
    VistParamsObject = b;
  b = CfgReadByte(CFG_VIST_PARAMSTYPE);
  if(b < VIST_PARAMS_TYPES_COUNT)
    VistParamsType = (TVistParamsType)b;
}

//---------------------------------------------------------
void SaveVistParams(void)
{
  CfgWriteByte(CFG_VIST_PARAMSOBJECT, VistParamsObject);
  CfgWriteByte(CFG_VIST_PARAMSTYPE, VistParamsType);
}

//---------------------------------------------------------
void LoadFirstProducedVistParams(void)
{
  VistParamsObject = 0;
  VistParamsType = vptVrms;
}

//---------------------------------------------------------
static void VistParamsGetEdit(void)
{
  INT32 val = LcdSpinEdit.Value;

  switch(VistParamsTab) {
    case 0:
      VistParamsObject = (BYTE)val;
      break;
    default:
      VistParamsType = (TVistParamsType)val;
  }
}

//---------------------------------------------------------
static void VistParamsSetEdit(void)
{
  INT32 max;
  INT32 val = 0;
  const WORD* res_ids = NULL;

  switch(VistParamsTab) {
    case 0:
      res_ids = VistParamsObjectResIds;
      max = VIST_PARAMS_OBJECTS_COUNT - 1;
      val = (INT32)VistParamsObject;
      break;
    default:
      res_ids = VistParamsTypeResIds;
      max = VIST_PARAMS_TYPES_COUNT - 1;
      val = (INT32)VistParamsType;
  }

  LcdSpinEdit.W                  = VistParamsGetW();
  LcdSpinEdit.X                  = LCD_W/2 - LcdSpinEdit.W/2;
  LcdSpinEdit.Y                  = VistParamsGetY() - 23;
  LcdSpinEdit.Max                = max;
  LcdSpinEdit.Value              = val;
  LcdSpinEdit.ResIds             = res_ids;
  LcdSpinEdit.Wrap               = TRUE;
  LcdSpinEdit.Edit.TextAlignment = lcdHaCenter;
  LcdSpinEdit.Edit.LetterSpacing = 0;
}

//---------------------------------------------------------
static WORD VistParamsGetY(void)
{
  return VistParamsGetYbyTab(VistParamsTab);
}

//---------------------------------------------------------
static WORD VistParamsGetYbyTab(BYTE tab)
{
  return 100 + 120*tab;
}

//---------------------------------------------------------
static WORD VistParamsGetW(void)
{
  if(!VistParamsTab)
    return 220;
  return LangId == lEnglish ? 92 : 124;
}

