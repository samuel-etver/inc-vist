/*---------------------------------------------------------
  IncParams.c
---------------------------------------------------------*/

#include "IncParams.h"
#include "lcd.h"
#include "keybrd.h"
#include "language.h"
#include "cfg.h"
#include "utils.h"

#define INC_PARAMSLENGTH_MIN      1U
#define INC_PARAMSLENGTH_MAX      9999U
#define INC_PARAMSDIAMETER_MIN    1U
#define INC_PARAMSDIAMETER_MAX    99U
#define INC_PARAMSTENSION_MIN     1U
#define INC_PARAMSTENSION_MAX     9999U
#define INC_PARAMSUNITS_COUNT     2U

#define INC_PARAMS_COUNT          4U

#define INC_PARAMS_X1             12U
#define INC_PARAMS_X2             228U

#define INC_PARAMS_Y1             60U
#define INC_PARAMS_Y2             120U
#define INC_PARAMS_Y3             180U
#define INC_PARAMS_Y4             240U

static const WORD IncParamsLabelIds[] = {
  649, 651, 144, 79
};
static const WORD IncParamsUnitsResIds[] = {
  21, 23
};
static const WORD IncParamsLabelYs[] = {
  INC_PARAMS_Y1,
  INC_PARAMS_Y2,
  INC_PARAMS_Y3,
  INC_PARAMS_Y4
};

NOINIT WORD IncParamsLength;
NOINIT BYTE IncParamsDiameter;
NOINIT WORD IncParamsTension;
NOINIT BYTE IncParamsUnits;
NOINIT static BYTE IncParamsLengthPos;
NOINIT static BYTE IncParamsDiameterPos;
NOINIT static BYTE IncParamsTensionPos;
NOINIT static BYTE IncParamsTab;

static void IncParamsGetEdit(void);
static void IncParamsSetEdit(void);

//---------------------------------------------------------
void InitIncParams(void)
{
  IncParamsLengthPos = 0;
  IncParamsDiameterPos = 0;
  IncParamsTensionPos = 0;
  IncParamsTab = 0;
  LoadFirstProducedIncParams();
}

//---------------------------------------------------------
void ShowIncParamsIni(void)
{
  PrgVerb = VB_INC_PARAMS;
  LcdRepaint();
}

//---------------------------------------------------------
void ShowIncParams(void)
{
  BOOL tab_changed = FALSE;
  TLcdSpinEdit* se = &LcdSpinEdit;
  WORD i;
  BYTE n;
  WORD y;

  if(ProcessStandardKeyDownActions() == TRUE) {
    IncParamsGetEdit();
    SaveIncParams();
  }

  if(KeyDown == KB_UP) {
    IncParamsGetEdit();
    if(IncParamsTab-- == 0)
      IncParamsTab = INC_PARAMS_COUNT - 1;
    tab_changed = TRUE;
  }

  if(KeyDown == KB_DOWN) {
    IncParamsGetEdit();
    if(++IncParamsTab == INC_PARAMS_COUNT)
      IncParamsTab = 0;
    tab_changed = TRUE;
  }

  if(LcdFlags.Repaint) {
    LcdDrawBegin();

    LcdDrawWorkspace();

    LcdSetFont(FONT10);
    for(i = 0; i < INC_PARAMS_COUNT; i++) {
      if(i & 1)
        LcdSetEvenRowTextColorDef();
      else
        LcdSetTextColorDef();
      if(i == 2) {
        LcdDrawLaLangText(INC_PARAMS_X1, IncParamsLabelYs[i] - LcdFontHeight/2U,
         IncParamsLabelIds[i]);
        LcdDrawLaLangText(INC_PARAMS_X1, IncParamsLabelYs[i] + LcdFontHeight/2U,
         IncParamsLabelIds[i] + LANG_COUNT);
      }
      else {
        LcdDrawLaLangText(INC_PARAMS_X1, IncParamsLabelYs[i], IncParamsLabelIds[i]);
      }
    }
    LcdSetTextColor(LcdLabelTextColorDef);

    IncParamsSetEdit();

    LcdDrawEnd();
  }
  else if(tab_changed == TRUE) {
    LcdDrawBegin();
    LcdSpinEditUnmap(se);
    IncParamsSetEdit();
    LcdDrawEnd();
  }

  if(LcdFlags.Repaint || tab_changed == TRUE) {
    LcdDrawBegin();

    LcdSetFont(FONT10);
    LcdSetLetterSpacing(se->Edit.LetterSpacing);

    for(i = 0; i < INC_PARAMS_COUNT; i++) {
      if(i == IncParamsTab)
        continue;

      if(i & 1)
        LcdSetEvenRowTextColorDef();
      else
        LcdSetTextColorDef();

      y = IncParamsLabelYs[i];

      switch(i) {
        case 0:
          n = IncParamsLengthToStr(LcdText, IncParamsLength);
          break;
        case 1:
          n = IncParamsDiameterToStr(LcdText, IncParamsDiameter);
          break;
        case 2:
          n = IncParamsTensionToStr(LcdText, IncParamsTension);
          break;
        default:
          n = IncParamsUnitsToStr(LcdText, IncParamsUnits);
      }
      LcdDrawRaText(INC_PARAMS_X2, y, LcdText, n);
    }

    LcdDrawEnd();
  }

  LcdSpinEditDo(se);
}

//---------------------------------------------------------
void LoadIncParams(void)
{
  WORD w = CfgReadByte(CFG_INC_PARAMSUNITS);
  if(w < INC_PARAMSUNITS_COUNT)
    IncParamsUnits = (BYTE)w;

  w = CfgReadByte(CFG_INC_PARAMSDIAMETER);
  if(w <= INC_PARAMSDIAMETER_MAX && w >= INC_PARAMSDIAMETER_MIN)
    IncParamsDiameter = (BYTE)w;

  w = CfgReadWord(CFG_INC_PARAMSLENGTH);
  if(w <= INC_PARAMSLENGTH_MAX && w >= INC_PARAMSLENGTH_MIN)
    IncParamsLength = w;

  w = CfgReadWord(CFG_INC_PARAMSTENSION);
  if(w <= INC_PARAMSTENSION_MAX && w >= INC_PARAMSTENSION_MIN)
    IncParamsTension = w;
}

//---------------------------------------------------------
void SaveIncParams(void)
{
  CfgWriteByte(CFG_INC_PARAMSUNITS, IncParamsUnits);
  CfgWriteByte(CFG_INC_PARAMSDIAMETER, IncParamsDiameter);
  CfgWriteWord(CFG_INC_PARAMSLENGTH, IncParamsLength);
  CfgWriteWord(CFG_INC_PARAMSTENSION, IncParamsTension);
}

//---------------------------------------------------------
void LoadFirstProducedIncParams(void)
{
  IncParamsLength = 100;
  IncParamsDiameter = INC_PARAMSDIAMETER_MIN;
  IncParamsTension = 100;
  IncParamsUnits = 0;
}

//---------------------------------------------------------
static void IncParamsGetEdit(void)
{
  INT32 val = LcdSpinEdit.Value;
  BYTE pos = LcdSpinEdit.Pos;

  switch(IncParamsTab) {
    case 0:
      IncParamsLength = (WORD)val;
      IncParamsLengthPos = pos;
      break;
    case 1:
      IncParamsDiameter = (BYTE)val;
      IncParamsDiameterPos = pos;
      break;
    case 2:
      IncParamsTension = (WORD)val;
      IncParamsTensionPos = pos;
      break;
    default:
      IncParamsUnits = (BYTE)val;
  }
}

//---------------------------------------------------------
static void IncParamsSetEdit(void)
{
  TLcdSpinEdit* se = &LcdSpinEdit;
  BYTE text_len = 0;
  INT32 min;
  INT32 max;
  INT32 val;
  BYTE pos = 0;
  BYTE comma = 0;
  const WORD* res_ids = NULL;
  BOOL wrap = FALSE;

  LcdSpinEditCreate(se);

  switch(IncParamsTab) {
    case 0:
      text_len = 5;
      min = 1;
      max = 9999;
      val = IncParamsLength;
      pos = IncParamsLengthPos;
      comma = 2;
      break;
    case 1:
      text_len = 2;
      min = 1;
      max = 99;
      val = IncParamsDiameter;
      pos = IncParamsDiameterPos;
      break;
    case 2:
      text_len = 4;
      min = 1;
      max = 9999;
      val = IncParamsTension;
      pos = IncParamsTensionPos;
      break;
    default:
      min = 0;
      max = ARRAY_LENGTH(IncParamsUnitsResIds) - 1;
      val = IncParamsUnits;
      res_ids = IncParamsUnitsResIds;
      wrap = TRUE;
  }

  se->W            = 88U;
  se->X            = INC_PARAMS_X2 - se->W;
  se->Y            = IncParamsLabelYs[IncParamsTab] - 15U;
  se->TextLen      = text_len;
  se->Min          = min;
  se->Max          = max;
  se->Value        = val;
  se->EnableLR     = TRUE;
  se->Pos          = pos;
  se->Comma        = comma;
  se->ResIds       = res_ids;
  se->Wrap         = wrap;
  se->Edit.Font    = FONT10;
  se->Edit.Balloon = LcdEditSmallBalloon;

  LcdSpinEditSetup(se);
  LcdSpinEditDraw(se);
}

//---------------------------------------------------------
BYTE IncParamsLengthToStr(BYTE* buff, WORD length)
{
  WordToStrAsFloatWithLeadingSpaces(buff, length, 5, 2);
  return 5;
}

//---------------------------------------------------------
BYTE IncParamsDiameterToStr(BYTE* buff, BYTE diameter)
{
  WordToStrWithLeadingSpaces(buff, diameter, 2);
  return 2;
}

//---------------------------------------------------------
BYTE IncParamsTensionToStr(BYTE* buff, WORD tension)
{
  WordToStrWithLeadingSpaces(buff, tension, 4);
  return 4;
}

//---------------------------------------------------------
BYTE IncParamsUnitsToStr(BYTE* buff, BYTE units)
{
  return LoadLangResource(buff, IncParamsUnitsResIds[units]);
}

