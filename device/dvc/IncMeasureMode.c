/*---------------------------------------------------------
  IncMeasureMode.c
---------------------------------------------------------*/

#include "IncMeasureMode.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"

#define MEASUREMODE_COUNT   ((BYTE)mmLast)

static const WORD IncMeasureModeResIds[] = {
  607, 609,
  TEXTRESOURCE_EOF
};

NOINIT TIncMeasureMode IncMeasureMode;

//---------------------------------------------------------
void InitIncMeasureMode(void)
{
  LoadFirstProducedIncMeasureMode();
}

//---------------------------------------------------------
void ShowIncMeasureModeIni(void)
{
  PrgVerb = VB_INC_MEASUREMODE;
  LcdDrawBegin();
  LcdDrawWorkspaceWithLangCaption(215);
  LcdMenuFactoryCreateEasy(IncMeasureModeResIds, IncMeasureMode);  
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowIncMeasureMode(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    IncMeasureMode = (TIncMeasureMode)LcdMenu.Selected;
    SaveIncMeasureMode();
  }

  LcdMenuDo(NULL);
}

//---------------------------------------------------------
void LoadIncMeasureMode(void)
{
  BYTE b = CfgReadByte(CFG_INC_MEASUREMODE);
  if(b < INC_MEASUREMODE_COUNT)
    IncMeasureMode = (TIncMeasureMode)b;
}

//---------------------------------------------------------
void SaveIncMeasureMode(void)
{
  CfgWriteByte(CFG_INC_MEASUREMODE, IncMeasureMode);
}

//---------------------------------------------------------
void LoadFirstProducedIncMeasureMode(void)
{
  IncMeasureMode = incMmInc;
}
