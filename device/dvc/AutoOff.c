/*---------------------------------------------------------
  autooff.c
---------------------------------------------------------*/

#include "autooff.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"
#include "system.h"
#include "language.h"
#include "power.h"
#include "accelerometer.h"
#include "light.h"

#define AUTOOFF_POWER_COUNT     ARRAY_LENGTH(AutoOffPowerClkMax)
#define AUTOOFF_LIGHT_COUNT     ARRAY_LENGTH(AutoOffLightClkMax)
#define AUTOOFF_CONTRAST_MIN    10
#define AUTOOFF_CONTRAST_MAX    100
#define AUTOOFF_CONTRAST_NOM    80

#define AUTOOFF_POWER_Y         78
#define AUTOOFF_LIGHT_Y         178
#define AUTOOFF_CONTRAST_Y      258
#define AUTOOFF_CONTRAST_DELTA  10

static NOINIT BYTE AutoOffPower;
static NOINIT WORD AutoOffPowerClk;
static NOINIT BYTE AutoOffLight;
static NOINIT WORD AutoOffLightClk;
static NOINIT BYTE AutoOffContrast;
static NOINIT BYTE AutoOffTab;

static const WORD AutoOffPowerClkMax[] = {
       0,
   5*600,
  10*600,
  15*600,
  20*600,
  25*600,
  30*600
};
static const WORD AutoOffLightClkMax[] = {
  0,
  100,
  150,
  200,
  250,
  300,
  450,
  600
};

static BYTE* AutoOffPowerGetText(void* o);
static BYTE* AutoOffLightGetText(void* o);
static BYTE* AutoOffContrastGetText(void* o);

//---------------------------------------------------------
void InitAutoOff(void)
{
  AutoOffPowerClk = 0;
  AutoOffLightClk = 0;
  AutoOffTab = 0;
  LoadFirstProducedAutoOff();
}

//---------------------------------------------------------
void ShowAutoOffIni(void)
{
  PrgVerb = VB_AUTOOFF;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(FONT10);
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdDrawCaLangText(LCD_W/2, 36, 260);
  LcdDrawCaLangText(LCD_W/2, 56, 262);
  LcdDrawCaLangText(LCD_W/2, 136, 260);
  LcdDrawCaLangText(LCD_W/2, 156, 264);
  LcdDrawCaLangText(LCD_W/2, 236, 279);

  LcdDrawEnd();
  LcdRepaint();
}

//---------------------------------------------------------
void ShowAutoOff(void)
{
  if(ProcessStandardKeyDownActions() == TRUE)
    SaveAutoOff();

  if(KeyDown == KB_UP) {
    if(AutoOffTab-- == 0)
      AutoOffTab = 2;
    LcdRepaint();
  }

  if(KeyDown == KB_DOWN) {
    if(++AutoOffTab == 3)
      AutoOffTab = 2;
    LcdRepaint();
  }

  if(KeyDown == KB_MINUS) {
    switch(AutoOffTab) {
      case 0:
        if(AutoOffPower-- == 0)
          AutoOffPower = AUTOOFF_POWER_COUNT-1;
        break;
      case 1:
        if(AutoOffLight-- == 0)
          AutoOffLight = AUTOOFF_LIGHT_COUNT-1;
        break;
      case 2:
        if((AutoOffContrast -= AUTOOFF_CONTRAST_DELTA) < AUTOOFF_CONTRAST_MIN)
          AutoOffContrast = AUTOOFF_CONTRAST_MAX;
        LcdSetContrast(AutoOffContrast);
    }
  }

  if(KeyDown == KB_PLUS) {
    switch(AutoOffTab) {
      case 0:
        if(++AutoOffPower == AUTOOFF_POWER_COUNT)
          AutoOffPower = 0;
        break;
      case 1:
        if(++AutoOffLight == AUTOOFF_LIGHT_COUNT)
          AutoOffLight = 0;
        break;
      case 2:
        if((AutoOffContrast += AUTOOFF_CONTRAST_DELTA) > AUTOOFF_CONTRAST_MAX)
          AutoOffContrast = AUTOOFF_CONTRAST_MIN;
        LcdSetContrast(AutoOffContrast);
    }
  }

  if(LcdFlags.Repaint) {
    WORD y;
    BYTE* buff;
    WORD i;

    LcdDrawBegin();

    LcdSpinEditCreate(NULL);
    LcdSpinEdit.W = 98;
    LcdSpinEdit.X = LCD_W/2 - LcdSpinEdit.W/2;
    LcdSpinEdit.Edit.TextAlignment = lcdHaCenter;
    switch(AutoOffTab) {
      case 0:
        LcdSpinEdit.GetText = AutoOffPowerGetText;
        LcdSpinEdit.Y = AUTOOFF_POWER_Y;
        break;
      case 1:
        LcdSpinEdit.GetText = AutoOffLightGetText;
        LcdSpinEdit.Y = AUTOOFF_LIGHT_Y;
        break;
      default:
        LcdSpinEdit.GetText = AutoOffContrastGetText;
        LcdSpinEdit.Y = AUTOOFF_CONTRAST_Y;
    }
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdSetTextColorDef();
    LcdSetFont(LCD_ITEM_FONT);
    LcdSetFgColor(LcdGetBgColor());
    for(i = 0; i < 3; i++) {
      if(AutoOffTab == i)
        continue;
      switch(i) {
        case 0:
          y = AUTOOFF_POWER_Y;
          buff = AutoOffPowerGetText(NULL);
          break;
        case 1:
          y = AUTOOFF_LIGHT_Y;
          buff = AutoOffLightGetText(NULL);
          break;
        default:
          y = AUTOOFF_CONTRAST_Y;
          buff = AutoOffContrastGetText(NULL);
      }
      LcdDrawRect(LcdSpinEdit.X, y, LcdSpinEdit.W, LcdSpinEdit.H);
      LcdDrawCaText(LCD_W/2, y + 15U, buff + 1, buff[0]);
    }

    LcdDrawEnd();
  }
  else
    LcdSpinEditDo(NULL);
}

//---------------------------------------------------------
void LoadAutoOff(void)
{
  BYTE b = CfgReadByte(CFG_AUTOOFF_POWER);
  if(b < AUTOOFF_POWER_COUNT)
    AutoOffPower = b;
  b = CfgReadByte(CFG_AUTOOFF_LIGHT);
  if(b < AUTOOFF_LIGHT_COUNT)
    AutoOffLight = b;
  b = AUTOOFF_CONTRAST_DELTA*
   (CfgReadByte(CFG_AUTOOFF_CONTRAST)/AUTOOFF_CONTRAST_DELTA);
  if(b <= AUTOOFF_CONTRAST_MAX && b >= AUTOOFF_CONTRAST_MIN)
    AutoOffContrast = b;
  LcdSetContrast(AutoOffContrast);
}

//---------------------------------------------------------
void SaveAutoOff(void)
{
  CfgWriteByte(CFG_AUTOOFF_POWER, AutoOffPower);
  CfgWriteByte(CFG_AUTOOFF_LIGHT, AutoOffLight);
  CfgWriteByte(CFG_AUTOOFF_CONTRAST, AutoOffContrast);
}

//---------------------------------------------------------
void LoadFirstProducedAutoOff(void)
{
  AutoOffPower = AUTOOFF_POWER_COUNT - 1;
  AutoOffLight = 5; // 30 s
  AutoOffContrast = AUTOOFF_CONTRAST_NOM;
}

//---------------------------------------------------------
void DoAutoOff(void)
{
  BOOL f = FALSE;

  switch(PrgVerb) {
    case VB_MEASURE:
    case VB_MEASURINGTRACT:
    case VB_MEASUREHARDWARETEST:
    case VB_INC_TEST:
    case VB_VIST_TEST:
      f = TRUE;
    default:
      ;
  }


  if(PrgVerb >= VB_MENU && f == FALSE) {
    if(AccelerometerActive == TRUE || KeyDown != KB_NOKEY) {
      if(Light == FALSE)
        LightOn();
      ClearAutoOff();
      AutoOffLightClk = 0;
    }
    else {
      if(PrgFlags.CentiSec)
        if(AutoOffLight != 0)
          if(++AutoOffLightClk >= AutoOffLightClkMax[AutoOffLight]) {
            if(Light == TRUE)
              LightOff();
          }
    }
  }

  if(AutoOffPower == 0) return;

  if(KeyDown != KB_NOKEY || f == TRUE)
    ClearAutoOff();
  else if(PrgFlags.CentiSec)
    if(++AutoOffPowerClk > AutoOffPowerClkMax[AutoOffPower])
      Halt();
}

//---------------------------------------------------------
void ClearAutoOff(void)
{
  AutoOffPowerClk = 0;
}

//---------------------------------------------------------
static BYTE* AutoOffPowerGetText(void* o)
{
  BYTE n;
  WORD id = AutoOffPower == 0 ? LangId + 945 : 268 - 1 + AutoOffPower;
  n = LoadTextResource(LcdText + 1, id);
  if(AutoOffPower != 0) {
    //LcdText[++n] = ' ';
    n += LoadLangResource(LcdText + n + 1, 266);
  }
  LcdText[0] = n;
  return LcdText;
}

//---------------------------------------------------------
static BYTE* AutoOffLightGetText(void* o)
{
  BYTE n;
  if(AutoOffLight == 0)
    LcdText[0] = LoadLangResource(LcdText + 1, 945);
  else {
    WordToStrWithLeadingSpaces(LcdText + 1,
     AutoOffLightClkMax[AutoOffLight]/10U, 3);
    n = StripDecimal(LcdText + 1, 3);
    //LcdText[++n] = ' ';
    LcdText[0] = n + LoadLangResource(LcdText + n + 1, 274);
  }
  return LcdText;
}

//---------------------------------------------------------
static BYTE* AutoOffContrastGetText(void* o)
{
  LcdText[0] = (BYTE)sprintf((char*)LcdText + 1, "%i%%", (int)AutoOffContrast);
  return LcdText;
}
