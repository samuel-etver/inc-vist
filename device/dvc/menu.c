/*---------------------------------------------------------
  menu.c
---------------------------------------------------------*/

#include "menu.h"
#include "lcd.h"
#include "keybrd.h"
#include "DeviceType.h"
#include "IncMeasureMode.h"
#include "language.h"

#define MENU_ITEM_H                     9
#define MENU_ITEMS_COUNT                20

typedef enum {
  MV_START,
  MV_MAIN,
  MV_TUNES,
  MV_DEVELOPER,

  MV_INC_MAIN,
  MV_INC_PARAMETERS,
  MV_INC_ARCHIVE,
  MV_INC_ADDITIONAL,

  MV_INCK_MAIN,
  MV_INCK_PARAMETERS,
  MV_INCK_ARCHIVE,
  MV_INCK_ADDITIONAL,

  MV_VIST_MAIN,
  MV_VIST_ARCHIVE,
  MV_VIST_ADDITIONAL,
} TMenuVerb;

#define MV_LAST                         0x1010U

#define MENU_LEVELS_COUNT               5

NOINIT BYTE MenuLevel;
static NOINIT TMenuVerb MenuVerb;
static NOINIT TLcdMenu MenuMenu[MENU_LEVELS_COUNT];
static NOINIT WORD MenuResIds[MENU_LEVELS_COUNT][MENU_ITEMS_COUNT];
static NOINIT const TPicture* MenuPics[MENU_LEVELS_COUNT][MENU_ITEMS_COUNT];

typedef struct {
  WORD Verb;
  BYTE VerbType;
  BYTE LevelChange;
  WORD ResId;
} TMenuData;

#define VT_PRG          0
#define VT_MENU         1

#define ML_UP           0
#define ML_DOWN         1
#define ML_CURR         2

static const TMenuData IncMainMenuData[] = {
  {VB_INC_PARAMS_INI,           VT_PRG,  ML_CURR,  514},
  {MV_INC_ARCHIVE,              VT_MENU, ML_DOWN,   63},
  {MV_INC_ADDITIONAL,           VT_MENU, ML_DOWN,   81},
  //{VB_MEASUREAMPLIFIERAUTO_INI, VT_PRG,  ML_CURR,    6},
  {MV_TUNES,                    VT_MENU, ML_DOWN,   95},
  {MV_LAST}
};
static const TMenuData IncParametersMenuData[] = {
  {MV_LAST}
};
static const TMenuData IncArchiveMenuData[] = {
  {MV_INC_MAIN,              VT_MENU, ML_UP,     63},
  {VB_INC_ARCHIVE_INI,       VT_PRG,  ML_CURR,  101},
  {VB_INC_MEMORY_INI,        VT_PRG,  ML_CURR,   67},
  {MV_LAST}
};
static const TMenuData IncAdditionalMenuData[] = {
  {MV_INC_MAIN,              VT_MENU, ML_UP,    589},
  {VB_CALENDAR_INI,          VT_PRG,  ML_CURR,   91},
  {VB_AUTOOFF_INI,           VT_PRG,  ML_CURR,   89},
  {VB_SUPPLYSOURCE_INI,      VT_PRG,  ML_CURR,   87},
  {VB_LANGUAGE_INI,          VT_PRG,  ML_CURR,   93},
  {VB_THEME_INI,             VT_PRG,  ML_CURR,  974},
  {VB_KEYBRDSOUND_INI,       VT_PRG,  ML_CURR,  919},
  {VB_ABOUTMANUFACTURER_INI, VT_PRG,  ML_CURR,  111},
  {VB_INC_TEST_INI,          VT_PRG,  ML_CURR,  647},
  {MV_LAST}
};

static const TMenuData InckMainMenuData[] = {
//  {VB_VIST_CALIBRATION_INI,VT_PRG,  ML_DOWN,  994},
  {VB_INC_PARAMS_INI,           VT_PRG,  ML_CURR,   LCD_MENU_ITEM_CB_ID},
  {MV_INCK_ARCHIVE,             VT_MENU, ML_DOWN,                    63},
  {MV_INCK_ADDITIONAL,          VT_MENU, ML_DOWN,                    81},
  {VB_INC_MEASUREMODE_INI,      VT_PRG,  ML_CURR,                   213},
  //{VB_MEASUREAMPLIFIERAUTO_INI, VT_PRG,  ML_CURR,                     6},
  {MV_TUNES,                    VT_MENU, ML_DOWN,                    95},
  {MV_LAST}
};
static const TMenuData InckParametersMenuData[] = {
  {MV_LAST}
};
static const TMenuData InckArchiveMenuData[] = {
  {MV_INCK_MAIN,             VT_MENU, ML_UP,     63},
  {VB_INC_ARCHIVE_INI,       VT_PRG,  ML_CURR,  101},
  {VB_INC_MEMORY_INI,        VT_PRG,  ML_CURR,   67},
  {MV_LAST}
};
static const TMenuData InckAdditionalMenuData[] = {
  {MV_INCK_MAIN,             VT_MENU, ML_UP,    589},
  {VB_CALENDAR_INI,          VT_PRG,  ML_CURR,   91},
  {VB_AUTOOFF_INI,           VT_PRG,  ML_CURR,   89},
  {VB_SUPPLYSOURCE_INI,      VT_PRG,  ML_CURR,   87},
  {VB_LANGUAGE_INI,          VT_PRG,  ML_CURR,   93},
  {VB_THEME_INI,             VT_PRG,  ML_CURR,  974},
  {VB_KEYBRDSOUND_INI,       VT_PRG,  ML_CURR,  919},
  {VB_ABOUTMANUFACTURER_INI, VT_PRG,  ML_CURR,  111},
  //{VB_VIST_SENSOR_INI,       VT_PRG,  ML_CURR,  158},
  {VB_INC_TEST_INI,          VT_PRG,  ML_CURR,  647},
  {MV_LAST}
};

static const TMenuData VistMainMenuData[] = {
  {VB_VIST_PARAMS_INI,          VT_PRG,  ML_CURR,  514},
  {MV_VIST_ARCHIVE,             VT_MENU, ML_DOWN,   63},
  {MV_VIST_ADDITIONAL,          VT_MENU, ML_DOWN,   81},
  //{VB_MEASUREAMPLIFIERAUTO_INI, VT_PRG,  ML_CURR,    6},
  {MV_TUNES,                    VT_MENU, ML_DOWN,   95},
  {MV_LAST}
};
static const TMenuData VistArchiveMenuData[] = {
  {MV_VIST_MAIN,             VT_MENU, ML_UP,     63},
  {VB_VIST_ARCHIVE_INI,      VT_PRG,  ML_CURR,  101},
  {VB_VIST_MEMORY_INI,       VT_PRG,  ML_CURR,   67},
  {MV_LAST}
};
static const TMenuData VistAdditionalMenuData[] = {
  {MV_VIST_MAIN,             VT_MENU, ML_UP,    589},
  {VB_CALENDAR_INI,          VT_PRG,  ML_CURR,   91},
  {VB_AUTOOFF_INI,           VT_PRG,  ML_CURR,   89},
  {VB_SUPPLYSOURCE_INI,      VT_PRG,  ML_CURR,   87},
  {VB_LANGUAGE_INI,          VT_PRG,  ML_CURR,   93},
  {VB_THEME_INI,             VT_PRG,  ML_CURR,  974},
  {VB_KEYBRDSOUND_INI,       VT_PRG,  ML_CURR,  919},
  {VB_ABOUTMANUFACTURER_INI, VT_PRG,  ML_CURR,  111},
  //{VB_VIST_SENSOR_INI,       VT_PRG,  ML_CURR,  158},
  {VB_VIST_TEST_INI,         VT_PRG,  ML_CURR,  647},
  {MV_LAST}
};

static const TMenuData TunesMenuData[] = {
  {MV_MAIN,                    VT_MENU, ML_UP,    95},
  {VB_FIRSTPRODUCED_INI,       VT_PRG,  ML_CURR, 486},
  {VB_DEVICETYPE_INI,          VT_PRG,  ML_CURR, 787},
  {VB_VIST_CALIBRATION_INI,    VT_PRG,  ML_DOWN, 994},
  {VB_IONCALIBRATION_INI,      VT_PRG,  ML_CURR, 799},
  {VB_ZEROCALIBRATION_INI,     VT_PRG,  ML_CURR, 241},
  {VB_MEASURINGTRACT_INI,      VT_PRG,  ML_CURR,  27},
  {VB_MEASUREHARDWARETEST_INI, VT_PRG,  ML_CURR,  49},
  {VB_ABOUTDEVICE_INI,         VT_PRG,  ML_CURR, 111},
  {MV_DEVELOPER,               VT_MENU, ML_DOWN, 436},
  {MV_LAST}
};
static const TMenuData DeveloperMenuData[] = {
  {MV_TUNES,                     VT_MENU, ML_UP,    436},
  {VB_FLASHDUMP_INI,             VT_PRG,  ML_CURR,  467},
  {MV_LAST}
};

static void MenuOnHome(void);
static void MenuOnEnd(void);
static void MenuOnSelect(void);
static const TMenuData* MenuGetMenuData(void);
static BYTE MenuGetMenuDataLen(void);
static BOOL MenuIsItemAvailable(BYTE index);
static BYTE MenuFilteredItemIndexToIndex(BYTE filtered_index);
static void MenuCreate(void);
static BYTE* MenuInckMainMenuGetText(WORD index);

//---------------------------------------------------------
void InitMenu(void)
{
  MenuVerb = MV_START;
  MenuLevel = 0;
}

//---------------------------------------------------------
void ShowMenuIni(void)
{
  PrgVerb = VB_MENU;

  LcdDrawBegin();

  LcdDrawWorkspace();

  if(MenuVerb == MV_START) {
    switch(DeviceType) {
      case dtInc:     MenuVerb = MV_INC_MAIN;  break;
      case dtIncVist: MenuVerb = MV_INCK_MAIN; break;
      default:        MenuVerb = MV_VIST_MAIN;
    }
    MenuCreate();
  }

  LcdMenuDraw(&MenuMenu[MenuLevel]);

  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowMenu(void)
{
  LcdMenuDo(&MenuMenu[MenuLevel]);

  switch(KeyDown) {
    case KB_MEASURE:
      PrgVerb = VB_MEASURE_INI;
      break;
    case KB_PLUS:
      MenuOnHome();
      break;
    case KB_MINUS:
      MenuOnEnd();
      break;
    case KB_MENU:
      MenuOnSelect();
  }
}

//---------------------------------------------------------
static void MenuOnHome(void)
{
  TLcdMenu* menu = &MenuMenu[MenuLevel];
  menu->FirstVisible = 0;
  menu->Selected     = 0;
  LcdMenuDraw(menu);
}

//---------------------------------------------------------
static void MenuOnEnd(void)
{
  TLcdMenu* menu = &MenuMenu[MenuLevel];
  menu->Selected = menu->Size - 1;
  if(menu->Size > menu->VisibleSize)
    menu->FirstVisible = menu->Selected - menu->VisibleSize + 1;
  LcdMenuDraw(menu);
}

//---------------------------------------------------------
static void MenuOnSelect(void)
{
  const TMenuData* md = MenuGetMenuData() +
    MenuFilteredItemIndexToIndex(MenuMenu[MenuLevel].Selected);

  if(md->VerbType == VT_PRG) {
    PrgVerb = (TProgramVerb)md->Verb;
    if(PrgVerb == VB_INC_PARAMS_INI) {
      if(IncMeasureMode == incMmVist && DeviceType == dtIncVist)
        PrgVerb = VB_VIST_PARAMS_INI;
    }
    return;
  }

  switch(md->Verb) {
    case MV_MAIN:
      switch(DeviceType) {
        case dtInc:     MenuVerb = MV_INC_MAIN;  break;
        case dtIncVist: MenuVerb = MV_INCK_MAIN; break;
        default:        MenuVerb = MV_VIST_MAIN;
      }
      break;
    case MV_TUNES:
      if(!PrgFlags.Password) {
        PrgVerb = VB_PASSWORD_INI;
        return;
      }
      MenuVerb = (TMenuVerb)md->Verb;
      break;
    default:
      MenuVerb = (TMenuVerb)md->Verb;
  }

  switch(md->LevelChange) {
    case ML_DOWN:
      MenuLevel++;
      MenuCreate();
      break;
    case ML_UP:
      MenuLevel--;
  }

  PrgVerb = VB_MENU_INI;
}

//---------------------------------------------------------
static const TMenuData* MenuGetMenuData(void)
{
  switch(MenuVerb) {
    case MV_TUNES:
      return TunesMenuData;
    case MV_DEVELOPER:
      return DeveloperMenuData;

    case MV_INC_MAIN:
      return IncMainMenuData;
    case MV_INC_PARAMETERS:
      return IncParametersMenuData;
    case MV_INC_ARCHIVE:
      return IncArchiveMenuData;
    case MV_INC_ADDITIONAL:
      return IncAdditionalMenuData;

    case MV_INCK_MAIN:
      return InckMainMenuData;
    case MV_INCK_PARAMETERS:
      return InckParametersMenuData;
    case MV_INCK_ARCHIVE:
      return InckArchiveMenuData;
    case MV_INCK_ADDITIONAL:
      return InckAdditionalMenuData;

    case MV_VIST_MAIN:
      return VistMainMenuData;
    case MV_VIST_ARCHIVE:
      return VistArchiveMenuData;
    case MV_VIST_ADDITIONAL:
      return VistAdditionalMenuData;

    default:;
  }

  return NULL;
}

//---------------------------------------------------------
static BYTE MenuGetMenuDataLen(void)
{
  WORD i = 0, n = 0;
  const TMenuData* md = MenuGetMenuData();

  do {
    if(MenuIsItemAvailable(i) == TRUE) ++n;
  } while(md[++i].Verb != MV_LAST);

  return n;
}

//---------------------------------------------------------
void MenuPasswordEntered(void)
{
  MenuOnSelect();
}

//---------------------------------------------------------
static BOOL MenuIsItemAvailable(BYTE index)
{
  const TMenuData* md = MenuGetMenuData() + index;

  if(md->VerbType == VT_MENU) {
    if(md->Verb == MV_TUNES && PrgFlags.SuperUser == 0)
      return FALSE;
  }
  else if(md->VerbType == VT_PRG) {
    if(md->Verb == VB_VIST_SENSOR_INI)
      if(DeviceType == dtIncVist && IncMeasureMode == incMmInc)
        return FALSE;
  }

  return TRUE;
}

//---------------------------------------------------------
static BYTE MenuFilteredItemIndexToIndex(BYTE filtered_index)
{
  const TMenuData* md = MenuGetMenuData();
  BYTE i = 0, index = 0;

  while((md + index)->Verb != MV_LAST) {
    if(MenuIsItemAvailable(index) == FALSE) {
      ++index; continue;
    }

    if(i == filtered_index)
      return index;

    ++i; ++index;
  }

  return 0;
}

//---------------------------------------------------------
static void MenuCreate(void)
{
  WORD i, n;
  WORD index;
  TLcdMenu* menu;
  const TMenuData* menu_data;
  const TPicture** pics;

  menu_data = MenuGetMenuData();
  n         = MenuGetMenuDataLen();
  menu      = &MenuMenu[MenuLevel];
  pics      = MenuPics[MenuLevel];
  for(i = 0; i < n; i++) {
    index = MenuFilteredItemIndexToIndex(i);
    MenuResIds[MenuLevel][i] = menu_data[index].ResId;
    if(menu_data[index].VerbType == VT_PRG)
      pics[i] = NULL;
    else if(menu_data[index].LevelChange == ML_UP)
      pics[i] = PicturesMenuUp;
    else
      pics[i] = PicturesMenuDown;
  }
  MenuResIds[MenuLevel][i] = TEXTRESOURCE_EOF;

  LcdMenuCreate(menu, MenuResIds[MenuLevel]);
  menu->Wrap        = FALSE;
  menu->Pics        = pics;
  menu->AlignLeft   = TRUE;
  menu->MarginRight = 26;
  menu->Font        = FONT13;
  if(MenuLevel == 0 && DeviceType == dtIncVist) {
    menu->GetText = MenuInckMainMenuGetText;
  }
  LcdMenuSetup(menu);
  menu->ItemH += 3;
  menu->X    = 1;
  menu->Y    = LCD_STATUS_H + 16;
  menu->W    = LCD_W - 2;
}

//---------------------------------------------------------
void MenuDeviceTypeChanged(void)
{
  BYTE saved_selected = MenuMenu[MenuLevel].Selected;
  TLcdMenu* menu;

  switch(DeviceType) {
    case dtInc:     MenuVerb = MV_INC_MAIN;  break;
    case dtIncVist: MenuVerb = MV_INCK_MAIN; break;
    case dtVist:    MenuVerb = MV_VIST_MAIN;
    default:;
  }

  menu = &MenuMenu[MenuLevel = 0];
  MenuCreate();
  menu->Selected = menu->Size - 1;
  if(menu->VisibleSize < menu->Size)
    menu->FirstVisible = menu->Size - menu->VisibleSize;

  menu = &MenuMenu[++MenuLevel];
  MenuVerb = MV_TUNES;
  MenuCreate();
  menu->Selected = saved_selected;
}

//---------------------------------------------------------
static BYTE* MenuInckMainMenuGetText(WORD index)
{
  BYTE* buff = LcdText + 1;
  BYTE n = LoadLangResource(buff, 516);  
  if(LangId == lEnglish) {
    buff[n++] = '(';
  }
  n += LoadLangResource(buff + n, IncMeasureMode == incMmInc ? 775 : 777);
  if(LangId == lEnglish) {
    buff[n++] = ')';
  }
  LcdText[0] = n;
  return LcdText;
}

//---------------------------------------------------------
void MenuThemeChange(void)
{
  WORD i;
  TLcdMenu* menu;
  for(i = 0; i < MENU_LEVELS_COUNT; i++) {
    menu = &MenuMenu[i];
    menu->Balloon = LcdMenuBalloon;
  }
}
