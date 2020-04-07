/*---------------------------------------------------------
  Theme.c
---------------------------------------------------------*/

#include "Theme.h"
#include "keybrd.h"
#include "lcd.h"
#include "cfg.h"
#include "Status.h"
#include "menu.h"

NOINIT TTheme Theme;

static const WORD ThemeResIds[] = {
  980, 976, 978,
  TEXTRESOURCE_EOF
};

//---------------------------------------------------------
void InitTheme(void)
{
  LoadFirstProducedTheme();
}

//---------------------------------------------------------
void ShowThemeIni(void)
{
  PrgVerb = VB_THEME;

  LcdDrawBegin();
  LcdDrawWorkspaceWithLangCaption(974);
  LcdMenuFactoryCreateEasy(ThemeResIds, Theme);
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowTheme(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    if(Theme != (TTheme)LcdMenu.Selected) {
      Theme = (TTheme)LcdMenu.Selected;
      SaveTheme();
      ThemeChange();
    }
  }

  LcdMenuDo(NULL);
}

//---------------------------------------------------------
void LoadTheme(void)
{
  BYTE b = CfgReadByte(CFG_THEME);
  if(b < THEME_COUNT)
    Theme = (TTheme)b;
}

//---------------------------------------------------------
void SaveTheme(void)
{
  CfgWriteByte(CFG_THEME, Theme);
}

//---------------------------------------------------------
void LoadFirstProducedTheme(void)
{
  Theme = themeSimple;
}

//---------------------------------------------------------
void ThemeChange(void)
{
  LcdApplyTheme();
  LcdDrawBegin();
  LcdDrawBackground();
  LcdDrawEnd();
  StatusRestart();
  MenuThemeChange();
}
