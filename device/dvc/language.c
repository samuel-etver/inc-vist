/*---------------------------------------------------------
  language.c
---------------------------------------------------------*/

#include "language.h"
#include "lcd.h"
#include "keybrd.h"
#include "cfg.h"

NOINIT TLanguage LangId;

static const WORD LanguageResIds[] = {
  2, 4, 
  TEXTRESOURCE_EOF
};

//---------------------------------------------------------
void InitLanguage(void)
{
  LoadFirstProducedLanguage();
}

//---------------------------------------------------------
void ShowLanguageIni(void)
{
  PrgVerb = VB_LANGUAGE;
  
  LcdDrawBegin();  
  LcdDrawWorkspaceWithLangCaption(93);  
  LcdMenuFactoryCreateEasy(LanguageResIds, LangId);  
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowLanguage(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    LangId = (TLanguage)LcdMenu.Selected;
    SaveLanguage();
  }

  LcdMenuDo(NULL);
}

//---------------------------------------------------------
void LoadLanguage(void)
{
  BYTE b = CfgReadByte(CFG_LANGUAGE);
  if(b < LANG_COUNT)
    LangId = (TLanguage)b;
}

//---------------------------------------------------------
void SaveLanguage(void)
{
  CfgWriteByte(CFG_LANGUAGE, LangId);
}

//---------------------------------------------------------
void LoadFirstProducedLanguage(void)
{
  LangId = lRussian;
}
