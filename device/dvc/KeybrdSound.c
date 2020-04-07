/*---------------------------------------------------------
  KeybrdSound.c
---------------------------------------------------------*/

#include "KeybrdSound.h"
#include "lcd.h"
#include "cfg.h"
#include "keybrd.h"

static const WORD KeybrdSoundResIds[] = {
  905, 903,
  TEXTRESOURCE_EOF
};

NOINIT BOOL KeybrdSoundOn;

//---------------------------------------------------------
void InitKeybrdSound(void)
{
  LoadFirstProducedKeybrdSound();
}

//---------------------------------------------------------
void ShowKeybrdSoundIni(void)
{
  PrgVerb = VB_KEYBRDSOUND;
  
  LcdDrawBegin();
  LcdDrawWorkspaceWithLangCaption(919);  
  LcdMenuFactoryCreateEasy(KeybrdSoundResIds, KeybrdSoundOn == TRUE ? 1 : 0);  
  LcdDrawEnd();
}

//---------------------------------------------------------
void ShowKeybrdSound(void)
{
  if(ProcessStandardKeyDownActions() == TRUE) {
    KeybrdSoundOn = TO_BOOL(LcdMenu.Selected == 1);
    SaveKeybrdSound();
  }

  LcdMenuDo(NULL);
}

//---------------------------------------------------------
void LoadKeybrdSound(void)
{
  BYTE b = CfgReadByte(CFG_KEYBRDSOUNDON);
  if(b == (BYTE)TRUE || b == (BYTE)FALSE)
    KeybrdSoundOn = (BOOL)b;
}

//---------------------------------------------------------
void SaveKeybrdSound(void)
{
  CfgWriteByte(CFG_KEYBRDSOUNDON, KeybrdSoundOn);
}

//---------------------------------------------------------
void LoadFirstProducedKeybrdSound(void)
{
  KeybrdSoundOn = TRUE;
}

  
  
