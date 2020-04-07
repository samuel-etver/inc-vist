/*---------------------------------------------------------
  keybrd_pd.c
---------------------------------------------------------*/

#include "keybrd_pd.h"
#include "keybrd.h"

static WORD KeybrdKeys;

//---------------------------------------------------------
void InitKeybrd_pd(void)
{
  KeybrdKeys = 0;
}

//---------------------------------------------------------
WORD GetKey_pd(void)
{
  WORD kb = 0;

  if(KeybrdKeys & 0x0001) kb |= KB_F1;
  if(KeybrdKeys & 0x0002) kb |= KB_F2;
  if(KeybrdKeys & 0x0004) kb |= KB_F3;
  if(KeybrdKeys & 0x0008) kb |= KB_MEASURE;
  if(KeybrdKeys & 0x0010) kb |= KB_UP;
  if(KeybrdKeys & 0x0020) kb |= KB_PLUS;
  if(KeybrdKeys & 0x0040) kb |= KB_LEFT;
  if(KeybrdKeys & 0x0080) kb |= KB_MENU;
  if(KeybrdKeys & 0x0100) kb |= KB_RIGHT;
  if(KeybrdKeys & 0x0200) kb |= KB_POWER;
  if(KeybrdKeys & 0x0400) kb |= KB_DOWN;
  if(KeybrdKeys & 0x0800) kb |= KB_MINUS;

  return kb;
}

//---------------------------------------------------------
void KeybrdSetKeys(WORD keys)
{
  KeybrdKeys = keys;
}

//---------------------------------------------------------
BOOL KeybrdIsPowerPressed(void)
{
  if(KeybrdKeys & 0x0200)
    return TRUE;
  return FALSE;
}
