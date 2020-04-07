#ifndef KEYBRD_DVC_H_INCLUDED
#define KEYBRD_DVC_H_INCLUDED

/*---------------------------------------------------------
  keybrd_dvc.h
---------------------------------------------------------*/

#include "xmain.h"

void InitKeybrd_pd(void);
WORD GetKey_pd(void);

#ifdef WIN32
void KeybrdSetKeys(WORD keys);
BOOL KeybrdIsPowerPressed(void);
#endif

#endif
