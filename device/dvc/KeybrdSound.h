#ifndef KEYBRDSOUND_H_INCLUDED
#define KEYBRDSOUND_H_INCLUDED

/*---------------------------------------------------------
  KeybrdSound.h
---------------------------------------------------------*/

#include "xmain.h"

void InitKeybrdSound(void);
void ShowKeybrdSoundIni(void);
void ShowKeybrdSound(void);

void LoadKeybrdSound(void);
void SaveKeybrdSound(void);

void LoadFirstProducedKeybrdSound(void);

extern NOINIT BOOL KeybrdSoundOn;

#endif
