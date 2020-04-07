#ifndef SOUND_H_INCLUDED
#define SOUND_H_INCLUDED

/*---------------------------------------------------------
  sound.h
---------------------------------------------------------*/

#include "xmain.h"

void InitSound(void);

void DoSound(void);
void SoundPlayClick(void);

void SoundSetFastFrequencyBefore(void);
void SoundSetNormalFrequencyBefore(void);

void SoundOn(void);
void SoundOff(void);

#endif
