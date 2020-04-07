/*---------------------------------------------------------
  sound.c
---------------------------------------------------------*/

#include "sound.h"
#include "sound_pd.h"
#include "delay.h"
#include "keybrd.h"
#include "measure.h"
#include "light.h"

static NOINIT TDelay SoundDelay;

//---------------------------------------------------------
void InitSound(void)
{
  InitSound_pd();
}

//---------------------------------------------------------
void DoSound(void)
{
  if(KeyFlags.PlayClick) {
    KeyFlags.PlayClick = 0;
    KeyFlags.PlayingClick = 1;
    SoundDelay = DelayBegin();
    SoundOn();
  } else if(KeyFlags.PlayingClick) {
    if(KeyFlags.WaitEndPlayingClick) {
      KeyFlags.WaitEndPlayingClick = 0;
      DelayEnd(SoundDelay, SYS_CONVERT_MCS(200000));
    }

    if(DelayIsEnded(SoundDelay, SYS_CONVERT_MCS(200000)) == TRUE) {
      KeyFlags.PlayingClick = 0;
      SoundOff();
    }
  }
}

//---------------------------------------------------------
void SoundPlayClick(void)
{
  KeyFlags.PlayClick = 1;
}

//---------------------------------------------------------
void SoundOn(void)
{
  SoundOn_pd();
}

//---------------------------------------------------------
void SoundOff(void)
{
  SoundOff_pd();
}

//---------------------------------------------------------
void SoundSetFastFrequencyBefore(void)
{
  SoundSetFastFrequencyBefore_pd();
}

//---------------------------------------------------------
void SoundSetNormalFrequencyBefore(void)
{
  SoundSetNormalFrequencyBefore_pd();
}
