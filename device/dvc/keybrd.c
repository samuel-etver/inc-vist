/*---------------------------------------------------------
  keybrd.c
---------------------------------------------------------*/

#include "keybrd.h"
#include "keybrd_pd.h"
#include "delay.h"
#include "usb.h"
#include  "system.h"
#include "KeybrdSound.h"

NOINIT TKeyFlags KeyFlags;
NOINIT WORD KeyUp;
NOINIT WORD KeyDown;
NOINIT WORD KeyHold;
NOINIT WORD KeyRepeat;
static NOINIT BYTE KeyClk;
NOINIT DWORD KeyScanTicks;
NOINIT WORD KeyScan;

static WORD GetKey(void);

//---------------------------------------------------------
void InitKeybrd(void)
{
  InitKeybrd_pd();
  KeyUp = KB_NOKEY;
  KeyDown = KB_NOKEY;
  KeyHold = KB_NOKEY;
  KeyScan = KB_NOKEY;
  KeyClk = 0;
  KeyRepeat = 0;
  KeyFlags.Dbl = 0;
  KeyFlags.Wait = 0;
  KeyFlags.PlayClick = 0;
  KeyFlags.PlayingClick = 0;
  KeyFlags.WaitEndPlayingClick = 0;
  KeyFlags.Stop = 0;
}

//---------------------------------------------------------
void SetupKeybrd(void)
{
  KeyScanTicks = GetTicks();
}

//---------------------------------------------------------
void DoKeybrd(void)
{
  KeyUp = KB_NOKEY;

  KeyDown = GetKey();

  if(KeyDown == KB_NOKEY) {
    if(!KeyFlags.Dbl)
      KeyUp = KeyHold;
    KeyHold = 0;
    KeyFlags.Dbl = 0;
    KeyFlags.Stop = 0;
    KeyFlags.Wait = 0;
    KeyRepeat = 0;
    return;
  }

  if(KeyDown == KeyHold) {
    KeyDown = KB_NOKEY;

    if(!KeyFlags.Stop)
      if(PrgFlags.CentiSec)
        if(++KeyClk >= (KeyFlags.Wait ? 5 : 2)) {
          KeyClk = 0;
          KeyFlags.Wait = 0;
          KeyDown = KeyHold;
          KeyRepeat++;
        }
    return;
  }

  {
    BYTE j;
    BYTE bits = 0;
    for(j = 0; j < 8*sizeof(KeyDown); j++)
      if(KeyDown & (1 << j)) bits++;

    KeyClk = 0;

    if(bits > 1 || !KeyFlags.Dbl) {
      KeyFlags.Wait = 1;
      if(PrgVerb > VB_LOADCFG) {
        if(KeybrdSoundOn == TRUE)
          KeyFlags.PlayClick = 1;
      }
      KeyFlags.Stop = 0;
      KeyRepeat = 1;
    }

    if(bits == 1 && KeyFlags.Dbl) {
      KeyUp = KeyHold;
      KeyFlags.Stop = 1;
    }

    if(bits > 1)
      KeyFlags.Dbl = 1;

    KeyHold = KeyDown;
  }
}

//---------------------------------------------------------
static WORD GetKey(void)
{
  WORD key = GetKey_pd();

  if(key != KeyScan) {
    KeyScan = key;
    KeyScanTicks = GetTicks();
  }
  else if((DWORD)(GetTicks() - KeyScanTicks) > SYS_CONVERT_MCS(5000))
    return key;

  return KeyHold;
}
