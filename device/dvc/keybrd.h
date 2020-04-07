#ifndef KEYBRD_H_INCLUDED
#define KEYBRD_H_INCLUDED

/*---------------------------------------------------------
  keybrd.h
---------------------------------------------------------*/

#include "xmain.h"

typedef struct {
  BIT Dbl: 1;
  BIT Wait: 1;
  BIT PlayClick: 1;
  BIT PlayingClick: 1;
  BIT WaitEndPlayingClick: 1;
  BIT Stop: 1;
} TKeyFlags;

extern NOINIT WORD KeyUp;
extern NOINIT WORD KeyDown;
extern NOINIT WORD KeyHold;
extern NOINIT WORD KeyRepeat;
extern NOINIT TKeyFlags KeyFlags;

#define KB_NOKEY        0x0000
#define KB_F1           0x0004
#define KB_F2           0x0002
#define KB_F3           0x0001
#define KB_MEASURE      0x0040
#define KB_UP           0x0020
#define KB_PLUS         0x0010
#define KB_LEFT         0x0400
#define KB_MENU         0x0200
#define KB_RIGHT        0x0100
#define KB_POWER        0x8000
#define KB_DOWN         0x2000
#define KB_MINUS        0x1000

void InitKeybrd(void);
void DoKeybrd(void);
void SetupKeybrd(void);

#endif
