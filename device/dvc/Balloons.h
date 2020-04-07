#ifndef BALLOONS_H_INCLUDED
#define BALLOONS_H_INCLUDED

/*---------------------------------------------------------
  Balloons.h
---------------------------------------------------------*/

#include "xmain.h"

typedef struct {
  BYTE Height;
  BYTE SideWidth;
  const BYTE *LeftSideData;
  const BYTE *RightSideData;
  const BYTE *BodyData;
} TBalloon;

extern const TBalloon BalloonsMenu;
extern const TBalloon Balloons36;
extern const TBalloon Balloons36Simple;
extern const TBalloon Balloons36Dark;
extern const TBalloon Balloons36DarkSimple;
extern const TBalloon BalloonsSmallSpinDark;
extern const TBalloon BalloonsSmallSpinDarkSimple;
extern const TBalloon BalloonsSmallSpinLight;
extern const TBalloon BalloonsSmallSpinLightSimple;
extern const TBalloon BalloonsEdit;
extern const TBalloon BalloonsEditSimple;
extern const TBalloon BalloonsEdit24;
extern const TBalloon BalloonsEdit24Simple;
extern const TBalloon BalloonsGauge;

#endif
