#ifndef PICTURES_H_INCLUDED
#define PICTURES_H_INCLUDED

/*---------------------------------------------------------
  pictures.h
--------------------------------------------------------*/

#include "xmain.h"

//#define PICTURE8
#ifndef PICTURE8
#  define PICTURE16
#endif

typedef BYTE TPicture8;
typedef WORD TPicture16;

#ifdef PICTURE8
typedef TPicture8 TPicture;
#else
typedef TPicture16 TPicture;
#endif

extern const TPicture PicturesMaterial[];
extern const TPicture PicturesUsb[];
extern const TPicture PicturesPower[];
extern const TPicture PicturesLowPower[];
extern const TPicture PicturesChargeLight[];
extern const TPicture PicturesChargeDark[];
extern const TPicture PicturesSpinUp[];
extern const TPicture PicturesSpinDown[];
extern const TPicture PicturesSmallSpinUp[];
extern const TPicture PicturesSmallSpinDown[];
extern const TPicture PicturesRectangle[];
extern const TPicture PicturesCircle[];
extern const TPicture PicturesChecked[];
extern const TPicture PicturesUnChecked[];
extern const TPicture PicturesArrowRight[];
extern const TPicture PicturesArrowLeft[];
extern const TPicture PicturesArrowTop[];
extern const TPicture PicturesMenuDown[];
extern const TPicture PicturesMenuUp[];
extern const TPicture PicturesChart_f[];
extern const TPicture PicturesChart_3k[];
extern const TPicture PicturesChart_30k[];
extern const TPicture PicturesChart_t[];
extern const TPicture PicturesChart_90[];

#endif
