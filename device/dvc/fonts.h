#ifndef FONTS_H_INCLUDED
#define FONST_H_INCLUDED

/*---------------------------------------------------------
 fonts.h
---------------------------------------------------------*/

#include "xmain.h"

#define FONT08        0
#define FONT10        1
#define FONT11        2
#define FONT12        3
#define FONT13        4
#define FONT16        5
#define FONT20        6

WORD FontsGetHeight(BYTE font);
WORD FontsGetAscent(BYTE font);
WORD FontsGetDescent(BYTE font);
const BYTE* FontsGetData(BYTE font);
const WORD* FontsGetCharIndexes(BYTE font);

#endif
