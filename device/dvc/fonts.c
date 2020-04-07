/*---------------------------------------------------------
  Fonts.h
---------------------------------------------------------*/

#include "Fonts.h"

#include "Font08.dat"
#include "Font10.dat"
#include "Font11.dat"
#include "Font12.dat"
#include "Font13.dat"
#include "Font16.dat"
#include "Font20.dat"

//---------------------------------------------------------
WORD FontsGetHeight(BYTE font)
{
  switch(font) {
    case FONT08: return Font08Height;
    case FONT10: return Font10Height;
    case FONT11: return Font11Height;
    case FONT13: return Font13Height;
    case FONT16: return Font16Height;
    case FONT20: return Font20Height;
  }
  return Font12Height;
}

//---------------------------------------------------------
WORD FontsGetAscent(BYTE font)
{
  switch(font) {
    case FONT08: return Font08Ascent;
    case FONT10: return Font10Ascent;
    case FONT11: return Font11Ascent;
    case FONT13: return Font13Ascent;
    case FONT16: return Font16Ascent;
    case FONT20: return Font20Ascent;
  }
  return Font12Ascent;
}

//---------------------------------------------------------
WORD FontsGetDescent(BYTE font)
{
  switch(font) {
    case FONT08: return Font08Descent;
    case FONT10: return Font10Descent;
    case FONT11: return Font11Descent;
    case FONT13: return Font13Descent;
    case FONT16: return Font16Descent;
    case FONT20: return Font20Descent;
  }
  return Font12Descent;
}

//---------------------------------------------------------
const BYTE* FontsGetData(BYTE font)
{
  switch(font) {
    case FONT08: return Font08Data;
    case FONT10: return Font10Data;
    case FONT11: return Font11Data;
    case FONT13: return Font13Data;
    case FONT16: return Font16Data;
    case FONT20: return Font20Data;
  }
  return Font12Data;
}

//---------------------------------------------------------
const WORD* FontsGetCharIndexes(BYTE font)
{
  switch(font) {
    case FONT08: return Font08CharIndexes;
    case FONT10: return Font10CharIndexes;
    case FONT11: return Font11CharIndexes;
    case FONT13: return Font13CharIndexes;
    case FONT16: return Font16CharIndexes;
    case FONT20: return Font20CharIndexes;
  }
  return Font12CharIndexes;
}
