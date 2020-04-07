#ifndef UTILS_H_INCLUDED
#define UTILS_H_INCLUDED

/*---------------------------------------------------------
  utils.h
---------------------------------------------------------*/

#include "xmain.h"

typedef struct {
  FLOAT32 ValueFloat;
  BOOL    ValueBool;
  WORD    ValueWord;
} TTrigger;

BOOL TriggerSetValueFloat(TTrigger* trigger, FLOAT32 new_value);
BOOL TriggerSetValueBool(TTrigger* trigger, BOOL new_value);
BOOL TriggerSetValueWord(TTrigger* trigger, WORD new_value);

BYTE Strip(BYTE* buff, BYTE n, BYTE symbol);

BYTE WordToStr(BYTE* buff, WORD w);
void WordToStrWithLeadingSymbol(BYTE* buff, WORD w, WORD n, BYTE symbol);
void WordToStrWithLeadingSpaces(BYTE* buff, WORD w, WORD n);
void WordToStrWithLeadingZeroes(BYTE* buff, WORD w, WORD n);
void DwordToStrWithLeadingSymbol(BYTE* buff, DWORD dw, WORD n, BYTE symbol);
void DwordToStrWithLeadingSpaces(BYTE* buff, DWORD dw, WORD n);
void DwordToStrWithLeadingZeroes(BYTE* buff, DWORD dw, WORD n);
BYTE FloatToStr(BYTE* buff, FLOAT32 f, BYTE comma);

void WordToStrAsFloatWithLeadingSymbol(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits, BYTE symbol);
void WordToStrAsFloatWithLeadingSpaces(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits);
void WordToStrAsFloatWithLeadingZeroes(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits);

void DwordToStrAsFloatWithLeadingSymbol(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits, BYTE symbol);
void DwordToStrAsFloatWithLeadingSpaces(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits);
void DwordToStrAsFloatWithLeadingZeroes(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits);

void ByteValueUp(BYTE* value, BYTE max_value, BYTE pos);
void ByteValueDown(BYTE* value, BYTE min_value, BYTE pos);
void WordValueUp(WORD* value, WORD max_value, BYTE pos);
void WordValueDown(WORD* value, WORD min_value, BYTE pos);
void DwordValueUp(DWORD* value, DWORD max_value, BYTE pos);
void DwordValueDown(DWORD* value, DWORD min_value, BYTE pos);

WORD BuildValuePattern(BYTE pos, BYTE n, BOOL ignore_inverted);
WORD BuildValueWithCommaPattern(WORD pos, BYTE n, BYTE comma,
  BOOL ignore_inverted);

BYTE BcdToDec(BYTE b);
BYTE DecToBcd(BYTE b);

BYTE StripDecimal(BYTE* buff, BYTE n);

BOOL ToBool(WORD value);

BYTE GetFirstZeroBit(DWORD data);

void Sort(void* data, WORD n, WORD item_size, INT16 (*cmp)(void* v0, void* v1));
void SortU8(BYTE* data, WORD n);
void SortU16(WORD* data, WORD n);
void SortU32(DWORD* data, WORD n);
void SortU64(UINT64* data, WORD n);
void SortI8(INT8* data, WORD n);
void SortI16(INT16* data, WORD n);
void SortI32(INT32* data, WORD n);
void SortI64(INT64* data, WORD n);
void SortF32(FLOAT32* data, WORD n);
void SortF64(FLOAT64* data, WORD n);

BYTE UtoStrU(BYTE* buff, FLOAT32 u);
BYTE UtoStrI(BYTE* buff, FLOAT32 u);

#endif
