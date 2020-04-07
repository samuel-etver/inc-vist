/*---------------------------------------------------------
  utils.c
---------------------------------------------------------*/

#include "utils.h"
#include <math.h>

//---------------------------------------------------------
BOOL TriggerSetValueFloat(TTrigger* trigger, FLOAT32 new_value)
{
  if(trigger->ValueFloat != new_value) {
    trigger->ValueFloat = new_value;
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
BOOL TriggerSetValueBool(TTrigger* trigger, BOOL new_value)
{
  if(trigger->ValueBool != new_value) {
    trigger->ValueBool = new_value;
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
BOOL TriggerSetValueWord(TTrigger* trigger, WORD new_value)
{
  if(trigger->ValueWord != new_value) {
    trigger->ValueWord = new_value;
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
BYTE Strip(BYTE* buff, BYTE n, BYTE symbol)
{
  WORD lo;
  WORD hi;

  WORD i;

  for(lo = 0; lo < n; lo++)
    if(buff[lo] != symbol) break;

  for(hi = n; hi > lo; hi--)
    if(buff[hi-1] != symbol) break;

  for(i = lo; i < hi; i++)
    buff[i-lo] = buff[i];

  return hi-lo;
}

//---------------------------------------------------------
BYTE WordToStr(BYTE* buff, WORD w)
{
  WordToStrWithLeadingSpaces(buff, w, 5);
  return StripDecimal(buff, 5);
}

//---------------------------------------------------------
void WordToStrWithLeadingSymbol(BYTE* buff, WORD w, WORD n, BYTE symbol)
{
  WORD i;
  div_t div_res;

  div_res.quot = (int)(DWORD)w;

  memset(buff, symbol, n);

  for(i = 0; i < n; i++) {
    div_res = div(div_res.quot, 10);
    buff[(n - 1) - i] = '0' + (BYTE)div_res.rem;
    if(!div_res.quot)
      return;
  }

  if(div_res.quot)
    memset(buff, '*', n);
}

//---------------------------------------------------------
void WordToStrWithLeadingSpaces(BYTE* buff, WORD w, WORD n)
{
  WordToStrWithLeadingSymbol(buff, w, n, ' ');
}

//---------------------------------------------------------
void WordToStrWithLeadingZeroes(BYTE* buff, WORD w, WORD n)
{
  WordToStrWithLeadingSymbol(buff, w, n, '0');
}

//---------------------------------------------------------
void DwordToStrWithLeadingSymbol(BYTE* buff, DWORD dw, WORD n, BYTE symbol)
{
  WORD i;
  div_t div_res;

  div_res.quot = (int)dw;

  memset(buff, symbol, n);

  for(i = 0; i < n; i++) {
    div_res = div(div_res.quot, 10);
    buff[(n - 1) - i] = '0' + (BYTE)div_res.rem;
    if(!div_res.quot)
      return;
  }

  if(div_res.quot)
    memset(buff, '*', n);
}

//---------------------------------------------------------
void DwordToStrWithLeadingSpaces(BYTE* buff, DWORD dw, WORD n)
{
  DwordToStrWithLeadingSymbol(buff, dw, n, ' ');
}

//---------------------------------------------------------
void DwordToStrWithLeadingZeroes(BYTE* buff, DWORD dw, WORD n)
{
  DwordToStrWithLeadingSymbol(buff, dw, n, '0');
}

//---------------------------------------------------------
void WordToStrAsFloatWithLeadingSymbol(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits, BYTE symbol)
{
  WORD i;
  div_t div_res;

  div_res.quot = (int)(DWORD)w;

  for(i = 0; i < n; i++) {
    if(i == fraq_digits) {
      buff[n - i - 1] = '.';
      continue;
    }

    div_res = div(div_res.quot, 10);
    buff[n - i - 1] = '0' + (BYTE)div_res.rem;
  }

  if(div_res.quot) {
    for(i = 0; i < n; i++)
      if(buff[i] != '.') buff[i] = '*';
    return;
  }

  if(n > fraq_digits + 2)
    for(i = 0; i < n - fraq_digits - 2; i++) {
      if(buff[i] != '0')
        break;
      buff[i] = symbol;
    }
}

//---------------------------------------------------------
void WordToStrAsFloatWithLeadingSpaces(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits)
{
  WordToStrAsFloatWithLeadingSymbol(buff, w, n, fraq_digits, ' ');
}

//---------------------------------------------------------
void WordToStrAsFloatWithLeadingZeroes(BYTE* buff, WORD w, WORD n,
  BYTE fraq_digits)
{
  WordToStrAsFloatWithLeadingSymbol(buff, w, n, fraq_digits, '0');
}

//---------------------------------------------------------
void DwordToStrAsFloatWithLeadingSymbol(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits, BYTE symbol)
{
  WORD i;

  for(i = 0; i < n; i++) {
    if(i == fraq_digits) {
      buff[n - i - 1] = '.';
      continue;
    }

    buff[n - i - 1] = '0' + dw%10; dw /= 10;
  }

  if(dw > 0) {
    for(i = 0; i < n; i++)
      if(buff[i] != '.')
        buff[i] = '*';
    return;
  }

  if(n > fraq_digits + 2)
    for(i = 0; i < n - fraq_digits - 2; i++) {
      if(buff[i] != '0')
        break;
      buff[i] = symbol;
    }
}

//---------------------------------------------------------
void DwordToStrAsFloatWithLeadingSpaces(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits)
{
  DwordToStrAsFloatWithLeadingSymbol(buff, dw, n, fraq_digits, ' ');
}

//---------------------------------------------------------
void DwordToStrAsFloatWithLeadingZeroes(BYTE* buff, DWORD dw, WORD n,
  BYTE fraq_digits)
{
  DwordToStrAsFloatWithLeadingSymbol(buff, dw, n, fraq_digits, '0');
}

//---------------------------------------------------------
BYTE FloatToStr(BYTE* buff, FLOAT32 f, BYTE comma)
{
  BYTE format[8];
  BYTE n;

  if(comma < 9) {
    memcpy(format, "%.0f", 5);
    format[2] += comma;
    n = (BYTE)sprintf((char*)buff, (const char*)format, f);
  }
  else {
    memcpy(buff, "?", n = 1);
  }
  return n;
}

static const DWORD DeltaValue[] = {
  1, 10, 100, 1000, 10000, 100000
};

//---------------------------------------------------------
void ByteValueUp(BYTE* value, BYTE max_value, BYTE pos)
{
  WORD v = *value;

  if(pos < 3)
    v += DeltaValue[pos];

  if(v > max_value)
    v = max_value;

  *value = (BYTE)v;
}

//---------------------------------------------------------
void ByteValueDown(BYTE* value, BYTE min_value, BYTE pos)
{
  INT16 v = (INT16)(WORD)*value;

  if(pos < 3)
    v -= DeltaValue[pos];

  if(v < (INT16)(WORD)min_value)
    v = (INT16)(WORD)min_value;

  *value = (BYTE)v;
}

//---------------------------------------------------------
void WordValueUp(WORD* value, WORD max_value, BYTE pos)
{
  DWORD v = *value;

  if(pos < 5)
    v += DeltaValue[pos];

  if(v > max_value)
    v = max_value;

  *value = (WORD)v;
}

//---------------------------------------------------------
void WordValueDown(WORD* value, WORD min_value, BYTE pos)
{
  INT32 v = (INT32)(DWORD)*value;

  if(pos < 5)
    v -= DeltaValue[pos];

  if(v < (INT32)(DWORD)min_value)
    v = (INT32)(DWORD)min_value;

  *value = (WORD)v;
}

//---------------------------------------------------------
void DwordValueUp(DWORD* value, DWORD max_value, BYTE pos)
{
  DWORD v = *value;

  if(pos < 6)
    v += DeltaValue[pos];

  if(v > max_value)
    v = max_value;

  *value = (DWORD)v;
}

//---------------------------------------------------------
void DwordValueDown(DWORD* value, DWORD min_value, BYTE pos)
{
  INT32 v = (INT32)(DWORD)*value;

  if(pos < 6)
    v -= DeltaValue[pos];

  if(v < (INT32)(DWORD)min_value)
    v = (INT32)(DWORD)min_value;

  *value = (DWORD)v;
}

//---------------------------------------------------------
WORD BuildValuePattern(BYTE pos, BYTE n, BOOL ignore_inverted)
{
  WORD pattern = 0;

  if(ignore_inverted == FALSE) {
    WORD mask = 0x0001 << pos;
    WORD i;

    for(i = pos; i < n; i++) {
      pattern |= mask;
      mask <<= 1;
    }
  }

  return pattern;
}

//---------------------------------------------------------
WORD BuildValueWithCommaPattern(WORD pos, BYTE n, BYTE comma,
  BOOL ignore_inverted)
{
  return BuildValuePattern(pos + (pos >= comma ? 1 : 0), n, ignore_inverted);
}

//---------------------------------------------------------
BYTE BcdToDec(BYTE b)
{
  return 10*(b>>4) + (b & 0x0F);
}

//---------------------------------------------------------
BYTE DecToBcd(BYTE b)
{
  return ((b/10)<<4) | (b%10);
}

//---------------------------------------------------------
BYTE StripDecimal(BYTE* buff, BYTE n)
{
  WORD lo;
  WORD i;

  for(lo=0; lo<n; lo++) {
    if(buff[lo] != ' ') break;
  }

  for(; n>lo; n--) {
    if(buff[n - 1] != ' ') break;
  }

  for(i=lo; i<n; i++)
    buff[i-lo] = buff[i];

  return n-lo;
}

//---------------------------------------------------------
BOOL ToBool(WORD value)
{
  if(value)
    return TRUE;
  return FALSE;
}

//---------------------------------------------------------
BYTE GetFirstZeroBit(DWORD data)
{
  DWORD mask;
  BYTE i = 0;

  for(mask = 0x01; data & mask; mask <<= 1) ++i;

  return i;
}

//---------------------------------------------------------
void Sort(void* data, WORD n, WORD item_size, INT16 (*cmp)(void* v0, void* v1))
{
  WORD i;
  WORD j;
  BYTE* ptr_i = data;
  BYTE* ptr_j;
  BYTE buff[8];

  if(item_size > 8 || item_size == 0) return;

  for(i = 0; i < n - 1; i++) {
    ptr_j = ptr_i + item_size;
    for(j = i + 1; j < n; j++) {
      if((*cmp)(ptr_i, ptr_j) > 0) {
        memcpy(buff, ptr_i, item_size);
        memcpy(ptr_i, ptr_j, item_size);
        memcpy(ptr_j, buff, item_size);
      }
      ptr_j += item_size;
    }
    ptr_i += item_size;
  }
}

//---------------------------------------------------------
static INT16 CmpU8(void* v0, void* v1)
{
  if(*(BYTE*)v0 < *(BYTE*)v1) return -1;
  if(*(BYTE*)v0 > *(BYTE*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortU8(BYTE* data, WORD n)
{
  Sort(data, n, 1, CmpU8);
}

//---------------------------------------------------------
static INT16 CmpU16(void* v0, void* v1)
{
  if(*(WORD*)v0 < *(WORD*)v1) return -1;
  if(*(WORD*)v0 > *(WORD*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortU16(WORD* data, WORD n)
{
  Sort(data, n, 2, CmpU16);
}

//---------------------------------------------------------
static INT16 CmpU32(void* v0, void* v1)
{
  if(*(DWORD*)v0 < *(DWORD*)v1) return -1;
  if(*(DWORD*)v0 > *(DWORD*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortU32(DWORD* data, WORD n)
{
  Sort(data, n, 4, CmpU32);
}

//---------------------------------------------------------
static INT16 CmpU64(void* v0, void* v1)
{
  if(*(UINT64*)v0 < *(UINT64*)v1) return -1;
  if(*(UINT64*)v0 > *(UINT64*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortU64(UINT64* data, WORD n)
{
  Sort(data, n, 8, CmpU64);
}

//---------------------------------------------------------
static INT16 CmpI8(void* v0, void* v1)
{
  if(*(INT8*)v0 < *(INT8*)v1) return -1;
  if(*(INT8*)v0 > *(INT8*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortI8(INT8* data, WORD n)
{
  Sort(data, n, 1, CmpI8);
}

//---------------------------------------------------------
static INT16 CmpI16(void* v0, void* v1)
{
  if(*(INT16*)v0 < *(INT16*)v1) return -1;
  if(*(INT16*)v0 > *(INT16*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortI16(INT16* data, WORD n)
{
  Sort(data, n, 2, CmpI16);
}

//---------------------------------------------------------
static INT16 CmpI32(void* v0, void* v1)
{
  if(*(INT32*)v0 < *(INT32*)v1) return -1;
  if(*(INT32*)v0 > *(INT32*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortI32(INT32* data, WORD n)
{
  Sort(data, n, 4, CmpI32);
}

//---------------------------------------------------------
static INT16 CmpI64(void* v0, void* v1)
{
  if(*(INT64*)v0 < *(INT64*)v1) return -1;
  if(*(INT64*)v0 > *(INT64*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortI64(INT64* data, WORD n)
{
  Sort(data, n, 8, CmpI64);
}

//---------------------------------------------------------
static INT16 CmpF32(void* v0, void* v1)
{
  if(*(FLOAT32*)v0 < *(FLOAT32*)v1) return -1;
  if(*(FLOAT32*)v0 > *(FLOAT32*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortF32(FLOAT32* data, WORD n)
{
  Sort(data, n, 4, CmpF32);
}

//---------------------------------------------------------
static INT16 CmpF64(void* v0, void* v1)
{
  if(*(FLOAT64*)v0 < *(FLOAT64*)v1) return -1;
  if(*(FLOAT64*)v0 > *(FLOAT64*)v1) return 1;
  return 0;
}

//---------------------------------------------------------
void SortF64(FLOAT64* data, WORD n)
{
  Sort(data, n, 8, CmpF64);
}

//---------------------------------------------------------
BYTE UtoStrU(BYTE* buff, FLOAT32 u)
{
  if(u > 9.999f) {
    memcpy(buff, "*.***", 5);
  }
  else {
    if(u < 0.0f)
      u = 0.0f;
    WordToStrAsFloatWithLeadingSpaces(buff, (WORD)(1000.0f*u), 5, 3);
  }
  return 5;
}

//---------------------------------------------------------
BYTE UtoStrI(BYTE* buff, FLOAT32 u)
{
  if(u > 9.999f) {
    memcpy(buff, " *.***", 6);
  }
  else if(u < -9.999f) {
    memcpy(buff, "-*.***", 6);
  }
  else {
    sprintf((char*)buff, "%6.3f", u);
  }
  return 6;
}


