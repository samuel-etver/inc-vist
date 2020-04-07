/*---------------------------------------------------------
  Factor.c
---------------------------------------------------------*/

#include "Factor.h"
#include "cfg.h"
#include "lcd.h"
#include "utils.h"
#include <math.h>

//---------------------------------------------------------
FLOAT32 FactorToFloat(BYTE* factor, BYTE len, BYTE comma)
{
  FLOAT32 sign = factor[0] == '-' ? -1.0f : 1.0f;
  INT16 exp_sign;
  BYTE i;
  WORD v = 0;
  WORD exp = 0;

  for(i = 1; i < 2 + comma; i++) {
    v = (factor[i] - (BYTE)'0') + v*10U;
  }

  exp_sign = factor[i] == '-' ? -1 : 1;

  for(++i; i < len; i++) {
    exp = (factor[i] - (BYTE)'0') + exp*10U;
  }
  return sign*v*(FLOAT32)pow(10, exp_sign*(INT16)exp - (INT16)comma);
}

//---------------------------------------------------------
void FactorFromFloat(BYTE* factor, BYTE len, BYTE comma, FLOAT32 value)
{
  BYTE buff[32];
  BYTE format[16];
  INT16 exp_index0;
  INT16 exp_index1;
  INT16 exp_len;
  INT16 factor_exp_len = (INT16)len - (INT16)comma - 3;
  WORD index;
  WORD i;
  BOOL found;
  memset(factor, '?', len);
  if(comma + 4 > len)
    return;
  sprintf((char*)format, "%%+.%iE", (int)comma);
  i = sprintf((char*)buff, (char*)format, (double)value);

  exp_index1 = i - 1;
  for(exp_index0 = exp_index1; exp_index0 >= 0; exp_index0--) {
    if(buff[exp_index0] == 'E')
      break;
  }
  if(exp_index0 < 0)
    return;
  exp_index0++;

  memcpy(factor, buff, 2);
  memcpy(factor + 2, buff + 3, comma);
  index = 2 + comma;
  factor[index++] = buff[exp_index0++];
  
  exp_len = exp_index1 - exp_index0 + 1;
  if(factor_exp_len >= exp_len) {
    memset(factor + index, '0', factor_exp_len);
    memcpy(factor + index + factor_exp_len - exp_len, buff + exp_index0,
     exp_len);      
  }
  else {
    found = FALSE;
    for(i = 0; i < exp_len - factor_exp_len; i++) {
      if(buff[i + exp_index0] != '0') {
        found = TRUE; break;
      }  
    }
    if(found == TRUE) {
      memset(factor + index, '9', factor_exp_len);
    }
    else {
      memcpy(factor + index, buff + exp_index0 + exp_len - factor_exp_len,
       factor_exp_len);      
    }    
  }
}

//---------------------------------------------------------
BYTE FactorToStr(BYTE* buff, BYTE* factor, BYTE len, BYTE comma)
{
  BYTE comma2 = comma + 2U;
  if(len < comma2)
    return 0;
  memcpy(buff, factor, 2U);
  buff[2U] = '.';
  memcpy(buff + 3U, factor + 2U, comma);
  buff[3U + comma] = 'e';
  memcpy(buff + 4U + comma, factor + comma2, len - comma2);
  return len + 2U;
}
