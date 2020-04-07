#ifndef FLASH_PD_H_INCLUDED
#define FLASH_PD_H_INCLUDED

/*---------------------------------------------------
  flash_pd.h
---------------------------------------------------*/

#include "xmain.h"

void FlashWrite_pd(BYTE* buff, WORD n);
void FlashRead_pd(BYTE* buff, WORD n);
void FlashSetByte_pd(DWORD index, BYTE b);
BYTE FlashGetByte_pd(DWORD index);
BOOL FlashCheckSlave_pd(void);

#endif
