/*---------------------------------------------------------
  flash_pd.c
---------------------------------------------------------*/

#include "flash.h"
#include "flash_pd.h"

static BYTE FlashMem[FLASH_SIZE];

//---------------------------------------------------------
void FlashWrite_pd(BYTE* buff, WORD n)
{
  WORD i;
  for(i = 0; i < n; i++) {
    FlashAddress %= FLASH_SIZE;
    FlashMem[FlashAddress++] = buff[i];
  }
}

//---------------------------------------------------------
void FlashRead_pd(BYTE* buff, WORD n)
{
  WORD i;
  for(i = 0; i < n; i++) {
    FlashAddress %= FLASH_SIZE;
    buff[i] = FlashMem[FlashAddress++];
  }
}

//---------------------------------------------------------
BOOL FlashCheckSlave_pd(void)
{
  return TRUE;
}

//---------------------------------------------------------
void FlashSetByte_pd(DWORD index, BYTE b)
{
  if(index < FLASH_SIZE)
    FlashMem[index] = b;
}

//---------------------------------------------------------
BYTE FlashGetByte_pd(DWORD index)
{
  if(index < FLASH_SIZE)
    return FlashMem[index];
  return 0xFF;
}
