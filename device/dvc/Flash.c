/*---------------------------------------------------------
  flash.c (AT24C1024B)
---------------------------------------------------------*/

#include "flash.h"
#include "flash_pd.h"
#include "delay.h"
#include "cfg.h"

NOINIT BOOL FlashChanged;
NOINIT DWORD FlashAddress;
static NOINIT BYTE FlashBuff[FLASH_PAGE_SIZE];

//---------------------------------------------------------
void FlashWrite(BYTE* buff, WORD n)
{
  FlashChanged = TRUE;
  FlashWrite_pd(buff, n);
}

//---------------------------------------------------------
void FlashRead(BYTE* buff, WORD n)
{
  FlashRead_pd(buff, n);
}

//---------------------------------------------------------
DWORD FlashGetSize(void)
{
  return FLASH_SIZE;
}

//---------------------------------------------------------
BOOL FlashReadBool(void)
{
  return TO_BOOL(FlashReadByte());
}

//---------------------------------------------------------
void FlashWriteBool(BOOL data)
{
  FlashWriteByte(data);
}

//---------------------------------------------------------
BYTE FlashReadByte(void)
{
  BYTE data;
  FlashRead(&data, 1);
  return data;
}

//---------------------------------------------------------
void FlashWriteByte(BYTE data)
{
  FlashWrite(&data, 1);
}

//---------------------------------------------------------
WORD FlashReadWord(void)
{
  WORD data;
  FlashRead((BYTE*)&data, 2);
  return data;
}

//---------------------------------------------------------
void FlashWriteWord(WORD data)
{
  FlashWrite((BYTE*)&data, 2);
}

//---------------------------------------------------------
DWORD FlashReadDword(void)
{
  DWORD data;
  FlashRead((BYTE*)&data, 4);
  return data;
}

//---------------------------------------------------------
void FlashWriteDword(DWORD data)
{
  FlashWrite((BYTE*)&data, 4);
}

//---------------------------------------------------------
FLOAT32 FlashReadFloat(void)
{
  FLOAT32 data;
  FlashRead((BYTE*)&data, 4);
  return data;
}

//---------------------------------------------------------
void FlashWriteFloat(FLOAT32 data)
{
  FlashWrite((BYTE*)&data, 4);
}

//---------------------------------------------------------
BOOL FlashCheckSlave(void)
{
  return FlashCheckSlave_pd();
}

//---------------------------------------------------------
void FlashFill(BYTE value, WORD n)
{
  WORD bytes;

  memset(FlashBuff, value, FLASH_PAGE_SIZE);

  bytes = FLASH_PAGE_SIZE - (FlashAddress & (FLASH_PAGE_SIZE - 1));
  if(bytes > n)
    bytes = n;

  FlashWrite(FlashBuff, bytes);

  while((n -= bytes)) {
    bytes = n >= FLASH_PAGE_SIZE ? FLASH_PAGE_SIZE : n;
    FlashWrite(FlashBuff, bytes);
  }
}

//---------------------------------------------------------
DWORD FlashFindNotEqual(BYTE value)
{
  DWORD index = 0;
  WORD i;

  do {
    FlashRead(FlashBuff, FLASH_PAGE_SIZE);

    for(i = 0; i < FLASH_PAGE_SIZE; i++)
      if(FlashBuff[i] != value) return index + i;

    index += FLASH_PAGE_SIZE;

  } while(TRUE);
}
