#ifndef FLASH_H_INCLUDED
#define FLASH_H_INCLUDED

/*---------------------------------------------------------
  flash.h
---------------------------------------------------------*/

#include "xmain.h"
#include "iic.h"

#define FLASH_SIZE                0x20000
#define FLASH_PAGE_SIZE           0x80U

void FlashWrite(BYTE* buff, WORD n);
void FlashRead(BYTE* buff, WORD n);

DWORD FlashGetSize(void);

BOOL FlashReadBool(void);
void FlashWriteBool(BOOL data);
BYTE FlashReadByte(void);
void FlashWriteByte(BYTE data);
WORD FlashReadWord(void);
void FlashWriteWord(WORD data);
DWORD FlashReadDword(void);
void FlashWriteDword(DWORD data);
FLOAT32 FlashReadFloat(void);
void FlashWriteFloat(FLOAT32 data);
BOOL FlashCheckSlave(void);
void FlashFill(BYTE value, WORD n);
DWORD FlashFindNotEqual(BYTE value);

extern NOINIT BOOL FlashChanged;
extern NOINIT DWORD FlashAddress;

#endif
