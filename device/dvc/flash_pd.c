/*---------------------------------------------------------
  flash_pd.c
---------------------------------------------------------*/

#include "flash_pd.h"
#include "flash.h"
#include "system.h"
#include "delay.h"

#define FLASH_SLAVE_ADDRESS1			0xA0
#define FLASH_SLAVE_ADDRESS2			0xA2

//---------------------------------------------------------
void FlashWrite_pd(BYTE* buff, WORD n)
{
  IicStartWrite((FlashAddress & 0x10000) ? FLASH_SLAVE_ADDRESS2 :
    FLASH_SLAVE_ADDRESS1, FlashAddress & 0xFFFF, 2);

  while(n--) {
    IicWriteByte(*buff++);

    if((++FlashAddress & (FLASH_PAGE_SIZE - 1U)) == 0)
      if(n) {
        IicStopWrite();
        Delay(SYS_CONVERT_MCS(10000));
        IicStartWrite((FlashAddress & 0x10000) ? FLASH_SLAVE_ADDRESS2 :
          FLASH_SLAVE_ADDRESS1, FlashAddress & 0xFFFF, 2);
      }
  }

  IicStopWrite();
  Delay(SYS_CONVERT_MCS(10000));
}

//---------------------------------------------------------
void FlashRead_pd(BYTE* buff, WORD n)
{
  IicStartRead((FlashAddress & 0x10000) ? FLASH_SLAVE_ADDRESS2 :
    FLASH_SLAVE_ADDRESS1, FlashAddress & 0xFFFF, 2);

  while(0 < n--) {
    *buff++ = IicReadByte();

    if(++FlashAddress == 0x10000)
      if(n > 0) {
        IicStopRead();
        IicStartRead(FLASH_SLAVE_ADDRESS2, FlashAddress & 0xFFFF, 2);
      }
  }

  IicStopRead();
}

//---------------------------------------------------------
BOOL FlashCheckSlave_pd(void)
{
  if(IicCheckSlave(FLASH_SLAVE_ADDRESS1) == FALSE)
    return FALSE;
  return IicCheckSlave(FLASH_SLAVE_ADDRESS2);
}
