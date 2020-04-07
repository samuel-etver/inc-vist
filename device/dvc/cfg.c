/*---------------------------------------------------------
  cfg.c
---------------------------------------------------------*/

#include "cfg.h"
#include "flash.h"

//---------------------------------------------------------
void CfgRead(WORD address, BYTE* data, WORD n)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashRead(data, n);
}

//---------------------------------------------------------
void CfgWrite(WORD address, BYTE* data, WORD n)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashWrite(data, n);
}

//---------------------------------------------------------
BYTE CfgReadByte(WORD address)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  return FlashReadByte();
}

//---------------------------------------------------------
void CfgWriteByte(WORD address, BYTE data)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashWriteByte(data);
}

//---------------------------------------------------------
WORD CfgReadWord(WORD address)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  return FlashReadWord();
}

//---------------------------------------------------------
void CfgWriteWord(WORD address, WORD data)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashWriteWord(data);
}

//---------------------------------------------------------
DWORD CfgReadDword(WORD address)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  return FlashReadDword();
}

//---------------------------------------------------------
void CfgWriteDword(WORD address, DWORD data)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashWriteDword(data);
}

//---------------------------------------------------------
FLOAT32 CfgReadFloat(WORD address)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  return FlashReadFloat();
}

//---------------------------------------------------------
void CfgWriteFloat(WORD address, FLOAT32 data)
{
  FlashAddress = (DWORD)CFG_ADDRESS + address;
  FlashWriteFloat(data);
}

//---------------------------------------------------------
BOOL CfgReadFactor(WORD address, BYTE* factor, BYTE len, BYTE comma)
{
  BYTE buff[16];
  BYTE i;
  if(len > sizeof(buff))
    return FALSE;
  
  CfgRead(address, buff, len);
  
  if(buff[0] != '+' && buff[0] != '-')
    return FALSE;  
  for(i = 1; i < 2 + comma; i++) {
    if(buff[i] < '0') return FALSE;
    if(buff[i] > '9') return FALSE;
  }
  if(buff[i] != '+' && buff[i] != '-')
    return FALSE;
  for(++i; i < len; i++) {
    if(buff[i] < '0') return FALSE;
    if(buff[i] > '9') return FALSE;
  }
  
  memcpy(factor, buff, len);
  return TRUE;
}

//---------------------------------------------------------
void CfgWriteFactor(WORD address, BYTE* factor, BYTE len)
{
  CfgWrite(address, factor, len);
}

//---------------------------------------------------------
WORD CfgGetBaseDeviceModeAddress(BYTE device_mode)
{
  return CFG_FULL_SIZE;
}

//---------------------------------------------------------
WORD CfgGetEndAddress(void)
{
  return CfgGetBaseDeviceModeAddress((BYTE)-1);
}
