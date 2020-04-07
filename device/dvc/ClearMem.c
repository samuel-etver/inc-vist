/*---------------------------------------------------------
  ClearMem.c
---------------------------------------------------------*/

#include "ClearMem.h"
#include "ask.h"
#include "mem.h"

//---------------------------------------------------------
void ShowClearMemIni(void)
{
  TempWord = 170;
  TempByte = 1;

  ShowAskIni();
}

//---------------------------------------------------------
void ShowClearMem(void)
{
  ShowAsk();
  
  if(TempBool == TRUE)
    MemClear();
}
