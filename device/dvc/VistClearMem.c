/*---------------------------------------------------------
  VistClearMem.c
---------------------------------------------------------*/

#include "VistClearMem.h"
#include "ClearMem.h"

//---------------------------------------------------------
void ShowVistClearMemIni(void)
{
  PrgVerb = VB_VIST_CLEARMEM;
  ShowClearMemIni();
}

//---------------------------------------------------------
void ShowVistClearMem(void)
{
  ShowClearMem();
}

