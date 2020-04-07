/*---------------------------------------------------------
  IncClearMem.c
---------------------------------------------------------*/

#include "IncClearMem.h"
#include "ClearMem.h"

//---------------------------------------------------------
void ShowIncClearMemIni(void)
{
  PrgVerb = VB_INC_CLEARMEM;
  ShowClearMemIni();
}

//---------------------------------------------------------
void ShowIncClearMem(void)
{
  ShowClearMem();
}

