/*---------------------------------------------------------
  IncMemory.c
---------------------------------------------------------*/

#include "IncMemory.h"
#include "mem.h"
#include "memory.h"

//---------------------------------------------------------
void ShowIncMemoryIni(void)
{
  PrgVerb = VB_INC_MEMORY;
  
  MemoryTotal = MemGetTotalCells();
  MemoryUsed = MemGetUsedCells();
  
  ShowMemoryIni();
}

//---------------------------------------------------------
void ShowIncMemory(void)
{
  if(ShowMemory() == TRUE)
    PrgVerb = VB_INC_CLEARMEM_INI;
}
