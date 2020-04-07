/*---------------------------------------------------------
  VistMemory.c
---------------------------------------------------------*/

#include "VistMemory.h"
#include "memory.h"
#include "mem.h"

//---------------------------------------------------------
void ShowVistMemoryIni(void)
{
  PrgVerb = VB_VIST_MEMORY;
  
  MemoryTotal = MemGetTotalCells();
  MemoryUsed = MemGetUsedCells();
  
  ShowMemoryIni();
}

//---------------------------------------------------------
void ShowVistMemory(void)
{
  if(ShowMemory() == TRUE)
    PrgVerb = VB_VIST_CLEARMEM_INI;
}
