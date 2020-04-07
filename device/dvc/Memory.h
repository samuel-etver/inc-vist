#ifndef MEMORY_H_INCLUDED
#define MEMORY_H_INCLUDED

/*---------------------------------------------------------
  memory.h
---------------------------------------------------------*/

#include "xmain.h"

void ShowMemoryIni(void);
BOOL ShowMemory(void);  

extern NOINIT WORD MemoryTotal;
extern NOINIT WORD MemoryUsed;

#endif
