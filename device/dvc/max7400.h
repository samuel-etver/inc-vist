#ifndef MAX7400_H_INCLUDED
#define MAX7400_H_INCLUDED

/*---------------------------------------------------------
  max7400.h
---------------------------------------------------------*/

#include "xmain.h"

void InitMax7400(void);

void Max7400On(void);
void Max7400Off(void);

void Max7400SetFrequency(FLOAT32 f);

#endif
