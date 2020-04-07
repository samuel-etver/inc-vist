#ifndef MAX7400_PD_H_INCLUDED
#define MAX7400_PD_H_INCLUDED

/*---------------------------------------------------------
  max7400_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void InitMax7400_pd(void);
void Max7400On_pd(void);
void Max7400Off_pd(void);
void Max7400SetFrequency_pd(FLOAT32 f);

#endif
