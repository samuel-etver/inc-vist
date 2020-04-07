#ifndef ADC_PD_H_INCLUDED
#define ADC_PD_H_INCLUDED

/*---------------------------------------------------------
  adc_pd.h
---------------------------------------------------------*/

#include "xmain.h"

void InitAdc_pd(void);
WORD AdcGet_pd(BYTE channel);
__ramfunc void AdcAsyncStart_pd(BYTE channel);
__ramfunc BOOL AdcAsyncIsReady_pd(void);
__ramfunc WORD AdcAsyncGet_pd(void);

#endif
