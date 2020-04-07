#ifndef ADC_H_INCLUDED
#define ADC_H_INCLUDED

/*---------------------------------------------------------
  adc.h
---------------------------------------------------------*/

#include "xmain.h"
#include "adc_pd.h"

#define ADC_CODE_MAX      0xFFF

void InitAdc(void);
FLOAT32 AdcGetK(void);
WORD AdcGet(BYTE channel);
FLOAT32 AdcGetAsFloat(BYTE channel);

#define AdcAsyncStart   AdcAsyncStart_pd
#define AdcAsyncIsReady AdcAsyncIsReady_pd
#define AdcAsyncGet     AdcAsyncGet_pd

#endif
