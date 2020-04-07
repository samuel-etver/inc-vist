/*---------------------------------------------------------
  adc_pd.c
---------------------------------------------------------*/

#include "adc_pd.h"

//---------------------------------------------------------
void InitAdc_pd(void)
{
  ADC1->CR2 = 0x01;
}

//---------------------------------------------------------
WORD AdcGet_pd(BYTE channel)
{
  ADC1->SQR1 = 0;
  ADC1->SQR3 = channel;
  ADC1->CR2 |= 1U << 30U;
  while(!(ADC1->SR & 0x02));
  return ADC1->DR;
}

//---------------------------------------------------------
__ramfunc void AdcAsyncStart_pd(BYTE channel)
{
  ADC1->JSQR =
   ((DWORD)channel << 15U);
  ADC1->CR2 |= 1U << 22U;
}

//---------------------------------------------------------
__ramfunc BOOL AdcAsyncIsReady_pd(void)
{
  return (ADC1->SR & (1U << 2U)) ? TRUE : FALSE;
}

//---------------------------------------------------------
__ramfunc WORD AdcAsyncGet_pd(void)
{
  return (WORD)ADC1->JDR1;
}
