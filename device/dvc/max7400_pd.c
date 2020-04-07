/*---------------------------------------------------------
  max7400_pd.c
---------------------------------------------------------*/

#include "max7400_pd.h"
#include "system.h"

#define MAX7400_PORT    GPIOD
#define MAX7400_PIN     GPIO_PIN_6
#define MAX7400_MASK    GPIO_MASK_PIN_6
#define MAX7400_OUT     GPIO_OUT_PIN_6
#define MAX7400_IN      GPIO_IN_PIN_6

static NOINIT DWORD Max7400State;

void TIM4_IRQHandler(void);

//---------------------------------------------------------
void InitMax7400_pd(void)
{
  Max7400State = MAX7400_PIN;
}

//---------------------------------------------------------
void Max7400On_pd(void)
{
  HAL_TIM_Base_Start_IT(&htim4);
  GPIO_SET_MODE(MAX7400_PORT, MAX7400_MASK, MAX7400_OUT); 
}

//---------------------------------------------------------
void Max7400Off_pd(void)
{
  HAL_TIM_Base_Stop_IT(&htim4);
  GPIO_SET_MODE(MAX7400_PORT, MAX7400_MASK, MAX7400_IN);
}

//---------------------------------------------------------
void Max7400SetFrequency_pd(FLOAT32 f)
{
  WORD w = (WORD)((0.01f*1000000.0f/2.0f)/f);

  if((w & 1U))
    w++;
  
  TIM4->ARR = w;
  TIM4->CNT = 0;
}

//---------------------------------------------------------
__ramfunc void TIM4_IRQHandler(void)
{
  if((TIM4->SR & TIM_SR_UIF)) {
    Max7400State ^= MAX7400_PIN | (MAX7400_PIN << 16U);
    GPIO_SET1(MAX7400_PORT, Max7400State);
    TIM4->SR = ~TIM_SR_UIF;
  }
}
