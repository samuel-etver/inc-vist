/*---------------------------------------------------------
  sound_pd.c
---------------------------------------------------------*/

#include "sound_pd.h"
#include "system.h"
#include "delay.h"

#define SOUND_PORT      GPIOB
#define SOUND_PIN       GPIO_PIN_1
#define SOUND_MASK      GPIO_MASK_PIN_1
#define SOUND_OUT       GPIO_OUT_PIN_1
#define SOUND_ALT       GPIO_ALT_PIN_1

//---------------------------------------------------------
void InitSound_pd(void)
{
  SoundOff_pd();    
}

//---------------------------------------------------------
void SoundOn_pd(void)
{
  GPIO_SET_MODE(SOUND_PORT, SOUND_MASK, SOUND_ALT);
  HAL_TIM_OC_Start(&htim3, TIM_CHANNEL_4);
}

//---------------------------------------------------------
void SoundOff_pd(void)
{
  HAL_TIM_OC_Stop(&htim3, TIM_CHANNEL_4);
  GPIO_SET_MODE(SOUND_PORT, SOUND_MASK, SOUND_OUT);
  GPIO_SET_OUT_LOW(SOUND_PORT, SOUND_PIN);
}

//---------------------------------------------------------
void SoundSetFastFrequencyBefore_pd(void)
{
}

//---------------------------------------------------------
void SoundSetNormalFrequencyBefore_pd(void)
{
}
