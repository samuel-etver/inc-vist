/*---------------------------------------------------------
  light_pd.c
---------------------------------------------------------*/

#include "light_pd.h"
#include "light.h"
#include "system.h"
#include "keybrd.h"

#define LIGHT_PORT      GPIOA
#define LIGHT_PIN       GPIO_PIN_2
#define LIGHT_MASK      GPIO_MASK_PIN_2
#define LIGHT_OUT       GPIO_OUT_PIN_2

static NOINIT WORD LightContrastValueOn;
static NOINIT WORD LightContrastValueOff;
NOINIT BOOL LightState;

//---------------------------------------------------------
void InitLight_pd(void)
{
  WORD period = htim2.Instance->ARR;
  LightContrastValueOn = (WORD)(period*0.5f);
  LightContrastValueOff = (WORD)(period*0.05f);
  LightState = TRUE;
}

//---------------------------------------------------------
void SetupLight_pd(void)
{
  HAL_TIM_OC_Start(&htim2, TIM_CHANNEL_2);
}

//---------------------------------------------------------
void DoLight_pd(void)
{
/*  if(KeyDown == KB_F2) {
    if(LightState == TRUE) {
      LightState = FALSE;
      HAL_TIM_OC_Stop(&htim2, TIM_CHANNEL_2);
    }
    else {
      LightState = TRUE;
      HAL_TIM_OC_Start(&htim2, TIM_CHANNEL_2);
    }
  }*/
}

//---------------------------------------------------------
void LightOn_pd(void)
{
  htim2.Instance->CCR2 = LightContrastValueOn;
}

//---------------------------------------------------------
void LightOff_pd(void)
{
  htim2.Instance->CCR2 = LightContrastValueOff;
}

//---------------------------------------------------------
void LightSetContrast_pd(BYTE contrast)
{
  WORD period = htim2.Instance->ARR;
  FLOAT32 value = contrast/100.0f*period;
  LightContrastValueOn = value >= period ? period - 2U : (WORD)value;
  LightOn_pd();
}

