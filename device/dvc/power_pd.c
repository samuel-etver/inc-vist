/*---------------------------------------------------------
  power_pd.c
---------------------------------------------------------*/

#include "power_pd.h"
#include "system.h"

#define POWER_PORT      GPIOA
#define POWER_PIN       GPIO_PIN_2

//---------------------------------------------------------
void InitPower_pd(void)
{
} 

//---------------------------------------------------------
void PowerOn_pd(void)
{
  GPIO_SET_OUT_HIGH(POWER_PORT, POWER_PIN);
}

//---------------------------------------------------------
void PowerOff_pd(void)
{
  GPIO_SET_OUT_LOW(POWER_PORT, POWER_PIN);
}

