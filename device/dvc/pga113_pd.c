/*---------------------------------------------------------
  pga113_pd.c
---------------------------------------------------------*/

#include "pga113_pd.h"
#include "system.h"
#include "Delay.h"

#define PGA113_CS_PORT      GPIOD
#define PGA113_SCLK_PORT    GPIOD
#define PGA113_DIO_PORT     GPIOB
#define PGA113_CS_PIN       GPIO_PIN_5
#define PGA113_SCLK_PIN     GPIO_PIN_3
#define PGA113_DIO_PIN      GPIO_PIN_15
#define PGA113_CS_MASK      GPIO_MASK_PIN_5
#define PGA113_SCLK_MASK    GPIO_MASK_PIN_3
#define PGA113_DIO_MASK     GPIO_MASK_PIN_15
#define PGA113_CS_IN        GPIO_IN_PIN_5
#define PGA113_SCLK_IN      GPIO_IN_PIN_3
#define PGA113_DIO_IN       GPIO_IN_PIN_15
#define PGA113_CS_OUT       GPIO_OUT_PIN_5
#define PGA113_SCLK_OUT     GPIO_OUT_PIN_3
#define PGA113_DIO_OUT      GPIO_OUT_PIN_15

#define PGA113_K_COUNT    ARRAY_LENGTH(Pga113K)

NOINIT WORD Pga113Index;

static void Pga113Delay(void);
static void Pga113WriteK_pd(WORD k);
static void Pga113WriteKbyIndex_pd(WORD i);
static void Pga113Write_pd(WORD w);

//---------------------------------------------------------
void Pga113Init_pd(void)
{
  Pga113Index = 0;
  Pga113Off_pd();
}

//---------------------------------------------------------
void Pga113On_pd(void)
{
  GPIO_SET_OUT_HIGH(PGA113_CS_PORT, PGA113_CS_PIN);
  GPIO_SET_OUT_LOW(PGA113_SCLK_PORT, PGA113_SCLK_PIN);
  GPIO_SET_OUT_LOW(PGA113_DIO_PORT, PGA113_DIO_PIN);
  GPIO_SET_MODE(PGA113_CS_PORT, PGA113_CS_MASK, PGA113_CS_OUT);
  GPIO_SET_MODE(PGA113_SCLK_PORT, PGA113_SCLK_MASK, PGA113_SCLK_OUT);
  GPIO_SET_MODE(PGA113_DIO_PORT, PGA113_DIO_MASK, PGA113_DIO_OUT);
}

//---------------------------------------------------------
void Pga113Off_pd(void)
{
  GPIO_SET_MODE(PGA113_CS_PORT, PGA113_CS_MASK, PGA113_CS_IN);
  GPIO_SET_MODE(PGA113_SCLK_PORT, PGA113_SCLK_MASK, PGA113_SCLK_IN);
  GPIO_SET_MODE(PGA113_DIO_PORT, PGA113_DIO_MASK, PGA113_DIO_IN);
}

//---------------------------------------------------------
void Pga113SetK_pd(FLOAT32 value)
{
  if(value < Pga113K[0])
    value = Pga113K[0];
  else if(value > Pga113K[Pga113GetIndexMax()])
    value = Pga113K[Pga113GetIndexMax()];

  Pga113WriteK_pd((WORD)value);
}

//---------------------------------------------------------
static void Pga113WriteK_pd(WORD k)
{
  WORD i = Pga113GetIndexMax();

  do {
    if(Pga113K[i] <= k)
      break;
  } while(--i);

  Pga113WriteKbyIndex_pd(i);
}

//---------------------------------------------------------
static void Pga113WriteKbyIndex_pd(WORD i)
{
 // Pga113Write_pd(((Pga113Index = i) << 4U) | 0x2A00U);
  Pga113Write_pd(((Pga113Index = i) << 4U) | 0x2A01U);
}

//---------------------------------------------------------
static void Pga113Write_pd(WORD w)
{
  WORD i;

  GPIO_SET_OUT_LOW(PGA113_CS_PORT, PGA113_CS_PIN);

  for(i = 0; i < 16U; i++) {
    Pga113Delay();
    if(w & 0x8000U) {
      GPIO_SET_OUT_HIGH(PGA113_DIO_PORT, PGA113_DIO_PIN);
    }
    else {
      GPIO_SET_OUT_LOW(PGA113_DIO_PORT, PGA113_DIO_PIN);
    }
    Pga113Delay();
    GPIO_SET_OUT_HIGH(PGA113_SCLK_PORT, PGA113_SCLK_PIN);
    Pga113Delay();
    w <<= 1U;
    GPIO_SET_OUT_LOW(PGA113_SCLK_PORT, PGA113_SCLK_PIN);
  }

  Pga113Delay();
  GPIO_SET_OUT_HIGH(PGA113_CS_PORT, PGA113_CS_PIN);
}

//---------------------------------------------------------
void Pga113SetKmin_pd(void)
{
  Pga113WriteKbyIndex_pd(0);
}

//---------------------------------------------------------
void Pga113SetKmax_pd(void)
{
  Pga113WriteKbyIndex_pd(Pga113GetIndexMax());
}

//---------------------------------------------------------
BOOL Pga113SetKLower_pd(void)
{
  if(Pga113Index) {
    Pga113WriteKbyIndex_pd(--Pga113Index);
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
BOOL Pga113SetKLower2_pd(void)
{
  BYTE old_index = Pga113Index;

  if(Pga113Index >= 2)
    Pga113Index -= 2;
  else if(Pga113Index)
    Pga113Index = 0;

  if(old_index != Pga113Index) {
    Pga113WriteKbyIndex_pd(Pga113Index);
    return TRUE;
  }
  
  return FALSE;
}

//---------------------------------------------------------
BOOL Pga113SetKHigher_pd(void)
{
  if(Pga113Index < Pga113GetIndexMax()) {
    Pga113WriteKbyIndex_pd(++Pga113Index);
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
FLOAT32 Pga113GetK_pd(void)
{
  return PGA113_GET_K();
}

//---------------------------------------------------------
static void Pga113Delay(void)
{
  Delay(SYS_CONVERT_MCS(1U));
}

//---------------------------------------------------------
void Pga113SetKbyIndex_pd(BYTE index)
{
  if(index > Pga113GetIndexMax())
    index = Pga113GetIndexMax();
  Pga113WriteKbyIndex_pd(index);
}
