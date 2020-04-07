/*---------------------------------------------------------
  keybrd_dvc.c
---------------------------------------------------------*/

#include "keybrd_pd.h"
#include "system.h"

#define KEY_IN_PORT     GPIOE
#define KEY_IN0_PIN     GPIO_PIN_13
#define KEY_IN1_PIN     GPIO_PIN_14
#define KEY_IN2_PIN     GPIO_PIN_15
#define KEY_OUT_PORT    GPIOE
#define KEY_OUT0_MASK   GPIO_MASK_PIN_9
#define KEY_OUT1_MASK   GPIO_MASK_PIN_10
#define KEY_OUT2_MASK   GPIO_MASK_PIN_11
#define KEY_OUT3_MASK   GPIO_MASK_PIN_12
#define KEY_OUT0_IN     GPIO_IN_PIN_9
#define KEY_OUT1_IN     GPIO_IN_PIN_10
#define KEY_OUT2_IN     GPIO_IN_PIN_11
#define KEY_OUT3_IN     GPIO_IN_PIN_12
#define KEY_OUT0_OUT    GPIO_OUT_PIN_9
#define KEY_OUT1_OUT    GPIO_OUT_PIN_10
#define KEY_OUT2_OUT    GPIO_OUT_PIN_11
#define KEY_OUT3_OUT    GPIO_OUT_PIN_12
#define KEY_OUT0_PIN    GPIO_PIN_9
#define KEY_OUT1_PIN    GPIO_PIN_10
#define KEY_OUT2_PIN    GPIO_PIN_11
#define KEY_OUT3_PIN    GPIO_PIN_12

static WORD GetInputLinesState(void);

//---------------------------------------------------------
void InitKeybrd_pd(void)
{
  GPIO_SET_MODE(KEY_OUT_PORT,
              KEY_OUT0_MASK |
              KEY_OUT1_MASK |
              KEY_OUT2_MASK |
              KEY_OUT3_MASK,
              KEY_OUT0_IN |
              KEY_OUT1_IN |
              KEY_OUT2_IN |
              KEY_OUT3_IN);
}

//---------------------------------------------------------
static WORD GetInputLinesState(void)
{
  WORD bits = GPIO_GET(KEY_IN_PORT);
  BYTE state = 0;
  if(!(bits & KEY_IN0_PIN)) state  = 0x01;
  if(!(bits & KEY_IN1_PIN)) state |= 0x02;
  if(!(bits & KEY_IN2_PIN)) state |= 0x04;
  return state;
}

//---------------------------------------------------------
WORD GetKey_pd(void)
{
  WORD key;

  GPIO_SET_MODE(KEY_OUT_PORT, KEY_OUT0_MASK, KEY_OUT0_OUT);
  GPIO_SET_OUT_LOW(KEY_OUT_PORT, KEY_OUT0_PIN);
  GPIO_SET_MODE(KEY_OUT_PORT,
                KEY_OUT1_MASK |
                KEY_OUT2_MASK |
                KEY_OUT3_MASK,
                KEY_OUT1_IN |
                KEY_OUT2_IN |
                KEY_OUT3_IN);
  key = GetInputLinesState();
  GPIO_SET_OUT_HIGH(KEY_OUT_PORT, KEY_OUT0_PIN);

  GPIO_SET_MODE(KEY_OUT_PORT, KEY_OUT1_MASK, KEY_OUT1_OUT);
  GPIO_SET_OUT_LOW(KEY_OUT_PORT, KEY_OUT1_PIN);
  GPIO_SET_MODE(KEY_OUT_PORT,
                KEY_OUT0_MASK |
                KEY_OUT2_MASK |
                KEY_OUT3_MASK,
                KEY_OUT0_IN |
                KEY_OUT2_IN |
                KEY_OUT3_IN);
  key |=  GetInputLinesState() << (1*4);
  GPIO_SET_OUT_HIGH(KEY_OUT_PORT, KEY_OUT1_PIN);

  GPIO_SET_MODE(KEY_OUT_PORT, KEY_OUT2_MASK, KEY_OUT2_OUT);
  GPIO_SET_OUT_LOW(KEY_OUT_PORT, KEY_OUT2_PIN);
  GPIO_SET_MODE(KEY_OUT_PORT,
              KEY_OUT0_MASK |
              KEY_OUT1_MASK |
              KEY_OUT3_MASK,
              KEY_OUT0_IN |
              KEY_OUT1_IN |
              KEY_OUT3_IN);
  key |= GetInputLinesState() << (2*4);
  GPIO_SET_OUT_HIGH(KEY_OUT_PORT, KEY_OUT2_PIN);
  
  GPIO_SET_MODE(KEY_OUT_PORT, KEY_OUT3_MASK, KEY_OUT3_OUT);
  GPIO_SET_OUT_LOW(KEY_OUT_PORT, KEY_OUT3_PIN);
  GPIO_SET_MODE(KEY_OUT_PORT,
              KEY_OUT0_MASK |
              KEY_OUT1_MASK |
              KEY_OUT2_MASK,
              KEY_OUT0_IN |
              KEY_OUT1_IN |
              KEY_OUT2_IN);
  key |= GetInputLinesState() << (3*4);
  GPIO_SET_OUT_HIGH(KEY_OUT_PORT, KEY_OUT3_PIN);
  GPIO_SET_MODE(KEY_OUT_PORT,
              KEY_OUT0_MASK |
              KEY_OUT1_MASK |
              KEY_OUT2_MASK |
              KEY_OUT3_MASK,
              KEY_OUT0_IN |
              KEY_OUT1_IN |
              KEY_OUT2_IN |
              KEY_OUT3_IN);
  
  return key;
}
