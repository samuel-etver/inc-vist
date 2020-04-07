/*---------------------------------------------------------
  measure_dvc.c
---------------------------------------------------------*/

#include "measure_pd.h"
#include "measure.h"
#include "system.h"
#include "adc.h"
#include "DeviceType.h"
#include "MeasureAmplifier.h"
#include "HardwareFilter.h"
#include "MeasuringTract.h"
#include "DeviceType.h"
#include "IncMeasureMode.h"
#include "VistParams.h"

#define MEASURE_POWER_PORT            GPIOD
#define MEASURE_POWER_PIN             GPIO_PIN_10

#define MEASURE_SIGNAL_CHANNEL        12U

#define MEASURE_SIGNAL_PORT1          GPIOD
#define MEASURE_SIGNAL_PORT2          GPIOB
#define MEASURE_SIGNAL_PIN1           GPIO_PIN_7
#define MEASURE_SIGNAL_PIN2           GPIO_PIN_5
#define MEASURE_SIGNAL_MASK1          GPIO_MASK_PIN_7
#define MEASURE_SIGNAL_MASK2          GPIO_MASK_PIN_5
#define MEASURE_SIGNAL_IN1            GPIO_IN_PIN_7
#define MEASURE_SIGNAL_IN2            GPIO_IN_PIN_5
#define MEASURE_SIGNAL_OUT1           GPIO_OUT_PIN_7
#define MEASURE_SIGNAL_OUT2           GPIO_OUT_PIN_5

static NOINIT WORD MeasureTmp;
static NOINIT WORD MeasureTmpX;
static NOINIT WORD MeasureIndex;
static NOINIT WORD* MeasurePeriodSignTicksPtr;
static NOINIT WORD MeasurePeriodDownCnt;
static NOINIT BYTE MeasurePowerPort;

void TIM1_BRK_TIM9_IRQHandler(void);

//---------------------------------------------------------
void MeasureInit_pd(void)
{
  MeasurePowerPort = 0x00;
  MeasureOnExit_pd();
}

//---------------------------------------------------------
void MeasureOnEnter_pd(void)
{
  FLOAT32 fs;
  GPIO_SET_MODE(MEASURE_SIGNAL_PORT1, MEASURE_SIGNAL_MASK1, MEASURE_SIGNAL_OUT1);
  GPIO_SET_MODE(MEASURE_SIGNAL_PORT2, MEASURE_SIGNAL_MASK2, MEASURE_SIGNAL_OUT2);
  MeasureSetParamOn(mpVrms);
  htim9.Instance->CNT = 0;
  AdcAsyncStart(MEASURE_SIGNAL_CHANNEL);
  MeasureAmplifierSetOn();
  MeasureAmplifierSetKmax();
  if(DeviceType == dtInc ||
     (DeviceType == dtIncVist && IncMeasureMode == incMmInc))
    fs = 100.0f;
  else {
    if(VistParamsObject == 0) // Общий
      fs = 500.0f;
    else
      fs = 85.0f; // виброплощадка
  }
  HardwareFilterSetFrequency(fs);
  HardwareFilterOn();
  HAL_TIM_Base_Start_IT(&htim9);
}

//---------------------------------------------------------
void MeasureOnExit_pd(void)
{
  HAL_TIM_Base_Stop_IT(&htim9);
  HardwareFilterOff();
  MeasureAmplifierSetOff();
  GPIO_SET_MODE(MEASURE_SIGNAL_PORT1, MEASURE_SIGNAL_MASK1, MEASURE_SIGNAL_IN1);
  GPIO_SET_MODE(MEASURE_SIGNAL_PORT2, MEASURE_SIGNAL_MASK2, MEASURE_SIGNAL_IN2);
}

//---------------------------------------------------------
void MeasureSetSon_pd(void)
{
  GPIO_SET_OUT_LOW(MEASURE_SIGNAL_PORT1, MEASURE_SIGNAL_PIN1);
  GPIO_SET_OUT_LOW(MEASURE_SIGNAL_PORT2, MEASURE_SIGNAL_PIN2);
  MeasurePeriodPlus = 0;
}

//---------------------------------------------------------
void MeasureSetVon_pd(void)
{
  GPIO_SET_OUT_LOW(MEASURE_SIGNAL_PORT1, MEASURE_SIGNAL_PIN1);
  GPIO_SET_OUT_HIGH(MEASURE_SIGNAL_PORT2, MEASURE_SIGNAL_PIN2);
  MeasurePeriodPlus = 0;
}

//---------------------------------------------------------
void MeasureSetAon_pd(void)
{
  GPIO_SET_OUT_HIGH(MEASURE_SIGNAL_PORT1, MEASURE_SIGNAL_PIN1);
  GPIO_SET_OUT_HIGH(MEASURE_SIGNAL_PORT2, MEASURE_SIGNAL_PIN2);
  MeasurePeriodPlus = 0;
}

//---------------------------------------------------------
__ramfunc void TIM1_BRK_TIM9_IRQHandler(void) // 25 mcs
{
  BYTE k_ind;

  if((TIM9->SR & TIM_SR_UIF)) {
    TIM9->SR = ~TIM_SR_UIF;

    MeasurePeriodPlusTicks++;
    MeasurePeriodMinusTicks++;

    MeasureAdc = AdcAsyncGet();
    AdcAsyncStart(MEASURE_SIGNAL_CHANNEL);

    if(MeasureAdc > MEASURE_SIGNAL_OVERLOAD_HIGH ||
       MeasureAdc < MEASURE_SIGNAL_OVERLOAD_LOW) {
      MeasureSignalOverloadCnt++;
    }

    if(MeasureAdc > MEASURE_SIGNAL_LOW_HIGH ||
       MeasureAdc < MEASURE_SIGNAL_LOW_LOW) {
      MeasureSignalLowLevelNotFound = TRUE;
    }

    MeasureTmp = MeasureAdc < MeasureAdcValueZero
      ? (MeasureAdcValueZero - MeasureAdc)
      : (MeasureAdc - MeasureAdcValueZero);

    MeasureAdcX = (MeasureAdcX*15U + MeasureAdc)/16U;
    MeasureTmpX = MeasureAdcX < MeasureAdcValueZero
      ? (MeasureAdcValueZero - MeasureAdcX)
      : (MeasureAdcX - MeasureAdcValueZero);

    k_ind = MEASUREAMPLIFIER_GET_INDEX();

    MeasureVsqrSum += MeasureTmp*MeasureTmp;
    if(++MeasureVsqrSumCnt == MEASURE_V_SQR_SUM_COUNT_MAX) {
      MeasureAdcAvrSumArr[MeasureVsqrSumIPtr]  = MeasureVsqrSum;
      MeasureAdcAvrKIndArr[MeasureVsqrSumIPtr] = k_ind;
      if(++MeasureVsqrSumIPtr == MEASURE_V_SQR_SUM_SIZE)
        MeasureVsqrSumIPtr = 0;
      if(MeasureVsqrSumICnt < MEASURE_V_SQR_SUM_SIZE)
        MeasureVsqrSumICnt++;
      MeasureVsqrSumCnt = 0;
      MeasureVsqrSum = 0;
    }

    if(MeasureAdc > MeasureAdcValueMaxTmp) {
      MeasureAdcValueMaxTmp = MeasureAdc;
      MeasureAdcValueMinMaxKIndTmp = k_ind;
    }
    if(MeasureAdc < MeasureAdcValueMinTmp) {
      MeasureAdcValueMinTmp = MeasureAdc;
      MeasureAdcValueMinMaxKIndTmp = k_ind;
    }

    if(++MeasurePeriodAvailableTmpCnt == 40000U) { // 10 s
      MeasurePeriodAvailableTmp = FALSE;
      MeasurePeriodPtr = 0;
      return;
    }


    if(MeasureAdcX > MeasureAdcValueZero) {
      if(MeasurePeriodPlus) {
        if(MeasureTmp > MeasurePeriodAdcMax)
          MeasurePeriodAdcMax = MeasureTmp;
        if(MeasureTmpX >= MeasurePeriodAdcMaxX) {
          MeasurePeriodAdcMaxX = MeasureTmpX;
          MeasurePeriodDownCnt = 0;
        }
        else if(MeasurePeriodDownCnt < MEASURE_PERIOD_DOWN_COUNT_MAX) {
          ++MeasurePeriodDownCnt;
        }
        else if(MeasureTmpX <= MeasurePeriodAdcMaxX/2U) {
          MeasurePeriodSignTicksPtr = &MeasurePeriodPlusTicks;
          goto Reset;
        }
      }
    }
    else {
      if(!MeasurePeriodPlus) {
        if(MeasureTmp > MeasurePeriodAdcMax)
          MeasurePeriodAdcMax = MeasureTmp;
        if(MeasureTmpX >= MeasurePeriodAdcMaxX) {
          MeasurePeriodAdcMaxX = MeasureTmpX;
          MeasurePeriodDownCnt = 0;
        }
        else if(MeasurePeriodDownCnt < MEASURE_PERIOD_DOWN_COUNT_MAX) {
          ++MeasurePeriodDownCnt;
        }
        else if(MeasureTmpX <= MeasurePeriodAdcMaxX/2U) {
          MeasurePeriodSignTicksPtr = &MeasurePeriodMinusTicks;
          goto Reset;
        }
      }
    }
    return;

    Reset: {
      MeasurePeriodPlus ^= 1;
      if((MeasureIndex = MeasurePeriodPtr) >= MEASURE_PERIOD_SIZE)
        MeasureIndex -= MEASURE_PERIOD_SIZE;
      MeasurePeriodArr[MeasureIndex] = *MeasurePeriodSignTicksPtr;
      *MeasurePeriodSignTicksPtr = 0;
      MeasurePeriodAdcPeakArr[MeasureIndex] = MeasurePeriodAdcMax;
      MeasurePeriodKIndArr[MeasureIndex] = k_ind;
      if(++MeasurePeriodPtr == 2U*MEASURE_PERIOD_SIZE)
        MeasurePeriodPtr = MEASURE_PERIOD_SIZE;
      MeasurePeriodAvailableTmp = TRUE;
      MeasurePeriodAvailableTmpCnt = 0;
      MeasurePeriodAdcMaxX = 0;
      MeasurePeriodAdcMax  = 0;
    }
  }
}

//---------------------------------------------------------
BOOL MeasureCalcT_pd(FLOAT32* data)
{
  WORD i;
  WORD j;
  WORD index_i;
  WORD index_j;
  WORD cnt;
  WORD ptr;
  WORD index;
  DWORD mediana;
  WORD lo;
  WORD hi;
  DWORD i_sum;
  FLOAT32 f_sum;
  DWORD lo_value;
  DWORD hi_value;
  DWORD delta;
  WORD n;
  WORD peak;
  FLOAT32 peak_k;
  FLOAT32 adc_k = AdcGetK();

  if((MeasurePeriodAvailable = MeasurePeriodAvailableTmp) == FALSE) {
    return FALSE;
  }

  cnt = MeasurePeriodPtr < MEASURE_PERIOD_SIZE ?
   MeasurePeriodPtr : MEASURE_PERIOD_SIZE;
  ptr = MeasurePeriodPtr%MEASURE_PERIOD_SIZE;

  // Copy
  if(cnt < MEASURE_PERIOD_COUNT_MIN)
    return FALSE;

  if(cnt > MEASURE_PERIOD_COUNT_MAX)
    cnt = MEASURE_PERIOD_COUNT_MAX;

  cnt -= 2U;
  if((cnt & 1) == 0)
    cnt--;

  // Create index array
  index = (ptr + (MEASURE_PERIOD_SIZE - 1U) - cnt)%MEASURE_PERIOD_SIZE;
  for(i = 0; i < cnt; i++) {
    MeasurePeriodIndexArr[i] = index;
    if(++index == MEASURE_PERIOD_SIZE)
      index = 0;
  }

  // Sort
  for(i = 0; i < cnt; i++) {
    for(j = i + 1; j < cnt; j++) {
      index_i = MeasurePeriodIndexArr[i];
      index_j = MeasurePeriodIndexArr[j];
      if(MeasurePeriodArr[index_i] > MeasurePeriodArr[index_j]) {
        MeasurePeriodIndexArr[i] = index_j;
        MeasurePeriodIndexArr[j] = index_i;
      }
    }
  }

  // Mediana
  mediana = MeasurePeriodArr[MeasurePeriodIndexArr[i = cnt/2U]];
  delta = (DWORD)(mediana*0.25f);
  hi_value = mediana + delta;
  lo_value = mediana - delta;

  // Filter
  for(lo = 0; lo < i; lo++) {
    if(MeasurePeriodArr[MeasurePeriodIndexArr[lo]] >= lo_value)
      break;
  }
  for(hi = cnt - 1; hi >= i; hi--) {
    if(MeasurePeriodArr[MeasurePeriodIndexArr[hi]] <= hi_value)
      break;
  }
  n = hi - lo + 1U;

  // Period
  i_sum = 0;
  for(i = lo; i <= hi; i++) {
    i_sum += MeasurePeriodArr[MeasurePeriodIndexArr[i]];
  }
  data[0] = (0.000001f*MEASURE_PERIOD_TICK_IN_MCS)*(FLOAT32)i_sum/n;

  // Noise
  data[1] = 100.0f*(FLOAT32)(cnt - n)/cnt;

  // Peak
  peak = 0;
  peak_k = 1.0;
  f_sum  = 0;
  for(i = lo; i <= hi; i++) {
    index = MeasurePeriodIndexArr[i];
    j = MeasurePeriodAdcPeakArr[index];
    if(peak < j) {
      peak = j;
      peak_k = MEASUREAMPLIFIER_GET_KI(MeasurePeriodKIndArr[index]);
    }
    f_sum += MeasurePeriodAdcPeakArr[index]/
     MEASUREAMPLIFIER_GET_KI(MeasurePeriodKIndArr[index]);
  }
  data[2] = peak*adc_k/peak_k;

  // Amp
  data[3] = f_sum/n*adc_k;

  return TRUE;
}

//---------------------------------------------------------
FLOAT32 MeasureCalcUavr_pd(FLOAT32 value)
{
  return value;
}

//---------------------------------------------------------
void MeasurePowerOn_pd(BYTE mask)
{
  if((MeasurePowerPort |= mask)) {
    GPIO_SET_OUT_HIGH(MEASURE_POWER_PORT, MEASURE_POWER_PIN);
  }
}

//---------------------------------------------------------
void MeasurePowerOff_pd(BYTE mask)
{
  if(!(MeasurePowerPort &= ~mask)) {
    GPIO_SET_OUT_LOW(MEASURE_POWER_PORT, MEASURE_POWER_PIN);
  }
}

