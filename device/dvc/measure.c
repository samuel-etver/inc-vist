/*---------------------------------------------------------
  measure.c
---------------------------------------------------------*/

#include "measure.h"
#include "measure_pd.h"
#include "lcd.h"
#include "keybrd.h"
#include "calendar.h"
#include <math.h>
#include "Delay.h"
#include "system.h"
#include "language.h"
#include "mem.h"
#include "light.h"
#include "utils.h"
#include "autooff.h"
#include "adc.h"
#include "DeviceType.h"
#include "IncMeasure.h"
#include "VistMeasure.h"
#include "VistParams.h"
#include "VistSensor.h"
#include "HardwareFilter.h"
#include "MeasuringTract.h"
#include "MeasureAmplifier.h"
#include "IonCalibration.h"
#include "ZeroCalibration.h"

#define MEASURE_FILTER_N_10                 10
#define MEASURE_FILTER_N_20                 20
#define MEASURE_FILTER_N                    MEASURE_FILTER_N_20
#define MEASURE_FILTER_DIMENSION            (2*MEASURE_FILTER_N-1)

NOINIT TMeasureVerb MeasureVerb;
NOINIT TMeasureFlags MeasureFlags;
NOINIT WORD MeasureNumber;
NOINIT BYTE MeasureHour;
NOINIT BYTE MeasureMin;
NOINIT BYTE MeasureSec;
NOINIT BYTE MeasureDay;
NOINIT BYTE MeasureMon;
NOINIT BYTE MeasureYear;
static NOINIT TDelay MeasureDelay;
NOINIT BYTE MeasurePeriodPlus;
NOINIT WORD MeasureAdc;
NOINIT WORD MeasureAdcX;
NOINIT DWORD MeasurePeriodArr[MEASURE_PERIOD_SIZE];
NOINIT WORD MeasurePeriodIndexArr[MEASURE_PERIOD_COUNT_MAX];
NOINIT WORD MeasurePeriodPtr;
NOINIT WORD MeasurePeriodPlusTicks;
NOINIT WORD MeasurePeriodMinusTicks;
NOINIT WORD MeasurePeriodAdcMax;
NOINIT WORD MeasurePeriodAdcMaxX;
NOINIT WORD MeasurePeriodAdcPeakArr[MEASURE_PERIOD_SIZE];
NOINIT BYTE MeasurePeriodKIndArr[MEASURE_PERIOD_SIZE];
NOINIT BOOL MeasurePeriodAvailableTmp;
NOINIT WORD MeasurePeriodAvailableTmpCnt;
NOINIT BOOL MeasurePeriodAvailable;
NOINIT FLOAT32 MeasureT;
NOINIT FLOAT32 MeasureF;
NOINIT FLOAT32 MeasureNoise;
NOINIT FLOAT32 MeasureUavr;
NOINIT FLOAT32 MeasureUamp;
NOINIT FLOAT32 MeasureU2amp;
NOINIT FLOAT32 MeasureUpeak;
NOINIT FLOAT32 MeasureS;
NOINIT FLOAT32 MeasureV;
NOINIT FLOAT32 MeasureA;
NOINIT FLOAT32 MeasureTResult;
NOINIT FLOAT32 MeasureFResult;
NOINIT FLOAT32 MeasureNoiseResult;
NOINIT FLOAT32 MeasureSResult;
NOINIT FLOAT32 MeasureVResult;
NOINIT FLOAT32 MeasureAResult;
NOINIT FLOAT32 MeasureSignalLevel;
NOINIT WORD MeasureSignalOverloadCnt;
NOINIT TDelay MeasureSignalOverloadDelay;
NOINIT BOOL MeasureSignalOverloadSkip;
NOINIT TDelay MeasureSignalLowLevelDelay;
NOINIT BOOL MeasureSignalLowLevelNotFound;
NOINIT WORD MeasureAdcValueZero;
NOINIT WORD MeasureAdcValueMin;
NOINIT WORD MeasureAdcValueMax;
NOINIT BYTE MeasureAdcValueMinMaxKInd;
NOINIT WORD MeasureAdcValueMinTmp;
NOINIT WORD MeasureAdcValueMaxTmp;
NOINIT BYTE MeasureAdcValueMinMaxKIndTmp;
NOINIT TDelay MeasureAdcValueMinMaxDelay;
NOINIT TLcdRect MeasureNumberRect;
NOINIT TLcdRect MeasureObjectRect;
NOINIT TLcdRect MeasureCalendarRect;
NOINIT TLcdRect MeasureTRect;
NOINIT TLcdRect MeasureFRect;
NOINIT TLcdRect MeasureNoiseRect;
NOINIT TLcdRect MeasureVRect;
NOINIT TTrigger MeasureNumberTrigger;
NOINIT TTrigger MeasureTTrigger;
NOINIT TTrigger MeasureFTrigger;
NOINIT TTrigger MeasureNoiseTrigger;
NOINIT TTrigger MeasureSTrigger;
NOINIT TTrigger MeasureVTrigger;
NOINIT TTrigger MeasureATrigger;
NOINIT TTrigger MeasureTrigger1;
NOINIT TTrigger MeasureTrigger2;
NOINIT TTrigger MeasureTrigger3;
NOINIT TColor MeasureNumberColor;
NOINIT TColor MeasureCalendarColor;
NOINIT TColor MeasureFColor;
NOINIT TColor MeasureSColor;
NOINIT TColor MeasureVColor;
NOINIT TColor MeasureAColor;
NOINIT TColor MeasureSigmaColor;
NOINIT TColor MeasureNoiseColor;
NOINIT TColor MeasureDoneColor;
NOINIT TColor MeasureErrorMessageColor;
NOINIT DWORD MeasureAdcAvrSumArr[MEASURE_V_SQR_SUM_SIZE];
NOINIT BYTE MeasureAdcAvrKIndArr[MEASURE_V_SQR_SUM_SIZE];
NOINIT DWORD MeasureVsqrSum;
NOINIT WORD MeasureVsqrSumCnt;
NOINIT WORD MeasureVsqrSumIPtr;
NOINIT WORD MeasureVsqrSumICnt;
NOINIT BOOL MeasureAutoAmplifierEnabled;
NOINIT BYTE MeasureAmplifierStage;
NOINIT DWORD MeasureAmplifierTicks;
NOINIT TMeasureParam MeasureParam;
static NOINIT FLOAT32 MeasureFilterUData[MEASURE_FILTER_DIMENSION];
static NOINIT TMeasureFilter MeasureFilterU;
static NOINIT FLOAT32 MeasureFilterFData[MEASURE_FILTER_DIMENSION];
static NOINIT TMeasureFilter MeasureFilterF;
static NOINIT BOOL MeasureFilterFEnabled;


#if MEASURE_FILTER_N == MEASURE_FILTER_N_10
static const FLOAT32 MeasureFilterB[] = { // N=10
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  9, 8, 7, 6, 5, 4, 3, 2, 1
};
#elif MEASURE_FILTER_N == MEASURE_FILTER_N_20
static const FLOAT32 MeasureFilterB[] = { // N=20
   1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
 	11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
	19, 18, 17, 16, 15, 14, 13, 12, 11, 10,
	 9,  8,  7,  6,  5,  4,  3,  2,  1
};
#endif

static void MeasureResetMinMax(void);
static BOOL MeasureCalcT(FLOAT32* data);
static BYTE MeasureSVAtoStr(BYTE* buff, FLOAT32 value, BOOL f);

//---------------------------------------------------------
void InitMeasure(void)
{
  MeasureInit_pd();
  HardwareFilterInit();
  MeasureAmplifierInit();
  MeasureFlags.MeasureEntered = 0;
  MeasureFilterU.Data = MeasureFilterUData;
  MeasureFilterF.Data = MeasureFilterFData;
  MeasureFilterFEnabled = TRUE;
  MeasurePowerOff();
}

//---------------------------------------------------------
void MeasureMeasure(void)
{
  FLOAT32 tmp;

  if(!MeasureFlags.MeasureEntered)
    return;

  if(!MeasureFlags.Measuring) {
    if(DelayIsEnded(MeasureDelay, SYS_CONVERT_MCS(200000U)) == FALSE)
      return;
    MeasureFlags.Measuring = 1;
    MeasureReset();
  }

  if(PrgFlags.CentiSec)
    MeasureCalc();
  else {
    switch(MeasureAmplifierStage) {
      case 0:
        MeasureAmplifierStage++;
        MeasureAmplifierTicks = GetTicks();
      case 1:
        MeasureReset();
        MeasureAmplifierStage = 1;
        if(DelayIsEnded(MeasureAmplifierTicks, SYS_CONVERT_MCS(20000)) == TRUE) {
          MeasureAmplifierStage++;
        }
        break;
      case 2:
        MeasureAmplifierStage++;
        MeasureSignalOverloadDelay =
        MeasureSignalLowLevelDelay = DelayBegin();
        MeasureSignalOverloadCnt = 0;
        MeasureSignalLowLevelNotFound = FALSE;
        break;
      case 3:
        if(DelayIsEnded(MeasureSignalOverloadDelay, SYS_CONVERT_MCS(100000U)) == TRUE) {
          MeasureSignalOverloadDelay = DelayBegin();
          if(MeasureSignalOverloadCnt > 0) {
            if(MeasureAutoAmplifierEnabled == TRUE) {
              MeasureAmplifierSetKLower2();
              MeasureAmplifierStage = 0;
              break;
            }
          }
          MeasureSignalOverloadCnt = 0;
        }

        if(DelayIsEnded(MeasureSignalLowLevelDelay, SYS_CONVERT_MCS(500000U)) == TRUE) {
          MeasureSignalLowLevelDelay = DelayBegin();
          if(MeasureSignalLowLevelNotFound == FALSE &&
             MeasureAutoAmplifierEnabled   == TRUE) {
            if(MeasureAmplifierSetKHigher() == TRUE)
              MeasureAmplifierStage = 0;
          }
          MeasureSignalLowLevelNotFound = FALSE;
        }
    }

    if(DelayIsEnded(MeasureAdcValueMinMaxDelay, SYS_CONVERT_MCS(1000000U)) == TRUE) {
      MeasureAdcValueMinMaxDelay = DelayBegin();
      MeasureAdcValueMin = MeasureAdcValueMinTmp;
      MeasureAdcValueMax = MeasureAdcValueMaxTmp;
      MeasureAdcValueMinMaxKInd = MeasureAdcValueMinMaxKIndTmp;
      MeasureResetMinMax();

      tmp = 100.0f/ADC_CODE_MAX*
       (fabs((INT16)MeasureAdcValueMax - (INT16)MeasureAdcValueMin))/
       MEASUREAMPLIFIER_GET_KI(MeasureAdcValueMinMaxKInd);
      if(tmp > 100.0f)
        tmp = 100.0f;
      MeasureSignalLevel = tmp;
    }
  }
}

//---------------------------------------------------------
void ShowMeasureIni(void)
{
  PrgVerb = VB_MEASURE;

  switch(DeviceType) {
    case dtInc:
    case dtIncVist:
      MeasureVerb = MV_INC_INI;
      break;
    default:
      MeasureVerb = MV_VIST_INI;
  }

  MeasureOnEnter();
  MeasureLoadNumber();
  MeasureSetColors();
}

//---------------------------------------------------------
void ShowMeasure(void)
{
  switch(MeasureVerb) {
    case MV_INC_INI:
      ShowIncMeasureIni();
      MeasureVerb = MV_INC;
    case MV_INC:
      ShowIncMeasure(); break;
    case MV_VIST_INI:
      ShowVistMeasureIni();
      MeasureVerb = MV_VIST;
    case MV_VIST:
      ShowVistMeasure();
  }

  if(KeyDown == KB_MENU)
    PrgVerb = VB_MENU_INI;

  MeasureMeasure();

  if(PrgVerb != VB_MEASURE) {
    MeasureOnExit();
  }

  ClearAutoOff();
}

//---------------------------------------------------------
void MeasureOnEnter(void)
{
  MeasureAdcValueZero = ZeroCalibrationGetCode();
  MeasureSignalLevel = 0;
  MeasurePeriodAdcMax = 0;
  MeasurePeriodAdcMaxX = 0;

  MeasureReset();
  MeasurePowerOn();
  MeasureFilterFOn();
  MeasureOnEnter_pd();

  MeasureDelay = DelayBegin();
  MeasureAutoAmplifierEnabled = TRUE;
  MeasureFlags.MeasureEntered = 1;
  MeasureFlags.Measuring = 0;

  MeasureFilterU.Started = FALSE;
  MeasureFilterF.Started = FALSE;
}

//---------------------------------------------------------
void MeasureOnExit(void)
{
  MeasurePeriodPlusTicks = 0;
  MeasurePeriodMinusTicks = 0;
  MeasureFlags.Measuring = 0;
  MeasureFlags.MeasureEntered = 0;
  MeasureOnExit_pd();
  MeasurePowerOff();
}

//---------------------------------------------------------
void MeasureReset(void)
{
  DisableInterrupts();
  MeasurePeriodPtr = 0U;
  MeasureAdc  =
  MeasureAdcX =  MeasureAdcValueZero;
  MeasurePeriodAvailableTmp = FALSE;
  MeasurePeriodAvailableTmpCnt = 0;
  MeasurePeriodAvailable = FALSE;
  MeasureVsqrSum = 0U;
  MeasureVsqrSumCnt = 0U;
  MeasureVsqrSumICnt = 0U;
  MeasureVsqrSumIPtr = 0U;
  MeasureResetMinMax();
  EnableInterrupts();
  MeasureF = 0.0f;
  MeasureT = 0.0f;
  MeasureUavr  = 0.0f;
  MeasureUamp  = 0.0f;
  MeasureU2amp = 0.0f;
  MeasureUpeak = 0.0f;
  MeasureS = 0.0f;
  MeasureV = 0.0f;
  MeasureA = 0.0f;
  MeasureNoise = 0.0f;
  MeasureSignalLevel = 0.0f;
  MeasureSignalOverloadCnt = 0U;
  MeasureSignalOverloadDelay       =
  MeasureSignalLowLevelDelay       =
  MeasureAdcValueMinMaxDelay = DelayBegin();
  MeasureSignalOverloadSkip = TRUE;
  MeasureSignalLowLevelNotFound = FALSE;
  MeasureFlags.Result = 0;
  MeasureAmplifierStage = 0;
  IncMeasureReset();
}

//---------------------------------------------------------
static void MeasureResetMinMax(void)
{
  MeasureAdcValueMinTmp = ADC_CODE_MAX;
  MeasureAdcValueMaxTmp = 0;
  MeasureAdcValueMinMaxKIndTmp = MEASUREAMPLIFIER_GET_INDEX();
}

//---------------------------------------------------------
void MeasureSetColors(void)
{
  switch(Theme) {
    case themeSimple:
      MeasureNumberColor = LCD_RGB_TO_COLOR(96,96,96);
      MeasureCalendarColor = LCD_RGB_TO_COLOR(128,40,100);
      MeasureFColor = LCD_RGB_TO_COLOR(102,0,204);
      MeasureSColor = LCD_RGB_TO_COLOR(0,0,0);
      MeasureNoiseColor = LCD_RGB_TO_COLOR(0,102,204);
      MeasureDoneColor = LCD_RGB_TO_COLOR(196,0,0);
      MeasureErrorMessageColor = LCD_RGB_TO_COLOR(255,0,0);
      break;

    case themeDark:
      MeasureNumberColor = LCD_RGB_TO_COLOR(220,220,220);
      MeasureCalendarColor = LCD_RGB_TO_COLOR(230,110,220);
      MeasureFColor = LCD_RGB_TO_COLOR(255,255,0);
      MeasureSColor = LCD_RGB_TO_COLOR(0,220,140);
      MeasureNoiseColor = LCD_RGB_TO_COLOR(0,170,235);
      MeasureDoneColor = LCD_RGB_TO_COLOR(255,120,120);
      MeasureErrorMessageColor = LCD_RGB_TO_COLOR(255,0,0);
      break;

    default:
      MeasureNumberColor = LCD_RGB_TO_COLOR(96,96,96);
      MeasureCalendarColor = LCD_RGB_TO_COLOR(128,40,100);
      MeasureFColor = LCD_RGB_TO_COLOR(102,0,204);
      MeasureSColor = LCD_RGB_TO_COLOR(0,153,76);
      MeasureNoiseColor = LCD_RGB_TO_COLOR(0,102,204);
      MeasureDoneColor = LCD_RGB_TO_COLOR(196,0,0);
      MeasureErrorMessageColor = LCD_RGB_TO_COLOR(255,0,0);
  }

  MeasureVColor = MeasureSColor;
  MeasureAColor = MeasureSColor;
  MeasureSigmaColor = MeasureSColor;
}

//---------------------------------------------------------
void MeasureLoadNumber(void)
{
  MemLocateLast();

  MeasureNumber = 1;

  if(!MemFlags.Empty) {
    if(CalendarDay  == MemCellDate[0] &&
       CalendarMon  == MemCellDate[1] &&
       CalendarYear == MemCellDate[2])
      MeasureNumber = MemCellNumber + 1;
  }
}

//---------------------------------------------------------
BYTE MeasureFtoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  BYTE n;
  if(f == FALSE) {
    memcpy(buff, "?", n = 1);
  } else {
    if(value < 0)
      value = 0;
    if(value <= 99.99f) {
      n = FloatToStr(buff, value, 2);
    }
    else if(value <= 999.9f) {
      n = FloatToStr(buff, value, 1);
    }
    else if(value <= 9999.0f) {
      n = FloatToStr(buff, value, 0);
    }
    else {
      memcpy(buff, "****", n = 4);
    }
  }
  return n;
}

//---------------------------------------------------------
BYTE MeasureTtoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  BYTE n;
  if(f == FALSE) {
    memcpy(buff, "?", n = 1);
  }
  else {
    value *= 1000.0f; // ms
    if(value <= 9999.99f) {
      n = (BYTE)sprintf((char*)buff, "%7.2f", value);
    }
    else {
      memcpy(buff, "****.**", n = 7);
    }
  }
  return n;
}

//---------------------------------------------------------
BYTE MeasureNoiseToStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  BYTE n;
  if(f == FALSE) {
    memcpy(buff, "?", n = 1);
  }
  else {
    if(value < 0.0f)
      value = 0.0f;
    else if(value > 100.0f)
      value = 100.0f;
    n = (BYTE)sprintf((char*)buff, "%u", (WORD)value);
  }
  return n;
}

//---------------------------------------------------------
void MeasureDrawWait(void)
{
  LcdDrawBegin();
  LcdDrawWorkspace();
  LcdDrawCaLangText(LCD_W/2, LCD_H/3, 909);
  LcdDrawEnd();
}

//---------------------------------------------------------
void MeasureDrawDone(WORD y, BOOL done)
{
  TLcdRect r;
  WORD txt_w;
  BYTE n;
  WORD x, w, h;

  LcdSetBgColorDef();
  LcdSetFont(FONT10);

  n = LoadLangResource(LcdText, 729);
  txt_w = LcdGetTextWidth(LcdText, n);

  w = txt_w + 6U;
  x = LCD_W/2U - w/2U;
  h = LcdFontHeight + 4U;

  if(done == FALSE) {
    LcdSetFgColor(LcdGetBgColor());
    LcdDrawRect(x, y, w, h);
  }
  else {
    LcdSetLineThickness(1);
    LcdSetFgColor(MeasureDoneColor);
    LcdSetTextColor(MeasureDoneColor);
    r.Bottom = (r.Top = y + 1U) + h - 2U;
    r.Right = (r.Left = x + 1U) + w - 2U;
    LcdDrawCaRectText(&r, 1, LcdText, n);
    LcdDrawFrame(x, y, w, h);
  }
}

//---------------------------------------------------------
void MeasureSetSon(void)
{
  MeasureSetSon_pd();
}

//---------------------------------------------------------
void MeasureSetVon(void)
{
  MeasureSetVon_pd();
}

//---------------------------------------------------------
void MeasureSetAon(void)
{
  MeasureSetAon_pd();
}

//---------------------------------------------------------
void MeasureSetParamOn(TMeasureParam mp)
{
  switch(MeasureParam = mp) {
    case mpSpp:
      MeasureSetSon();
      break;
    case mpVrms:
      MeasureSetVon();
      break;
    case mpAamp:
      MeasureSetAon();
      break;
    default:;
  }
  MeasureFilterU.Started = FALSE;
  MeasureFilterF.Started = FALSE;
}

//---------------------------------------------------------
void MeasureCalc(void)
{
  WORD i;
  WORD cnt;
  WORD index;
  FLOAT32 tmp;
  FLOAT32 data[4];
  FLOAT32 measure_f;
  FLOAT32 measure_u_avr;
  FLOAT32 measure_u_amp;
  FLOAT32 measure_u_peak;
  FLOAT32 vist_sensor_k = VistSensorGetK(VistParamsObject);
  FLOAT32 k;

  // V
  cnt = MeasureVsqrSumICnt;
  if(cnt)
    cnt--;
  tmp = 0.0f;
  index = MeasureVsqrSumIPtr;
  if(index)
    index--;
  else
    index = MEASURE_V_SQR_SUM_SIZE - 1;
  for(i = 0; i < cnt; i++) {
    k = MEASUREAMPLIFIER_GET_KI(MeasureAdcAvrKIndArr[index]);
    tmp += (FLOAT32)MeasureAdcAvrSumArr[index]/(k*k);
    if(index)
      index--;
    else
      index = MEASURE_V_SQR_SUM_SIZE - 1;
  }
  if(cnt)
    tmp /= (cnt*MEASURE_V_SQR_SUM_COUNT_MAX);
  measure_u_avr = MeasureCalcUavr_pd(sqrt(tmp)*AdcGetK());
  switch(MeasureParam) {
    case mpVrms:
      measure_u_avr = MeasureFilterDo(&MeasureFilterU, measure_u_avr);
    default:;
  }
  MeasureUavr =  measure_u_avr;
  switch(MeasureParam) {
    case mpVrms:
      MeasureV = vist_sensor_k*
       (MeasuringTractAv[0] + MeasureUavr*MeasuringTractAv[1]);
    default:;
  }

  if(MeasureCalcT(data) == FALSE) {
    MeasureT = 0.0f;
    MeasureF = 0.0f;
    MeasureNoise = 0.0f;
    MeasureFilterF.Started = FALSE;
    switch(MeasureParam) {
      case mpSpp:
      case mpAamp:
        MeasureFilterU.Started = FALSE;
      default:;
    }
  }
  else {
    MeasureT       = data[0];
    MeasureNoise   = data[1];
    measure_u_peak = data[2];
    measure_u_amp  = data[3];

    switch(MeasureParam) {
      case mpSpp:
        measure_u_amp = MeasureFilterDo(&MeasureFilterU, measure_u_amp);
        break;
      case mpAamp:
        measure_u_peak = MeasureFilterDo(&MeasureFilterU, measure_u_peak);
      default:;
    }
    MeasureUamp  = measure_u_amp;
    MeasureU2amp = 2*measure_u_amp;
    MeasureUpeak = measure_u_peak;

    switch(MeasureParam) {
      case mpSpp:
        MeasureS = vist_sensor_k*
         (MeasuringTractAs[0] + MeasureU2amp*MeasuringTractAs[1]);
        break;
      case mpAamp:
        MeasureA = vist_sensor_k*
         (MeasuringTractAa[0] + MeasureUpeak*MeasuringTractAa[1]);
      default:
        ;
    }

    measure_f = MeasureCalcF(MeasureT);
    if(MeasureFilterFEnabled == TRUE) {
      MeasureF = MeasureFilterDo(&MeasureFilterF, measure_f);
    }
    else {
      MeasureF = measure_f;
    }
  }
}

//---------------------------------------------------------
static BOOL MeasureCalcT(FLOAT32* data)
{
  return MeasureCalcT_pd(data);
}

//---------------------------------------------------------
FLOAT32 MeasureCalcF(FLOAT32 t)
{
  if(t > 0.0f)
    return 1.0f/t;
  return 0.0f;
}

//---------------------------------------------------------
static void MeasureNumberToStr(BYTE* buff, WORD number)
{
  buff[0] = '\005';
  WordToStrWithLeadingZeroes(buff + 1, number, 3);
}

//---------------------------------------------------------
void MeasureDrawNumber(WORD number, BOOL redraw)
{
  MeasureNumberToStr(LcdText, number);
  MeasureNumberRect.Top = LCD_STATUS_H + 2;
  MeasureNumberRect.Right = LCD_W - 1;
  LcdSetTextColor(MeasureNumberColor);
  LcdSetFont(FONT10);
  LcdDrawRaAutoSizedRectText(&MeasureNumberRect, LcdText, 4, redraw);
}

//---------------------------------------------------------
static BYTE MeasureSVAtoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  BYTE n;
  BYTE comma;
  
  if(f == FALSE) {
    memcpy(buff, "?", n = 1);
  }
  else {
    comma = (BYTE)-1;

    if(value < 0.0f)
      value = 0.0f;
    
    if(value < 0.995f) 
      comma = 3;
    else if(value < 9.95f) 
      comma = 2;
    else if(value < 99.5f) 
      comma = 1;
    else if(value < 9999.5f)
      comma = 0;
  
    if(comma == (BYTE)-1)
      memset(buff, '*', n = 4);
    else
      n = FloatToStr(buff, value, comma);
  }
  
  return n;
}


//---------------------------------------------------------
BYTE MeasureStoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  return MeasureSVAtoStr(buff, value, f);
}

//---------------------------------------------------------
BYTE MeasureVtoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  return MeasureSVAtoStr(buff, value, f);
}

//---------------------------------------------------------
BYTE MeasureVtoStr2(BYTE* buff, FLOAT32 value, BOOL f)
{
  BYTE n;
  BYTE comma;

  if(f == FALSE) {
    memcpy(buff, "?", n = 1);
  }
  else {
    comma = (BYTE)-1;

    if(value < 0.0f)
      value = 0.0f;

    if(value <= 99.999f) {
      comma = 3;
    }
    else if(value <= 999.99f) {
      comma = 2;
    }
    else if(value <= 9999.9f) {
      comma = 1;
    }
    else {
      memset(buff, '*', n = 4);
    }

    if(comma != (BYTE)-1) {
      n = FloatToStr(buff, value, comma);
    }
  }

  return n;
}

//---------------------------------------------------------
BYTE MeasureAtoStr(BYTE* buff, FLOAT32 value, BOOL f)
{
  return MeasureSVAtoStr(buff, value, f);
}

//---------------------------------------------------------
void MeasureDrawCalendar(BOOL redraw)
{
  BYTE n;
  CalendarTimeToStr(LcdText, MeasureHour, MeasureMin, MeasureSec);
  memset(LcdText + (n = 8), ' ', 3);
  n += 3;
  CalendarDateToStr2(LcdText + n, MeasureDay, MeasureMon);
  n += 6;
  memset(LcdText + n, ' ', 1);
  n += 1;
  WordToStrWithLeadingSpaces(LcdText + n, MeasureYear + 2000, 4);
  n += 4;
  LcdSetTextColor(MeasureCalendarColor);
  LcdSetFont(FONT10);
  MeasureCalendarRect.Top = 254U;
  LcdDrawCaAutoSizedRectText(&MeasureCalendarRect, LCD_W/2U, LcdText, n, redraw);
}

//---------------------------------------------------------
void MeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw)
{
  BYTE n = LoadLangResource(LcdText, 233);
  n += MeasureFtoStr(LcdText + n, freq, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 657);

  MeasureFRect.Top = y;
  LcdSetTextColor(MeasureFColor);
  LcdSetFont(FONT20);
  LcdDrawCaAutoSizedRectText(&MeasureFRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void MeasureDrawS(WORD y, FLOAT32 s, BOOL f, BOOL redraw)
{
  BYTE n = LoadTextResource(LcdText, 661);
  n += MeasureStoStr(LcdText + n, s, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 176);

  MeasureVRect.Top = y;
  LcdSetTextColor(MeasureSColor);
  LcdSetFont(FONT16);
  LcdDrawCaAutoSizedRectText(&MeasureVRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void MeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw)
{
  BYTE n = LoadTextResource(LcdText, 662);
  n += MeasureVtoStr(LcdText + n, v, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 713);

  MeasureVRect.Top = y;
  LcdSetTextColor(MeasureVColor);
  LcdSetFont(FONT16);
  LcdDrawCaAutoSizedRectText(&MeasureVRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void MeasureDrawA(WORD y, FLOAT32 a, BOOL f, BOOL redraw)
{
  BYTE n = LoadTextResource(LcdText, 663);
  n += MeasureAtoStr(LcdText + n, a, f);
  LcdText[n++] = ' ';
  n += LoadLangResource(LcdText + n, 717);

  MeasureVRect.Top = y;
  LcdSetTextColor(MeasureAColor);
  LcdSetFont(FONT16);
  LcdDrawCaAutoSizedRectText(&MeasureVRect, LCD_W/2, LcdText, n, redraw);
}

//---------------------------------------------------------
void MeasureSetTime(void)
{
  MeasureHour = CalendarHour;
  MeasureMin  = CalendarMin;
  MeasureSec  = CalendarSec;
  MeasureDay  = CalendarDay;
  MeasureMon  = CalendarMon;
  MeasureYear = CalendarYear;
}

//---------------------------------------------------------
void MeasureGaugeCreate(WORD y)
{
  TLcdGauge* gauge;

  LcdGaugeCreate(gauge = &LcdGauge);
  gauge->Y   = y;
  gauge->Min = 0.0f;
  gauge->Max = 100.0f;
  gauge->Range = 1;
  gauge->RangeEnabled = FALSE;
  LcdGaugeSetup(gauge);
}

//---------------------------------------------------------
void MeasureGaugeSetValue(FLOAT32 value, BOOL redraw)
{
  LcdGaugeSetValue(NULL, value, TRUE);
}

//---------------------------------------------------------
void MeasureDrawUnits(WORD y, BYTE* txt, BYTE txt_len)
{
  WORD     w;
  TLcdRect rect;

  LcdSetFont(FONT10);
  LcdSetLineThickness(1);

  w = LcdGetTextWidth(txt, txt_len);

  rect.Left   = LCD_W/2 - 4 - w/2;
  rect.Right  = rect.Left + 4 + 4 + w;
  rect.Top    = y;
  rect.Bottom = y + 1 + 1 + LcdFontHeight;

  LcdSetFgColor(LCD_RGB_TO_COLOR(0xFF, 0x60, 0x60));
  LcdSetTextColor(LCD_RGB_TO_COLOR(0xFF, 0x60, 0x60));
  LcdDrawHorzLine(rect.Left, rect.Top,    rect.Right - rect.Left);
  LcdDrawHorzLine(rect.Left, rect.Bottom, rect.Right - rect.Left);
  LcdDrawVertLine(rect.Left,      rect.Top, rect.Bottom - rect.Top);
  LcdDrawVertLine(rect.Right - 1, rect.Top, rect.Bottom - rect.Top);

  rect.Left   += 2;
  rect.Right  -= 2;
  rect.Top    += 1;
  rect.Bottom -= 1;
  LcdDrawCaRectText(&rect, 0, txt, txt_len);
}

//---------------------------------------------------------
void MeasureFilterInit(TMeasureFilter *filter, FLOAT32 value)
{
  WORD i;
  FLOAT32 *data = filter->Data;
  for(i = 0; i < MEASURE_FILTER_DIMENSION; i++)
    *data++ = value;
  filter->Ticks = GetTicks();
  filter->Started = TRUE;
}

//---------------------------------------------------------
FLOAT32 MeasureFilterDo(TMeasureFilter* filter, FLOAT32 value)
{
#define I_MAX (MEASURE_FILTER_DIMENSION - 1)
  FLOAT32 result;
  WORD i;
  FLOAT32* data;

  if(filter->Started == FALSE) {
    MeasureFilterInit(filter, value);
    return value;
  }
  else {
    result = 0.0f;
    data = filter->Data;
    for(i = 0; i <= I_MAX; i++) {
      result += data[I_MAX - i]*MeasureFilterB[i];
      if(i == I_MAX)
        data[I_MAX] = value;
      else
        data[i] = data[i + 1];
    }
    return result/(MEASURE_FILTER_N*MEASURE_FILTER_N);
  }
#undef I_MAX
}

//---------------------------------------------------------
void MeasurePowerOn(void)
{
  MeasurePowerOn_pd(BIT0);
}

//---------------------------------------------------------
void MeasurePowerOff(void)
{
  MeasurePowerOff_pd(BIT0);
}

//---------------------------------------------------------
void MeasureFilterFOn(void)
{
  MeasureFilterFEnabled = TRUE;
}

//---------------------------------------------------------
void MeasureFilterFOff(void)
{
  MeasureFilterFEnabled = FALSE;
}

