#ifndef MEASURE_H_INCLUDED
#define MEASURE_H_INCLUDED

/*---------------------------------------------------------
  measure.h
---------------------------------------------------------*/

#include "xmain.h"
#include "delay.h"
#include "utils.h"
#include "lcd.h"
#include "adc.h"

#define MEASURE_PERIOD_SIZE                 100U

#define MEASURE_SIGNAL_OVERLOAD_LOW         ((WORD)(ADC_CODE_MAX/2*0.05))
#define MEASURE_SIGNAL_OVERLOAD_HIGH        ((WORD)(ADC_CODE_MAX - MEASURE_SIGNAL_OVERLOAD_LOW))
#define MEASURE_SIGNAL_LOW_LOW              ((WORD)(ADC_CODE_MAX/2 - ADC_CODE_MAX/2*0.3))
#define MEASURE_SIGNAL_LOW_HIGH             ((WORD)(ADC_CODE_MAX/2 + ADC_CODE_MAX/2*0.3))
#define MEASURE_SIGNAL_SIZE                 100U
#define MEASURE_SIGNAL_COUNT                800U

#define MEASURE_PERIOD_TICK_IN_MCS          25.0f
#define MEASURE_PERIOD_COUNT_MIN            8U
#define MEASURE_PERIOD_COUNT_MAX            50U

#define MEASURE_V_SQR_SUM_SIZE              10U
#define MEASURE_V_SQR_SUM_COUNT_MAX         1000U

#define MEASURE_PERIOD_DOWN_COUNT_MAX       10

typedef enum {
  mpSpp = 0,
  mpVrms,
  mpAamp,
  mpLast, mpNone
} TMeasureParam;
#define MEASURE_PARAMS_COUNT    ((BYTE)mpLast)

void InitMeasure(void);
void ShowMeasureIni(void);
void ShowMeasure(void);
void MeasureOnEnter(void);
void MeasureOnExit(void);
void MeasureLoadNumber(void);
void MeasureReset(void);
void MeasureMeasure(void);
BYTE MeasureFtoStr(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureTtoStr(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureNoiseToStr(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureStoStr(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureVtoStr(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureVtoStr2(BYTE* buff, FLOAT32 value, BOOL f);
BYTE MeasureAtoStr(BYTE* buff, FLOAT32 value, BOOL f);
void MeasureDrawNumber(WORD number, BOOL redraw);
void MeasureDrawDone(WORD y, BOOL done);
void MeasureDrawWait(void);
void MeasureSetSon(void);
void MeasureSetVon(void);
void MeasureSetAon(void);
void MeasureSetParamOn(TMeasureParam mp);
void MeasureCalc(void);
FLOAT32 MeasureCalcF(FLOAT32 t);
void MeasureGaugeCreate(WORD y);
void MeasureGaugeSetValue(FLOAT32 value, BOOL redraw);
void MeasureDrawCalendar(BOOL redraw);
void MeasureDrawF(WORD y, FLOAT32 freq, BOOL f, BOOL redraw);
void MeasureDrawS(WORD y, FLOAT32 s, BOOL f, BOOL redraw);
void MeasureDrawV(WORD y, FLOAT32 v, BOOL f, BOOL redraw);
void MeasureDrawA(WORD y, FLOAT32 a, BOOL f, BOOL redraw);
void MeasureSetColors(void);
void MeasureSetTime(void);
void MeasureDrawUnits(WORD y, BYTE* txt, BYTE txt_len);
void MeasurePowerOn(void);
void MeasurePowerOff(void);
void MeasureFilterFOn(void);
void MeasureFilterFOff(void);


typedef enum {
  MV_INC_INI,
  MV_INC,
  MV_VIST_INI,
  MV_VIST
} TMeasureVerb;

typedef struct {
  BIT MeasureEntered: 1;
  BIT Measuring:      1;
  BIT Result:         1;
} TMeasureFlags;

extern NOINIT TMeasureVerb MeasureVerb;
extern NOINIT TMeasureFlags MeasureFlags;
extern NOINIT WORD MeasureNumber;
extern NOINIT BYTE MeasureHour;
extern NOINIT BYTE MeasureMin;
extern NOINIT BYTE MeasureSec;
extern NOINIT BYTE MeasureDay;
extern NOINIT BYTE MeasureMon;
extern NOINIT BYTE MeasureYear;
extern NOINIT WORD MeasureAdc;
extern NOINIT WORD MeasureAdcX;
extern NOINIT BYTE MeasurePeriodPlus;
extern NOINIT DWORD MeasurePeriodArr[MEASURE_PERIOD_SIZE];
extern NOINIT WORD MeasurePeriodIndexArr[MEASURE_PERIOD_COUNT_MAX];
extern NOINIT WORD MeasurePeriodPtr;
extern NOINIT WORD MeasurePeriodPlusTicks;
extern NOINIT WORD MeasurePeriodMinusTicks;
extern NOINIT WORD MeasurePeriodAdcMax;
extern NOINIT WORD MeasurePeriodAdcMaxX;
extern NOINIT WORD MeasurePeriodAdcPeakArr[MEASURE_PERIOD_SIZE];
extern NOINIT BYTE MeasurePeriodKIndArr[MEASURE_PERIOD_SIZE];
extern NOINIT BOOL MeasurePeriodAvailableTmp;
extern NOINIT WORD MeasurePeriodAvailableTmpCnt;
extern NOINIT BOOL MeasurePeriodAvailable;
extern NOINIT FLOAT32 MeasureT;
extern NOINIT FLOAT32 MeasureF;
extern NOINIT FLOAT32 MeasureNoise;
extern NOINIT FLOAT32 MeasureUavr;
extern NOINIT FLOAT32 MeasureUamp;
extern NOINIT FLOAT32 MeasureU2amp;
extern NOINIT FLOAT32 MeasureUpeak;
extern NOINIT FLOAT32 MeasureS;
extern NOINIT FLOAT32 MeasureV;
extern NOINIT FLOAT32 MeasureA;
extern NOINIT FLOAT32 MeasureTResult;
extern NOINIT FLOAT32 MeasureFResult;
extern NOINIT FLOAT32 MeasureNoiseResult;
extern NOINIT FLOAT32 MeasureSResult;
extern NOINIT FLOAT32 MeasureVResult;
extern NOINIT FLOAT32 MeasureAResult;
extern NOINIT FLOAT32 MeasureSignalLevel;
extern NOINIT WORD MeasureSignalOverloadCnt;
extern NOINIT TDelay MeasureSignalOverloadDelay;
extern NOINIT BOOL MeasureSignalOverloadSkip;
extern NOINIT TDelay MeasureSignalLowLevelDelay;
extern NOINIT BOOL MeasureSignalLowLevelNotFound;
extern NOINIT WORD MeasureAdcValueZero;
extern NOINIT WORD MeasureAdcValueMin;
extern NOINIT WORD MeasureAdcValueMax;
extern NOINIT BYTE MeasureAdcValueMinMaxKInd;
extern NOINIT WORD MeasureAdcValueMinTmp;
extern NOINIT WORD MeasureAdcValueMaxTmp;
extern NOINIT BYTE MeasureAdcValueMinMaxKIndTmp;
extern NOINIT TDelay MeasureAdcValueMinMaxDelay;
extern NOINIT TLcdRect MeasureNumberRect;
extern NOINIT TLcdRect MeasureObjectRect;
extern NOINIT TLcdRect MeasureCalendarRect;
extern NOINIT TLcdRect MeasureTRect;
extern NOINIT TLcdRect MeasureFRect;
extern NOINIT TLcdRect MeasureNoiseRect;
extern NOINIT TLcdRect MeasureVRect;
extern NOINIT TTrigger MeasureNumberTrigger;
extern NOINIT TTrigger MeasureTTrigger;
extern NOINIT TTrigger MeasureFTrigger;
extern NOINIT TTrigger MeasureNoiseTrigger;
extern NOINIT TTrigger MeasureSTrigger;
extern NOINIT TTrigger MeasureVTrigger;
extern NOINIT TTrigger MeasureATrigger;
extern NOINIT TTrigger MeasureTrigger1;
extern NOINIT TTrigger MeasureTrigger2;
extern NOINIT TTrigger MeasureTrigger3;
extern NOINIT TColor MeasureNumberColor;
extern NOINIT TColor MeasureCalendarColor;
extern NOINIT TColor MeasureFColor;
extern NOINIT TColor MeasureSColor;
extern NOINIT TColor MeasureVColor;
extern NOINIT TColor MeasureAColor;
extern NOINIT TColor MeasureSigmaColor;
extern NOINIT TColor MeasureNoiseColor;
extern NOINIT TColor MeasureDoneColor;
extern NOINIT TColor MeasureErrorMessageColor;
extern NOINIT DWORD MeasureAdcAvrSumArr[MEASURE_V_SQR_SUM_SIZE];
extern NOINIT BYTE MeasureAdcAvrKIndArr[MEASURE_V_SQR_SUM_SIZE];
extern NOINIT DWORD MeasureVsqrSum;
extern NOINIT WORD MeasureVsqrSumCnt;
extern NOINIT WORD MeasureVsqrSumIPtr;
extern NOINIT WORD MeasureVsqrSumICnt;
extern NOINIT BOOL MeasureAutoAmplifierEnabled;
extern NOINIT BYTE MeasureAmplifierStage;
extern NOINIT DWORD MeasureAmplifierTicks;
extern NOINIT TMeasureParam MeasureParam;

typedef struct {
  FLOAT32       *Data;
  DWORD         Ticks;
  BOOL          Started;
} TMeasureFilter;

void MeasureFilterInit(TMeasureFilter *filter, FLOAT32 value);
FLOAT32 MeasureFilterDo(TMeasureFilter* filter, FLOAT32 value);

#endif
