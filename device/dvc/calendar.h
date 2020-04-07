#ifndef CALENDAR_H_INCLUDED
#define CALENDAR_H_INCLUDED

/*---------------------------------------------------------
  calendar.h
---------------------------------------------------------*/

#include "xmain.h"

extern NOINIT BYTE CalendarHour;
extern NOINIT BYTE CalendarMin;
extern NOINIT BYTE CalendarSec;
extern NOINIT BYTE CalendarYear;
extern NOINIT BYTE CalendarMon;
extern NOINIT BYTE CalendarDay;

void InitCalendar(void);
void SetupCalendar(void);
void ShowCalendarIni(void);
void ShowCalendar(void);

void ReadCalendar(void);
void WriteCalendar(void);

void CalendarTimeToStr(BYTE* buff, BYTE hour, BYTE min, BYTE sec);
void CalendarDateToStr(BYTE* buff, BYTE day, BYTE mon, BYTE year);
void CalendarTimeToStr2(BYTE* buff, BYTE hour, BYTE min);
void CalendarDateToStr2(BYTE* buff, BYTE day, BYTE mon);

#endif
