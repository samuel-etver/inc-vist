/*---------------------------------------------------------
  calendar_pd.c
---------------------------------------------------------*/

#include "calendar.h"
#include "calendar_pd.h"

//---------------------------------------------------------
void SetupCalendar_pd(void)
{
}

//---------------------------------------------------------
void ReadCalendar_pd(void)
{
  SYSTEMTIME tm;

  GetLocalTime(&tm);

  CalendarHour = tm.wHour;
  CalendarMin  = tm.wMinute;
  CalendarSec  = tm.wSecond;
  CalendarDay  = tm.wDay;
  CalendarMon  = tm.wMonth;
  CalendarYear = tm.wYear%100;
}

//---------------------------------------------------------
void WriteCalendar_pd(void)
{
}
