/*---------------------------------------------------------
  calendar_pd.c
---------------------------------------------------------*/

#include "calendar_pd.h"
#include "calendar.h"
#include "utils.h"

#define CALENDAR_ENABLE_WRITE() \
  do { \
    RTC->WPR = 0xCA; \
    RTC->WPR = 0x53; \
  } while(0); \
  RTC->ISR |= (1U << 7U); \
  while((RTC->ISR & (1U << 6U)) == 0)
    
#define CALENDAR_DISABLE_WRITE() \
  RTC->ISR &= ~(DWORD)(1U << 7U)
    

static const BYTE CalendarDays[] = {
  31, 28, 31, 30, 31, 30,
  31, 31, 30, 31, 30, 31
};

static void CalendarReadDateRegs(DWORD* regs);

//---------------------------------------------------------
void SetupCalendar_pd(void)
{
}

//---------------------------------------------------------
void ReadCalendar_pd(void)
{
  DWORD tr;
  DWORD dr;
  DWORD regs[2];
  
  CalendarReadDateRegs(regs);
  tr = regs[0];
  dr = regs[1];
  
  CalendarSec = BcdToDec(tr & 0x7FU);
  CalendarMin = BcdToDec((tr >> 8U) & 0x7FU);
  CalendarHour = BcdToDec((tr >> 16U) & 0x3FU);
  CalendarDay = BcdToDec(dr & 0x3FU);
  CalendarMon = BcdToDec((dr >> 8U) & 0x1FU);
  CalendarYear = BcdToDec((dr >> 16U) & 0xFFU);
}

//---------------------------------------------------------
void WriteCalendar_pd(void)
{
  DWORD tr;
  DWORD dr;
  DWORD regs[2];
  BYTE last_day;
  BYTE day;
  
  CalendarReadDateRegs(regs);
  
  tr = regs[0];
  dr = regs[1];
  
  last_day = CalendarDays[CalendarMon - 1U];
  if(CalendarMon == 2U)
    if((CalendarYear % 4U) == 0)
      last_day++;
  day = last_day < CalendarDay ? last_day : CalendarDay;
  
  tr &= ~(
   ((DWORD)0x3FU << 16U) |
   ((DWORD)0x7FU << 8U)  |
   ((DWORD)0x7FU));
  tr |= 
   (((DWORD)DecToBcd(CalendarHour) & 0x3FU) << 16U) |
   (((DWORD)DecToBcd(CalendarMin)  & 0x7FU) << 8U)  |
   (((DWORD)DecToBcd(CalendarSec)  & 0x7FU));
  dr &= ~(
   ((DWORD)0xFFU << 16U) |
   ((DWORD)0x1FU << 8U)  |
   ((DWORD)0x3FU));
  dr |= 
   (((DWORD)DecToBcd(CalendarYear) & 0xFFU) << 16U) |
   (((DWORD)DecToBcd(CalendarMon)  & 0x1FU) << 8U)  |
   (((DWORD)DecToBcd(day)          & 0x3FU));
  
  CALENDAR_ENABLE_WRITE();
  
  RTC->TR = tr;
  RTC->DR = dr;
  
  CALENDAR_DISABLE_WRITE();
}

//---------------------------------------------------------
static void CalendarReadDateRegs(DWORD* regs)
{
  DWORD tr;
  DWORD dr;
  
  tr = RTC->TR;
  if(tr != RTC->TR)
    tr = RTC->TR;
  dr = RTC->DR;
  if(dr != RTC->DR)
    dr = RTC->DR;
  
  regs[0] = tr;
  regs[1] = dr;
}
