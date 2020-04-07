/*---------------------------------------------------------
  calendar.c
---------------------------------------------------------*/

#include "calendar.h"
#include "calendar_pd.h"
#include "Lcd.h"
#include "keybrd.h"
#include "language.h"
#include "utils.h"

#define CALENDAR_DATE_X             (LCD_W/2 - 20*8/2)
#define CALENDAR_TIME_X             CALENDAR_DATE_X
#define CALENDAR_DATE_Y             210
#define CALENDAR_TIME_Y             100

static NOINIT enum {
  CV_VIEW_INI,
  CV_VIEW,
  CV_EDIT_INI,
  CV_EDIT
} CalendarVerb;

NOINIT BYTE CalendarHour;
NOINIT BYTE CalendarMin;
NOINIT BYTE CalendarSec;
NOINIT BYTE CalendarYear;
NOINIT BYTE CalendarMon;
NOINIT BYTE CalendarDay;
static NOINIT BYTE CalendarPos;
static NOINIT BYTE CalendarSavedHour;
static NOINIT BYTE CalendarSavedMin;
static NOINIT BYTE CalendarSavedSec;
static NOINIT BYTE CalendarSavedDay;
static NOINIT BYTE CalendarSavedMon;
static NOINIT BYTE CalendarSavedYear;

static void ShowViewIni(void);
static void ShowView(void);
static void ShowEditIni(void);
static void ShowEdit(void);

static WORD CalendarGetSpinEditX(void);
static WORD CalendarGetSpinEditY(void);
static void CalendarDrawTime(BYTE pos);
static void CalendarDrawDate(BYTE pos);

static void CalendarDrawLabels(void);
static void CalendarGetEdit(void);
static void CalendarSetEdit(void);

//---------------------------------------------------------
void InitCalendar(void)
{
  CalendarPos  = 0;
  CalendarHour = 0;
  CalendarMin  = 0;
  CalendarSec  = 0;
  CalendarYear = 0;
  CalendarMon  = 1;
  CalendarDay  = 1;
}

//---------------------------------------------------------
void SetupCalendar(void)
{
  SetupCalendar_pd();
}

//---------------------------------------------------------
void ShowCalendarIni(void)
{
  PrgVerb = VB_CALENDAR;
  CalendarVerb = CV_VIEW_INI;
}

//---------------------------------------------------------
void ShowCalendar(void)
{
  ProcessStandardKeyDownActions();

  switch(CalendarVerb) {
    case CV_VIEW_INI:
      ShowViewIni();
    case CV_VIEW:
      ShowView(); break;
    case CV_EDIT_INI:
      ShowEditIni();
    case CV_EDIT:
      ShowEdit();
  }
}

//---------------------------------------------------------
static void ShowViewIni(void)
{
  CalendarVerb = CV_VIEW;

  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdDrawLangHint(953, 0, 0);
  CalendarDrawLabels();

  CalendarSavedSec = 0xFF;

  TempByte = LcdGetTextWidth((BYTE*)"0", 1) + 3;

  LcdDrawEnd();
}

//---------------------------------------------------------
static void ShowView(void)
{
  if(KeyDown == KB_F1)
    CalendarVerb = CV_EDIT_INI;

  if(CalendarSec != CalendarSavedSec) {
    CalendarSavedHour = CalendarHour;
    CalendarSavedMin  = CalendarMin;
    CalendarSavedSec  = CalendarSec;
    CalendarSavedDay  = CalendarDay;
    CalendarSavedMon  = CalendarMon;
    CalendarSavedYear = CalendarYear;

    LcdDrawBegin();

    CalendarDrawTime(0xFF);
    CalendarDrawDate(0xFF);

    LcdDrawEnd();
  }
}

//---------------------------------------------------------
static void ShowEditIni(void)
{
  CalendarVerb = CV_EDIT;

  CalendarSavedHour = CalendarHour;
  CalendarSavedMin  = CalendarMin;
  CalendarSavedSec  = CalendarSec;
  CalendarSavedDay  = CalendarDay;
  CalendarSavedMon  = CalendarMon;
  CalendarSavedYear = CalendarYear;

  LcdDrawBegin();

  LcdDrawWorkspace();

  CalendarDrawLabels();

  LcdSetLetterSpacing(3);
  LcdSpinEditCreate(NULL);
  LcdSpinEdit.W = TempByte*2;
  LcdSpinEdit.X = CalendarGetSpinEditX();
  LcdSpinEdit.Y = CalendarGetSpinEditY();
  LcdSpinEdit.TextLen = 2;
  LcdSpinEdit.Wrap = TRUE;
  CalendarSetEdit();
  LcdSpinEditSetup(NULL);
  LcdSpinEditDraw(NULL);

  CalendarDrawTime(CalendarPos);
  CalendarDrawDate(CalendarPos);

  LcdDrawLangHint(955, 0, 0);

  LcdDrawEnd();

  TempBool = FALSE;
}

//---------------------------------------------------------
static void ShowEdit(void)
{
  if(KeyDown == KB_MENU || KeyDown == KB_MEASURE) {
    CalendarGetEdit();
    CalendarHour = CalendarSavedHour;
    CalendarMin  = CalendarSavedMin;
    CalendarSec  = CalendarSavedSec;
    CalendarDay  = CalendarSavedDay;
    CalendarMon  = CalendarSavedMon;
    CalendarYear = CalendarSavedYear;
    WriteCalendar();
  }

  if(KeyDown == KB_F1)
    CalendarVerb = CV_VIEW_INI;

  if(KeyDown == KB_LEFT) {
    CalendarGetEdit();
    if(CalendarPos-- == 0)
      CalendarPos = 5;
    TempBool = TRUE;
  }

  if(KeyDown == KB_RIGHT) {
    CalendarGetEdit();
    if(++CalendarPos == 6)
      CalendarPos = 0;
    TempBool = TRUE;
  }

  if(KeyDown == KB_UP || KeyDown == KB_DOWN) {
    CalendarGetEdit();
    CalendarPos = (CalendarPos + 3)%6;
    TempBool = TRUE;
  }

  if(TempBool == TRUE) {
    TempBool = FALSE;

    CalendarSetEdit();

    LcdDrawBegin();

    LcdSpinEditUnmap(NULL);
    CalendarDrawTime(CalendarPos);
    CalendarDrawDate(CalendarPos);
    LcdSpinEdit.X = CalendarGetSpinEditX();
    LcdSpinEdit.Y = CalendarGetSpinEditY();
    LcdSpinEditSetup(NULL);
    LcdSpinEditDraw(NULL);

    LcdDrawEnd();
  }


  LcdSpinEditDo(NULL);
}

//---------------------------------------------------------
static void CalendarGetEdit(void)
{
  BYTE val = (BYTE)LcdSpinEdit.Value;

  switch(CalendarPos) {
    case 0: CalendarSavedHour = val; break;
    case 1: CalendarSavedMin = val;  break;
    case 2: CalendarSavedSec = val;  break;
    case 5: CalendarSavedYear = val;
  }

  if(LangId == lRussian) {
    switch(CalendarPos) {
      case 3: CalendarSavedDay = val;  break;
      case 4: CalendarSavedMon = val;
    }
  }
  else {
    switch(CalendarPos) {
      case 3: CalendarSavedMon = val;  break;
      case 4: CalendarSavedDay = val;
    }
  }
}

//---------------------------------------------------------
static void CalendarSetEdit(void)
{
  WORD max = 0;
  WORD min = 0;
  BYTE val = 0;

  switch(CalendarPos) {
    case 0: val = CalendarSavedHour; max = 23;          break;
    case 1: val = CalendarSavedMin;  max = 59;          break;
    case 2: val = CalendarSavedSec;  max = 59;          break;
    case 5: val = CalendarSavedYear; max = 99;
  }

  if(LangId == lRussian) {
    switch(CalendarPos) {
      case 3:  val = CalendarSavedDay;  max = 31;          break;
      case 4:  val = CalendarSavedMon;  max = 12; min = 1;
    }
  }
  else {
    switch(CalendarPos) {
      case 3:  val = CalendarSavedMon;  max = 12; min = 1; break;
      case 4:  val = CalendarSavedDay;  max = 31;
    }
  }

  LcdSpinEdit.Value = val;
  LcdSpinEdit.Min   = min;
  LcdSpinEdit.Max   = max;
}

//---------------------------------------------------------
static WORD CalendarGetSpinEditX(void)
{
  return CALENDAR_TIME_X + (CalendarPos%3)*TempByte*3;
}

//---------------------------------------------------------
static WORD CalendarGetSpinEditY(void)
{
  return (CalendarPos < 3 ? CALENDAR_TIME_Y : CALENDAR_DATE_Y) -
   LCD_LABEL_TO_SPIN_EDIT_OFFS;
}

//---------------------------------------------------------
static void CalendarDrawTime(BYTE pos)
{
  TLcdRect rect;
  WORD     i;

  LcdSetFont(LCD_ITEM_FONT);
  LcdSetLetterSpacing(3);
  LcdSetTextColorDef();

  CalendarTimeToStr(LcdText, CalendarSavedHour, CalendarSavedMin,
   CalendarSavedSec);

  rect.Left   = CALENDAR_TIME_X;
  rect.Bottom = (rect.Top = CALENDAR_TIME_Y) + LcdFontHeight;

  for(i = 0; i < 8; i++) {
    rect.Right = rect.Left + TempByte;
    if((i == 0 && pos == 0) ||
       (i == 1 && pos == 0) ||
       (i == 3 && pos == 1) ||
       (i == 4 && pos == 1) ||
       (i == 6 && pos == 2) ||
       (i == 7 && pos == 2)) {
    }
    else
      LcdDrawCaRectText(&rect, 0, LcdText + i, 1);
    rect.Left = rect.Right;
  }
}

//---------------------------------------------------------
static void CalendarDrawDate(BYTE pos)
{
  TLcdRect rect;
  WORD     i;

  CalendarDateToStr(LcdText, CalendarSavedDay, CalendarSavedMon,
   CalendarSavedYear);

  rect.Left = CALENDAR_DATE_X;
  rect.Bottom = (rect.Top = CALENDAR_DATE_Y) + LcdFontHeight;

  for(i = 0; i < 8; i++) {
    rect.Right = rect.Left + TempByte;
    if((i == 0 && pos == 3) ||
       (i == 1 && pos == 3) ||
       (i == 3 && pos == 4) ||
       (i == 4 && pos == 4) ||
       (i == 6 && pos == 5) ||
       (i == 7 && pos == 5)) {
    }
    else
      LcdDrawCaRectText(&rect, 0, LcdText + i, 1);
    rect.Left = rect.Right;
  }
}

//---------------------------------------------------------
void ReadCalendar(void)
{
  ReadCalendar_pd();
}

//---------------------------------------------------------
void WriteCalendar(void)
{
  WriteCalendar_pd();
}

//---------------------------------------------------------
void CalendarDateToStr(BYTE* buff, BYTE day, BYTE mon, BYTE year)
{
  WORD a0;
  WORD a1;
  WORD a2;
  BYTE sep;

  if(LangId == lRussian) {
    a0 = day;
    a1 = mon;
    a2 = year;
    sep = '.';
  }
  else {
    a0 = mon;
    a1 = day;
    a2 = year;
    sep = '-';
  }

  WordToStrWithLeadingZeroes(buff + 6, a2, 2);
  WordToStrWithLeadingZeroes(buff + 3, a1, 2);
  WordToStrWithLeadingZeroes(buff,     a0, 2);
  buff[5] = buff[2] = sep;
}

//---------------------------------------------------------
void CalendarTimeToStr(BYTE* buff, BYTE hour, BYTE min, BYTE sec)
{
  WordToStrWithLeadingZeroes(buff + 6, sec,  2);
  WordToStrWithLeadingZeroes(buff + 3, min,  2);
  WordToStrWithLeadingZeroes(buff,     hour, 2);
  buff[5] = buff[2] = ':';
}

//---------------------------------------------------------
void CalendarTimeToStr2(BYTE* buff, BYTE hour, BYTE min)
{
  buff[2] = ':';
  WordToStrWithLeadingZeroes(buff,     hour, 2);
  WordToStrWithLeadingZeroes(buff + 3, min,  2);
}

//---------------------------------------------------------
void CalendarDateToStr2(BYTE* buff, BYTE day, BYTE mon)
{
  if(LangId == lRussian) {
    memset(buff + 2, ' ', 4);
    WordToStrWithLeadingSpaces(buff, day, 2);
    if(mon >= 1 && mon <= 12)
      LoadLangResource(buff + 3, 186 + LANG_COUNT*(mon - 1));
  }
  else {
    memset(buff, ' ', 4);
    if(mon >= 1 && mon <= 12)
      LoadLangResource(buff, 186 + LANG_COUNT*(mon - 1));
    WordToStrWithLeadingSpaces(buff + 4, day, 2);
  }
}

//---------------------------------------------------------
static void CalendarDrawLabels(void)
{
  LcdSetFont(LCD_ITEM_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdDrawCaLangText(LCD_W/2, CALENDAR_TIME_Y - 46, 182);
  LcdDrawCaLangText(LCD_W/2, CALENDAR_DATE_Y - 46, 184);
}
