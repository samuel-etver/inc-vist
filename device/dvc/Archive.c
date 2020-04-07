/*---------------------------------------------------------
  Archive.c
---------------------------------------------------------*/

#include "Archive.h"
#include "lcd.h"
#include "keybrd.h"
#include "calendar.h"
#include "mem.h"

NOINIT BOOL ArchiveRedraw;

//---------------------------------------------------------
void ShowArchiveIni(void)
{
  MemLocateLast();
  MeasureSetColors();
  ArchiveRedraw = TRUE;
}

//---------------------------------------------------------
void ShowArchive(void)
{
  ProcessStandardKeyDownActions();
}

//---------------------------------------------------------
BOOL ArchiveCheck(void)
{
  return TO_BOOL(!MemFlags.Empty);
}

//---------------------------------------------------------
void ArchiveNoDataIni(void)
{
  LcdDrawBegin();

  LcdDrawWorkspace();

  LcdSetFont(LCD_CAPTION_FONT);
  LcdDrawCaLangText(LCD_W/2, 100, 307);

  LcdDrawEnd();
}

//---------------------------------------------------------
void ArchiveNoData(void)
{
}

//---------------------------------------------------------
void ArchiveDataIni(void)
{
  LcdDrawBegin();

  if(ArchiveRedraw == TRUE) {
    LcdDrawWorkspace();
    LcdDrawLangHint(773, 0, 0);
  }

  ArchiveDrawNumber();
  ArchiveDrawCalendar();

  LcdDrawEnd();
}

//---------------------------------------------------------
BOOL ArchiveData(void)
{
  switch(KeyDown) {
    case KB_LEFT:
      MemLocatePrev(); break;
    case KB_RIGHT:
      MemLocateNext(); break;
    case KB_UP:
      MemLocatePrevDate(); break;
    case KB_DOWN:
      MemLocateNextDate(); break;
    case KB_F1:
      MemDelCell(); break;
    default:
      return FALSE;
  }

  return TRUE;
}

//---------------------------------------------------------
void ArchiveDrawCalendar(void)
{
  MeasureDay = MemCellDate[0];
  MeasureMon = MemCellDate[1];
  MeasureYear = MemCellDate[2];
  MeasureHour = MemCellTime[0];
  MeasureMin = MemCellTime[1];
  MeasureSec = MemCellTime[2];
  MeasureDrawCalendar(ArchiveRedraw);
}

//---------------------------------------------------------
void ArchiveDrawNumber(void)
{
  MeasureDrawNumber(MemCellNumber, ArchiveRedraw);
}
