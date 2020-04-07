/*---------------------------------------------------------
  IncArchive.c
---------------------------------------------------------*/

#include "IncArchive.h"
#include "IncMeasure.h"
#include "IncMeasureMode.h"
#include "VistArchive.h"
#include "mem.h"
#include "lcd.h"
#include "keybrd.h"
#include "archive.h"

static NOINIT enum {
  IAV_CHECK,
  IAV_NODATA_INI,
  IAV_NODATA,
  IAV_DATA_INI,
  IAV_DATA
} IncArchiveVerb;

static BYTE IncArchiveMeasureMode;

static void IncArchiveCheck(void);
static void IncArchiveNoDataIni(void);
static void IncArchiveNoData(void);
static void IncArchiveDataIni(void);
static void IncArchiveData(void);
static void IncArchiveDataDraw(void);

//---------------------------------------------------------
void ShowIncArchiveIni(void)
{
  PrgVerb = VB_INC_ARCHIVE;
  IncArchiveVerb = IAV_CHECK;
  ShowArchiveIni();
}

//---------------------------------------------------------
void ShowIncArchive(void)
{
  ShowArchive();

  switch(IncArchiveVerb) {
    case IAV_CHECK:
      IncArchiveCheck();
      break;
    case IAV_NODATA_INI:
      IncArchiveNoDataIni();
    case IAV_NODATA:
      IncArchiveNoData();
      break;
    case IAV_DATA_INI:
      IncArchiveDataIni();
    case IAV_DATA:
      IncArchiveData();
  }
}

//---------------------------------------------------------
static void IncArchiveCheck(void)
{
  IncArchiveVerb = IAV_NODATA_INI;
  if(ArchiveCheck() == TRUE)
    IncArchiveVerb = IAV_DATA_INI;
}

//---------------------------------------------------------
static void IncArchiveNoDataIni(void)
{
  IncArchiveVerb = IAV_NODATA;
  ArchiveNoDataIni();
}

//---------------------------------------------------------
static void IncArchiveNoData(void)
{
  ArchiveNoData();
}

//---------------------------------------------------------
static void IncArchiveDataIni(void)
{
  IncArchiveVerb = IAV_DATA;
  if(MemCellMeasureMode != IncArchiveMeasureMode) {
    IncArchiveMeasureMode = MemCellMeasureMode;
    ArchiveRedraw = TRUE;
  }
  ArchiveDataIni();
  IncArchiveDataDraw();
}

//---------------------------------------------------------
static void IncArchiveData(void)
{
  if(ArchiveData() == TRUE)
    IncArchiveVerb = IAV_CHECK;
}

//---------------------------------------------------------
static void IncArchiveDataDraw(void)
{
  if(MemCellMeasureMode == incMmVist) {
    VistArchiveDataDraw();
  }
  else {
    LcdDrawBegin();

    IncMeasureDrawLength(MemCellIncLength, ArchiveRedraw);
    IncMeasureDrawDiameter(MemCellIncDiameter, ArchiveRedraw);

    IncMeasureDrawF(74, MemCellF, TRUE, ArchiveRedraw);
    IncMeasureDrawSigma(115, MemCellIncSigma, TRUE, ArchiveRedraw);
    IncMeasureDrawUnits(155);
    IncMeasureDrawDeltaL(190, MemCellIncDeltaL, TRUE, ArchiveRedraw);
    IncMeasureDrawEpsilon(220, MemCellIncEpsilon, TRUE, ArchiveRedraw);

    ArchiveDrawCalendar();

    LcdDrawEnd();
  }

  ArchiveRedraw = FALSE;
}
