/*---------------------------------------------------------
  VistArchive.c
---------------------------------------------------------*/

#include "VistArchive.h"
#include "VistMeasure.h"
#include "VistParams.h"
#include "lcd.h"
#include "keybrd.h"
#include "archive.h"
#include "measure.h"
#include "mem.h"

static NOINIT enum {
  VAV_CHECK,
  VAV_NODATA_INI,
  VAV_NODATA,
  VAV_DATA_INI,
  VAV_DATA
} VistArchiveVerb;

static void VistArchiveCheck(void);
static void VistArchiveNoDataIni(void);
static void VistArchiveNoData(void);
static void VistArchiveDataIni(void);
static void VistArchiveData(void);

//---------------------------------------------------------
void ShowVistArchiveIni(void)
{
  PrgVerb = VB_VIST_ARCHIVE;
  VistArchiveVerb = VAV_CHECK;
  ShowArchiveIni();
}

//---------------------------------------------------------
void ShowVistArchive(void)
{
  ShowArchive();

  switch(VistArchiveVerb) {
    case VAV_CHECK:
      VistArchiveCheck();
      break;
    case VAV_NODATA_INI:
      VistArchiveNoDataIni();
    case VAV_NODATA:
      VistArchiveNoData();
      break;
    case VAV_DATA_INI:
      VistArchiveDataIni();
    case VAV_DATA:
      VistArchiveData();
  }
}

//---------------------------------------------------------
static void VistArchiveCheck(void)
{
  VistArchiveVerb = VAV_NODATA_INI;
  if(ArchiveCheck() == TRUE)
    VistArchiveVerb = VAV_DATA_INI;
}

//---------------------------------------------------------
static void VistArchiveNoDataIni(void)
{
  VistArchiveVerb = VAV_NODATA;
  ArchiveNoDataIni();
}

//---------------------------------------------------------
static void VistArchiveNoData(void)
{
  ArchiveNoData();
}

//---------------------------------------------------------
static void VistArchiveDataIni(void)
{
  VistArchiveVerb = VAV_DATA;
  ArchiveDataIni();
  VistArchiveDataDraw();
}

//---------------------------------------------------------
static void VistArchiveData(void)
{
  if(ArchiveData() == TRUE)
    VistArchiveVerb = VAV_CHECK;
}

//---------------------------------------------------------
void VistArchiveDataDraw(void)
{
  LcdDrawBegin();

  VistMeasureDrawObject(MemCellVistObject, ArchiveRedraw);

  VistMeasureDrawF(74, MemCellF, TRUE, ArchiveRedraw);
  switch(MemCellVistType) {
    case vptSpp:
      VistMeasureDrawS(134, MemCellVistS, TRUE, ArchiveRedraw);
      break;
    case vptVrms:
      VistMeasureDrawV(134, MemCellVistV, TRUE, ArchiveRedraw);
      break;
    case vptAamp:
      VistMeasureDrawA(134, MemCellVistA, TRUE, ArchiveRedraw);
      break;
    default:
      ;
  }
  VistMeasureDrawNoise(180, MeasureNoise, TRUE, ArchiveRedraw);

  //VistMeasureDrawType(224, (TVistParamsType)MemCellVistType);

  LcdDrawEnd();

  ArchiveRedraw = FALSE;
}
