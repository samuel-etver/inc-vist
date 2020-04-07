/*---------------------------------------------------------
  Mem.c
---------------------------------------------------------*/

#include "mem.h"
#include "flash.h"
#include "measure.h"
#include "utils.h"
#include "DeviceType.h"
#include "IncParams.h"
#include "IncMeasureMode.h"
#include "IncMeasure.h"
#include "VistParams.h"

#define MEM_BUFF_SIZE                           128U
static NOINIT BYTE MemBuff[MEM_BUFF_SIZE];

static NOINIT WORD MemCellPrev;
static NOINIT WORD MemCellNext;
static NOINIT WORD MemCellCurr;
NOINIT TMemFlags MemFlags;
NOINIT BYTE MemCellDate[3];
NOINIT BYTE MemCellTime[3];
NOINIT WORD MemCellNumber;
NOINIT FLOAT32 MemCellT;
NOINIT FLOAT32 MemCellF;
NOINIT FLOAT32 MemCellNoise;
NOINIT BYTE MemCellMeasureMode;
NOINIT BYTE MemCellIncDiameter;
NOINIT WORD MemCellIncLength;
NOINIT WORD MemCellIncTension;
NOINIT FLOAT32 MemCellIncEpsilon;
NOINIT FLOAT32 MemCellIncDeltaL;
NOINIT FLOAT32 MemCellIncSigma;
NOINIT BYTE MemCellVistObject;
NOINIT BYTE MemCellVistType;
NOINIT FLOAT32 MemCellVistV;
NOINIT FLOAT32 MemCellVistS;
NOINIT FLOAT32 MemCellVistA;

static DWORD MemGetCellAddr(WORD cell);

static void MemWrite(WORD n);
static void MemRead(WORD n);

static BYTE MemGetByte(WORD index);
static void MemSetByte(WORD index, BYTE data);
static void MemGetBytes(WORD index, BYTE* buff, WORD n);
static WORD MemGetWord(WORD index);
static void MemSetWord(WORD index, WORD data);
static FLOAT32 MemGetFloat(WORD index);
static void MemSetFloat(WORD index, FLOAT32 data);

static void MemSaveCellData(void);

//---------------------------------------------------------
WORD MemGetTotalCells(void)
{
  return MEM_CELLS_SIZE;
}

//---------------------------------------------------------
WORD MemGetUsedCells(void)
{
  FlashAddress = MEM_SYS_ADDR + MEM_SYS_USED_CELLS;
  MemRead(2);
  return MemGetWord(0);
}

//---------------------------------------------------------
WORD MemGetFreeCells(void)
{
  return MemGetTotalCells() - MemGetUsedCells();
}

//---------------------------------------------------------
void MemClear(void)
{
  memset(MemBuff, 0xFFU, MEM_SYS_SIZE);
  MemSetWord(MEM_SYS_USED_CELLS, 0);
  FlashAddress = MEM_SYS_ADDR;
  MemWrite(MEM_SYS_SIZE);
  FlashAddress = MEM_CELLS_SYS_ADDR;
  FlashFill(0, MEM_CELLS_SYS_SIZE);
}

//---------------------------------------------------------
static DWORD MemGetCellAddr(WORD cell)
{
  return MEM_CELLS_ADDR + cell*(DWORD)MEM_CELL_SIZE;
}

//---------------------------------------------------------
void MemLoadCell(void)
{
  MemFlags.Empty = 0;

  FlashAddress = MemGetCellAddr(MemCellCurr);
  MemRead(MEM_CELL_SIZE);
  MemGetBytes(MEM_CELL_DATE, MemCellDate, 3);
  MemGetBytes(MEM_CELL_TIME, MemCellTime, 3);
  MemCellPrev = MemGetWord(MEM_CELL_PREV);
  MemCellNext = MemGetWord(MEM_CELL_NEXT);
  MemCellNumber = MemGetWord(MEM_CELL_NUMBER);
  MemCellT = MemGetFloat(MEM_CELL_T);
  MemCellF = MeasureCalcF(MemCellT);
  MemCellNoise = MemGetFloat(MEM_CELL_NOISE);
  MemCellMeasureMode = MemGetByte(MEM_CELL_MEASUREMODE);
  MemCellIncDiameter = MemGetByte(MEM_CELL_INC_DIAMETER);
  MemCellIncLength = MemGetWord(MEM_CELL_INC_LENGTH);
  MemCellIncTension = MemGetWord(MEM_CELL_INC_TENSION);
  MemCellIncEpsilon = MemGetFloat(MEM_CELL_INC_EPSILON);
  MemCellIncDeltaL = MemGetFloat(MEM_CELL_INC_DELTAL);
  MemCellIncSigma = MemGetFloat(MEM_CELL_INC_SIGMA);
  MemCellVistObject = MemGetByte(MEM_CELL_VIST_OBJECT);
  MemCellVistType = MemGetByte(MEM_CELL_VIST_TYPE);
  MemCellVistS = MemGetFloat(MEM_CELL_VIST_S);
  MemCellVistV = MemGetFloat(MEM_CELL_VIST_V);
  MemCellVistA = MemGetFloat(MEM_CELL_VIST_A);
}

//---------------------------------------------------------
void MemLocateLast(void)
{
  FlashAddress = MEM_SYS_ADDR;
  MemRead(MEM_SYS_SIZE);

  if(MemGetWord(MEM_SYS_USED_CELLS) == 0)
    MemFlags.Empty = 1;
  else {
    MemCellCurr = MemGetWord(MEM_SYS_LAST_CELL);
    MemLoadCell();
  }
}

//---------------------------------------------------------
void MemLocateFirst(void)
{
  FlashAddress = MEM_SYS_ADDR;
  MemRead(MEM_SYS_SIZE);

  if(MemGetWord(MEM_SYS_USED_CELLS) == 0)
    MemFlags.Empty = 1;
  else {
    MemCellCurr = MemGetWord(MEM_SYS_FIRST_CELL);
    MemLoadCell();
  }
}

//---------------------------------------------------------
void MemLocateNext(void)
{
  if(!MemFlags.Empty && MemCellNext != MEM_NULL) {
    MemCellCurr = MemCellNext;
    MemLoadCell();
  }
}

//---------------------------------------------------------
void MemLocatePrev(void)
{
  if(!MemFlags.Empty && MemCellPrev != MEM_NULL) {
    MemCellCurr = MemCellPrev;
    MemLoadCell();
  }
}

//---------------------------------------------------------
void MemLocateNextDate(void)
{
  BYTE curr_date[3];

  if(MemFlags.Empty) return;

  memcpy(curr_date, MemCellDate, 3);

  while(MemCellNext != MEM_NULL) {
    MemCellCurr = MemCellNext;
    MemLoadCell();
    if(memcmp(curr_date, MemCellDate, 3))
      break;
  }

  MemLoadCell();
}

//---------------------------------------------------------
void MemLocatePrevDate(void)
{
  BYTE curr_date[3];

  if(MemFlags.Empty) return;

  memcpy(curr_date, MemCellDate, 3);

  while(MemCellPrev != MEM_NULL) {
    MemCellCurr = MemCellPrev;
    MemLoadCell();
    if(memcmp(curr_date, MemCellDate, 3))
      break;
  }

  MemLoadCell();
}

//---------------------------------------------------------
void MemDelCell(void)
{
  WORD first_cell;
  WORD last_cell;

  if(MemFlags.Empty) return;

  if(MemCellNext == MEM_NULL) {
    last_cell = MemCellPrev;
  } else {
    FlashAddress = MEM_SYS_ADDR + MEM_SYS_LAST_CELL;
    MemRead(2);
    last_cell = MemGetWord(0);

    FlashAddress = MemGetCellAddr(MemCellNext) + MEM_CELL_PREV;
    MemSetWord(0, MemCellPrev);
    MemWrite(2);
  }

  if(MemCellPrev == MEM_NULL) {
    first_cell = MemCellNext;
  } else {
    FlashAddress = MEM_SYS_ADDR + MEM_SYS_FIRST_CELL;
    MemRead(2);
    first_cell = MemGetWord(0);

    FlashAddress = MemGetCellAddr(MemCellPrev) + MEM_CELL_NEXT;
    MemSetWord(0, MemCellNext);
    MemWrite(2);
  }

  FlashAddress = MEM_CELLS_SYS_ADDR + (MemCellCurr >> 3);
  MemRead(1);
  MemSetByte(0, MemGetByte(0) & ~(BYTE)(1 << (MemCellCurr & 0x07)));
  FlashAddress--;
  MemWrite(1);

  FlashAddress = MEM_SYS_ADDR + MEM_SYS_USED_CELLS;
  MemRead(2);
  MemSetWord(MEM_SYS_USED_CELLS, MemGetWord(0) - 1U);
  MemSetWord(MEM_SYS_FIRST_CELL, first_cell);
  MemSetWord(MEM_SYS_LAST_CELL, last_cell);
  FlashAddress = MEM_SYS_ADDR;
  MemWrite(MEM_SYS_SIZE);

  if(MemCellPrev == MEM_NULL && MemCellNext == MEM_NULL)
    MemFlags.Empty = 1;
  else {
    MemCellCurr = MemCellPrev != MEM_NULL ? MemCellPrev : MemCellNext;
    MemLoadCell();
  }
}

//---------------------------------------------------------
void MemAddCell(void)
{
  WORD used_cells = MemGetUsedCells();
  WORD total_cells = MemGetTotalCells();
  WORD last_cell;
  WORD new_cell;
  WORD new_number;
  BYTE b;

  if(used_cells > total_cells) return;

  if(used_cells == total_cells) {
    FlashAddress = MEM_SYS_ADDR + MEM_SYS_LAST_CELL;
    MemRead(2);
    MemCellCurr = MemGetWord(0);
    MemLoadCell();
    MemDelCell();
    used_cells = MemGetUsedCells();
    if(used_cells >= total_cells)
      return;
  }

  if(used_cells == 0) {
    MemSetByte(0, 0x01);
    FlashAddress = MEM_CELLS_SYS_ADDR;
    MemWrite(1);

    MemSetWord(MEM_SYS_USED_CELLS, 1);
    MemSetWord(MEM_SYS_FIRST_CELL, 0);
    MemSetWord(MEM_SYS_LAST_CELL,  0);
    FlashAddress = MEM_SYS_ADDR;
    MemWrite(MEM_SYS_SIZE);

    FlashAddress = MemGetCellAddr(0);
    MemSetWord(MEM_CELL_PREV, MEM_NULL);
    MemSetWord(MEM_CELL_NUMBER, 1);
    MemSaveCellData();
    return;
  }

  FlashAddress = MEM_CELLS_SYS_ADDR;
  new_cell = 0;
  do {
    MemRead(1);
    b = ~MemGetByte(0);
    if(b) {
      if(b & 0x01) ;
      else if(b & 0x02) new_cell += 1;
      else if(b & 0x04) new_cell += 2;
      else if(b & 0x08) new_cell += 3;
      else if(b & 0x10) new_cell += 4;
      else if(b & 0x20) new_cell += 5;
      else if(b & 0x40) new_cell += 6;
      else              new_cell += 7;
      break;
    }
    new_cell += 8;
  } while(new_cell < MEM_CELLS_SIZE);

  if(new_cell >= MEM_CELLS_SIZE) return;

  FlashAddress = MEM_SYS_ADDR;
  MemRead(MEM_SYS_SIZE);
  last_cell = MemGetWord(MEM_SYS_LAST_CELL);
  MemSetWord(MEM_SYS_LAST_CELL, new_cell);
  MemSetWord(MEM_SYS_USED_CELLS, 1 + MemGetWord(MEM_SYS_USED_CELLS));
  FlashAddress = MEM_SYS_ADDR;
  MemWrite(MEM_SYS_SIZE);

  FlashAddress = MEM_CELLS_SYS_ADDR + (new_cell >> 3);
  MemRead(1);
  MemSetByte(0, MemGetByte(0) | (1 << (new_cell & 0x07)));
  --FlashAddress;
  MemWrite(1);

  FlashAddress = MemGetCellAddr(last_cell) + MEM_CELL_NEXT;
  MemSetWord(0, new_cell);
  MemWrite(2);
  FlashAddress = MemGetCellAddr(last_cell) + MEM_CELL_DATE;
  MemRead(3);
  if(MemGetByte(0) == MeasureDay &&
     MemGetByte(1) == MeasureMon &&
     MemGetByte(2) == MeasureYear) {
    FlashAddress = MemGetCellAddr(last_cell) + MEM_CELL_NUMBER;
    MemRead(2);
    new_number = 1 + MemGetWord(0);
  } else {
    new_number = 1;
  }

  FlashAddress = MemGetCellAddr(new_cell);
  MemSetWord(MEM_CELL_NUMBER, new_number);
  MemSetWord(MEM_CELL_PREV, last_cell);
  MemSaveCellData();
}

//---------------------------------------------------------
static BYTE MemGetByte(WORD index)
{
  return MemBuff[index];
}

//---------------------------------------------------------
static void MemSetByte(WORD index, BYTE data)
{
  MemBuff[index] = data;
}

//---------------------------------------------------------
static void MemGetBytes(WORD index, BYTE* buff, WORD n)
{
  memcpy(buff, MemBuff + index, n);
}

//---------------------------------------------------------
static WORD MemGetWord(WORD index)
{
  WORD w;
  memcpy(&w, MemBuff + index, 2);
  return w;
}

//---------------------------------------------------------
static void MemSetWord(WORD index, WORD data)
{
  memcpy(MemBuff + index, &data, 2);
}

//---------------------------------------------------------
static FLOAT32 MemGetFloat(WORD index)
{
  FLOAT32 f;
  memcpy(&f, MemBuff + index, 4);
  return f;
}

//---------------------------------------------------------
static void MemSetFloat(WORD index, FLOAT32 data)
{
  memcpy(MemBuff + index, &data, 4);
}

//---------------------------------------------------------
static void MemWrite(WORD n)
{
  FlashWrite(MemBuff, n);
}

//---------------------------------------------------------
static void MemRead(WORD n)
{
  FlashRead(MemBuff, n);
}

//---------------------------------------------------------
static void MemSaveCellData(void)
{
  MemSetWord(MEM_CELL_NEXT, MEM_NULL);
  MemSetByte(MEM_CELL_TIME,     MeasureHour);
  MemSetByte(MEM_CELL_TIME + 1, MeasureMin);
  MemSetByte(MEM_CELL_TIME + 2, MeasureSec);
  MemSetByte(MEM_CELL_DATE,     MeasureDay);
  MemSetByte(MEM_CELL_DATE + 1, MeasureMon);
  MemSetByte(MEM_CELL_DATE + 2, MeasureYear);
  MemSetFloat(MEM_CELL_T, MeasureTResult);
  MemSetFloat(MEM_CELL_NOISE, MeasureNoiseResult);
  if(DeviceType == dtVist)
    MemSetByte(MEM_CELL_MEASUREMODE, (BYTE)incMmVist);
  else if(DeviceType == dtIncVist)
    MemSetByte(MEM_CELL_MEASUREMODE, (BYTE)IncMeasureMode);
  else
    MemSetByte(MEM_CELL_MEASUREMODE, (BYTE)incMmInc);
  MemSetByte(MEM_CELL_INC_DIAMETER, IncParamsDiameter);
  MemSetWord(MEM_CELL_INC_LENGTH, IncParamsLength);
  MemSetWord(MEM_CELL_INC_TENSION, IncParamsTension);
  MemSetFloat(MEM_CELL_INC_EPSILON, IncMeasureEpsilon);
  MemSetFloat(MEM_CELL_INC_DELTAL, IncMeasureDeltaL);
  MemSetFloat(MEM_CELL_INC_SIGMA, IncMeasureSigma);
  MemSetByte(MEM_CELL_VIST_OBJECT, VistParamsObject);
  MemSetByte(MEM_CELL_VIST_TYPE, VistParamsType);
  switch(VistParamsType) {
    case vptSpp:
      MemSetFloat(MEM_CELL_VIST_S, MeasureSResult);
      break;
    case vptVrms:
      MemSetFloat(MEM_CELL_VIST_V, MeasureVResult);
      break;
    case vptAamp:
      MemSetFloat(MEM_CELL_VIST_A, MeasureAResult);
      break;
    default:  ;
  }
  FlashWrite(MemBuff, MEM_CELL_SIZE);
}
