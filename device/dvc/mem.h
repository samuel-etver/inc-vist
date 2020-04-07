#ifndef MEM_H_INCLUDED
#define MEM_H_INCLUDED

/*---------------------------------------------------------
  mem.h
---------------------------------------------------------*/

#include "xmain.h"
#include "measure.h"

#define MEM_NULL                                0xFFFFU

#define MEM_SYS_ADDR                            1000
#define MEM_SYS_SIZE                            6U
#define MEM_SYS_USED_CELLS                      0U
#define MEM_SYS_FIRST_CELL                      2U
#define MEM_SYS_LAST_CELL                       4U

#define MEM_CELLS_SYS_ADDR                      1010U
#define MEM_CELLS_SYS_SIZE                      250U
#define MEM_CELLS_ADDR                          1500U
#define MEM_CELLS_SIZE                          2000U

// Cells
#define MEM_CELL_DATE                           0U
#define MEM_CELL_TIME                           3U
#define MEM_CELL_PREV                           6U
#define MEM_CELL_NEXT                           8U
#define MEM_CELL_NUMBER                         10U
#define MEM_CELL_BASE_SIZE                      12U

#define MEM_CELL_T                              12U
#define MEM_CELL_NOISE                          16U
#define MEM_CELL_MEASUREMODE                    20U

// Inc
#define MEM_CELL_INC_DIAMETER                   21U
#define MEM_CELL_INC_LENGTH                     22U
#define MEM_CELL_INC_TENSION                    24U
#define MEM_CELL_INC_EPSILON                    26U
#define MEM_CELL_INC_DELTAL                     30U
#define MEM_CELL_INC_SIGMA                      34U
// Vist
#define MEM_CELL_VIST_OBJECT                    38U
#define MEM_CELL_VIST_TYPE                      39U
#define MEM_CELL_VIST_S                         40U
#define MEM_CELL_VIST_V                         MEM_CELL_VIST_S
#define MEM_CELL_VIST_A                         MEM_CELL_VIST_S

#define MEM_CELL_SIZE                           44U


typedef struct {
 BIT Empty: 1;
} TMemFlags;

extern NOINIT TMemFlags MemFlags;
extern NOINIT BYTE MemCellDate[3];
extern NOINIT BYTE MemCellTime[3];
extern NOINIT WORD MemCellNumber;
extern NOINIT FLOAT32 MemCellT;
extern NOINIT FLOAT32 MemCellF;
extern NOINIT FLOAT32 MemCellNoise;
extern NOINIT BYTE MemCellMeasureMode;
extern NOINIT BYTE MemCellIncDiameter;
extern NOINIT WORD MemCellIncLength;
extern NOINIT WORD MemCellIncTension;
extern NOINIT FLOAT32 MemCellIncEpsilon;
extern NOINIT FLOAT32 MemCellIncDeltaL;
extern NOINIT FLOAT32 MemCellIncSigma;
extern NOINIT BYTE MemCellVistObject;
extern NOINIT BYTE MemCellVistType;
extern NOINIT FLOAT32 MemCellVistS;
extern NOINIT FLOAT32 MemCellVistV;
extern NOINIT FLOAT32 MemCellVistA;

WORD MemGetTotalCells(void);
WORD MemGetUsedCells(void);
WORD MemGetFreeCells(void);
void MemClear(void);
void MemLoadCell(void);
void MemLocateLast(void);
void MemLocateFirst(void);
void MemLocateNext(void);
void MemLocatePrev(void);
void MemLocateNextDate(void);
void MemLocatePrevDate(void);
void MemDelCell(void);
void MemAddCell(void);

#endif
