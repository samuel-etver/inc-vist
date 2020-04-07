#ifndef IIC_H_INCLUDED
#define IIC_H_INCLUDED

/*---------------------------------------------------------
  iic.h
---------------------------------------------------------*/

#include "xmain.h"

typedef struct {
  BIT StartWrite: 1;
  BIT Ack: 1;
} TIicFlags;

extern NOINIT TIicFlags IicFlags;

void InitIic(void);

typedef void (*TIicStartWriteProc)(BYTE slave, DWORD address,
  BYTE address_size);
typedef void (*TIicStopWriteProc)(void);
typedef void (*TIicStartReadProc)(BYTE slave, DWORD address,
  BYTE address_size);
typedef void (*TIicStopReadProc)(void);
typedef void (*TIicWriteByteProc)(BYTE data);
typedef BYTE (*TIicReadByteProc)(void);

extern NOINIT TIicStartWriteProc IicStartWrite;
extern NOINIT TIicStopWriteProc IicStopWrite;
extern NOINIT TIicStartReadProc IicStartRead;
extern NOINIT TIicStopReadProc IicStopRead;
extern NOINIT TIicWriteByteProc IicWriteByte;
extern NOINIT TIicReadByteProc IicReadByte;

void IicSetNormalFrequencyAfter(void);
void IicSetFastFrequencyBefore(void);

BOOL IicCheckSlave(BYTE slave);
BOOL IicIsSoft(void);
void IicSetSoft(void);

#endif
