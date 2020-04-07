#ifndef VISTPARAMS_H_INCLUDED
#define VISTPARAMS_H_INCLUDED

/*---------------------------------------------------------
  VistParams.h
---------------------------------------------------------*/

#include "xmain.h"

void InitVistParams(void);
void ShowVistParamsIni(void);
void ShowVistParams(void);

void LoadVistParams(void);
void SaveVistParams(void);

void LoadFirstProducedVistParams(void);

typedef enum {
  vptSpp = 0,
  vptVrms,
  vptAamp, vptLast
} TVistParamsType;

#define VIST_PARAMS_TYPES_COUNT     ((BYTE)vptLast)
#define VIST_PARAMS_OBJECTS_COUNT   2

extern NOINIT BYTE VistParamsObject;
extern NOINIT TVistParamsType VistParamsType;
extern const WORD VistParamsTypeResIds[];

#endif
