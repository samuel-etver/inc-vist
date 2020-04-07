#ifndef CFG_H_INCLUDED
#define CFG_H_INCLUDED

/*---------------------------------------------------------
  cfg.h
---------------------------------------------------------*/

#include "xmain.h"
#include "mem.h"

#define CFG_ADDRESS                             0

#define CFG_LANGUAGE                            0
#define CFG_THEME                               1
#define CFG_AUTOOFF_POWER                       2
#define CFG_AUTOOFF_LIGHT                       3
#define CFG_AUTOOFF_CONTRAST                    4
#define CFG_KEYBRDSOUNDON                       5
// 6

#define CFG_DEVICETYPE                          20
#define CFG_MEASURINGTRACT_S_A0                 21
#define CFG_MEASURINGTRACT_V_A0                 29
#define CFG_MEASURINGTRACT_FS                   37
#define CFG_IONCALIBRATION                      41
#define CFG_MEASUREHARDWARETEST_K               47
#define CFG_MEASUREHARDWARETEST_F               48
#define CFG_MEASUREHARDWARETEST_PARAM           50
#define CFG_MEASURINGTRACT_A_A0                 51
#define CFG_MEASURINGTRACT_S_A1                 59
#define CFG_MEASURINGTRACT_V_A1                 67
#define CFG_MEASURINGTRACT_A_A1                 75
#define CFG_ZEROCALIBRATION_VALUE               83
// 85

#define CFG_VIST_PARAMSTYPE                     90
#define CFG_VIST_PARAMSOBJECT                   91
#define CFG_VIST_SENSORFACTOR0                  92
#define CFG_VIST_SENSORFACTOR1                  96
// 100

#define CFG_INC_MEASUREMODE                     140
#define CFG_INC_PARAMSUNITS                     141
#define CFG_INC_PARAMSDIAMETER                  142
#define CFG_INC_PARAMSLENGTH                    143
#define CFG_INC_PARAMSTENSION                   145
// 147

#define CFG_FULL_SIZE														200

void CfgRead(WORD address, BYTE* data, WORD n);
void CfgWrite(WORD address, BYTE* data, WORD n);

BYTE CfgReadByte(WORD address);
void CfgWriteByte(WORD address, BYTE data);

WORD CfgReadWord(WORD address);
void CfgWriteWord(WORD address, WORD data);

DWORD CfgReadDword(WORD address);
void CfgWriteDword(WORD address, DWORD data);

FLOAT32 CfgReadFloat(WORD address);
void CfgWriteFloat(WORD address, FLOAT32 data);

BOOL CfgReadFactor(WORD address, BYTE* factor, BYTE len, BYTE comma);
void CfgWriteFactor(WORD address, BYTE* factor, BYTE len);

WORD CfgGetBaseDeviceModeAddress(BYTE device_mode);
WORD CfgGetEndAddress(void);

#endif
