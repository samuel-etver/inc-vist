#ifndef MAIN_H_INCLUDED
#define MAIN_H_INCLUDED

/*---------------------------------------------------------
  main.h
---------------------------------------------------------*/

#ifndef WIN32
#include "stm32f4xx_hal.h"
#include <intrinsics.h>

extern ADC_HandleTypeDef hadc1;
extern TIM_HandleTypeDef htim2;
extern TIM_HandleTypeDef htim3;
extern TIM_HandleTypeDef htim4;
extern TIM_HandleTypeDef htim5;
extern TIM_HandleTypeDef htim9;
extern SPI_HandleTypeDef hspi1;
extern PCD_HandleTypeDef hpcd_USB_OTG_FS;

/*!!!

 -  Disable HAL_RTC_SetTime, HAL_RTC_SetDate,
    HAL_RTC_SetAlarm in MX_RTC_Init (main.c)
 - Insert
    void xmain(void);
    xmain();
    into main();
  - Add __weak to TIM4_IRQHandler in st32f4xx_it.c
  - Add __weak to TIM1_BRK_TIM9_IRQHandler in st32f4xx_it.c

*/

#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "product.h"
#include "SoftwareVersion.h"
#include "build.h"
#include "common.h"

#define SUPERUSER

#ifndef WIN32
typedef unsigned long           DWORD;
typedef unsigned short          WORD;
typedef unsigned char           BYTE;
typedef unsigned char           BITS;
typedef unsigned char           BIT;
typedef unsigned char           BOOL;
typedef signed char             INT8;
typedef signed short            INT16;
typedef signed long             INT32;
typedef signed long long        INT64;
typedef unsigned short          UINT16;
typedef unsigned long           UINT32;
typedef unsigned long long      UINT64;
typedef float                   FLOAT32;
typedef double                  FLOAT64;

#  define FALSE                 0x00
#  define TRUE                  0xFF

#  define NOINIT                __no_init

#  define LOWORD(x)             ((WORD)(0xFFFF & (x)))
#  define HIWORD(x)             ((WORD)(0xFFFF & ((x) >> 16)))
#  define LOBYTE(x)             ((BYTE)(0xFF & (x)))
#  define HIBYTE(x)             ((BYTE)(0xFF & ((x) >> 8)))

#else

#  define NOINIT
#  define __ramfunc
#  include <windows.h>

typedef unsigned char           BIT;
typedef float                   FLOAT32;
typedef double                  FLOAT64;

#endif


#define ARRAY_LENGTH(a)         (sizeof(a)/sizeof(a[0]))
#define TO_BOOL(a)              ((a) ? TRUE : FALSE)

#define BIT0                    0x00000001
#define BIT1                    0x00000002
#define BIT2                    0x00000004
#define BIT3                    0x00000008
#define BIT4                    0x00000010
#define BIT5                    0x00000020
#define BIT6                    0x00000040
#define BIT7                    0x00000080
#define BIT8                    0x00000100
#define BIT9                    0x00000200
#define BIT10                   0x00000400
#define BIT11                   0x00000800
#define BIT12                   0x00001000
#define BIT13                   0x00002000
#define BIT14                   0x00004000
#define BIT15                   0x00008000
#define BIT16                   0x00010000
#define BIT17                   0x00020000
#define BIT18                   0x00040000
#define BIT19                   0x00080000
#define BIT20                   0x00100000
#define BIT21                   0x00200000
#define BIT22                   0x00400000
#define BIT23                   0x00800000
#define BIT24                   0x01000000
#define BIT25                   0x02000000
#define BIT26                   0x04000000
#define BIT27                   0x08000000
#define BIT28                   0x10000000
#define BIT29                   0x20000000
#define BIT30                   0x40000000
#define BIT31                   0x80000000


typedef union {
  DWORD d;
  INT32 l;
  INT16 i;
  FLOAT32 f;
  BYTE  b[4];
  WORD  w[2];
} UN;

typedef enum TProgramVerb {
  VB_CHIPCHECK_INI,VB_CHIPCHECK,
  VB_LOADCFG,
  VB_SPLASH_INI,VB_SPLASH,
  VB_MENU_INI,VB_MENU,
  VB_MEASURE_INI,VB_MEASURE,
  VB_LANGUAGE_INI,VB_LANGUAGE,
  VB_DEVICEMODE_INI,VB_DEVICEMODE,
  VB_CALENDAR_INI,VB_CALENDAR,
  VB_SUPPLYSOURCE_INI,VB_SUPPLYSOURCE,
  VB_VOLTAGEDOWN_INI,VB_VOLTAGEDOWN,
  VB_ABOUTDEVICE_INI,VB_ABOUTDEVICE,
  VB_ABOUTMANUFACTURER_INI,VB_ABOUTMANUFACTURER,
  VB_AUTOOFF_INI,VB_AUTOOFF,
  VB_PASSWORD_INI,VB_PASSWORD,
  VB_FLASHDUMP_INI,VB_FLASHDUMP,
  VB_FIRSTPRODUCED_INI,VB_FIRSTPRODUCED,
  VB_FACTORYLOAD_INI,VB_FACTORYLOAD,
  VB_FACTORYSAVE_INI,VB_FACTORYSAVE,
  VB_THEME_INI,VB_THEME,
  VB_KEYBRDSOUND_INI,VB_KEYBRDSOUND,

  VB_DEVICETYPE_INI,VB_DEVICETYPE,
  VB_IONCALIBRATION_INI,VB_IONCALIBRATION,
  VB_MEASURINGTRACT_INI,VB_MEASURINGTRACT,
  VB_MEASUREHARDWARETEST_INI,VB_MEASUREHARDWARETEST,
  VB_ZEROCALIBRATION_INI,VB_ZEROCALIBRATION,

  VB_INC_PARAM_INI,VB_INC_PARAM,
  VB_INC_ARCHIVE_INI,VB_INC_ARCHIVE,
  VB_INC_MEMORY_INI,VB_INC_MEMORY,
  VB_INC_CLEARMEM_INI,VB_INC_CLEARMEM,
  VB_INC_MEASUREMODE_INI,VB_INC_MEASUREMODE,
  VB_INC_PARAMS_INI,VB_INC_PARAMS,
  VB_INC_TEST_INI,VB_INC_TEST,

  VB_VIST_PARAM_INI,VB_VIST_PARAM,
  VB_VIST_ARCHIVE_INI,VB_VIST_ARCHIVE,
  VB_VIST_MEMORY_INI,VB_VIST_MEMORY,
  VB_VIST_CLEARMEM_INI,VB_VIST_CLEARMEM,
  VB_VIST_PARAMS_INI,VB_VIST_PARAMS,
  VB_VIST_TEST_INI,VB_VIST_TEST,
  VB_VIST_SENSOR_INI,VB_VIST_SENSOR,
  VB_VIST_CALIBRATION_INI,VB_VIST_CALIBRATION
} TProgramVerb;

extern NOINIT TProgramVerb PrgVerb;

typedef struct {
  BIT CentiSec: 1;
  BIT SuperUser: 1;
  BIT Password: 1;
  BIT Temp: 1;
  BIT LowSupplySource: 1;
  BIT Charging: 1;
} TProgramFlags;

extern NOINIT TProgramFlags PrgFlags;

extern NOINIT BOOL TempBool;
extern NOINIT BYTE TempByte;
extern NOINIT WORD TempWord;
extern NOINIT DWORD TempDword;
extern NOINIT INT8 TempInt8;
extern NOINIT FLOAT32 TempFloat;
extern NOINIT void* TempPtr;
extern NOINIT BOOL  DebugBool;
extern NOINIT INT32 DebugInt32;
extern NOINIT FLOAT32 DebugFloat;

#define TEMP_BUFF_SIZE        1024
extern NOINIT BYTE TempBuff[TEMP_BUFF_SIZE];

void MainInit(void);
void MainSetup(void);
void MainDo(void);

void xmain(void);
BOOL ProcessStandardKeyDownActions(void);

#endif
