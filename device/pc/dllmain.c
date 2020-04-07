/*---------------------------------------------------------
  dllmain.cpp
---------------------------------------------------------*/

#include "dllmain.h"
#include "xmain.h"
#include "lcd.h"
#include "system.h"
#include "system_pd.h"
#include "keybrd.h"
#include "keybrd_pd.h"
#include "flash.h"
#include "flash_pd.h"
#include "SupplySource.h"
#include "menu.h"
#include "measure.h"
#include "measure_pd.h"

#define VAR_TYPE_BOOL       0
#define VAR_TYPE_INT8       1
#define VAR_TYPE_BYTE       2
#define VAR_TYPE_INT16      3
#define VAR_TYPE_WORD       4
#define VAR_TYPE_INT32      5
#define VAR_TYPE_DWORD      6
#define VAR_TYPE_FLOAT32    7
#define VAR_TYPE_FLOAT64    8
#define VAR_TYPE_STR        9
#define VAR_TYPE_BIT        10
#define VAR_TYPE_NONE       0xFFFF

#define VAR_BUFF_SIZE       256

static BYTE VarBuff[VAR_BUFF_SIZE];
// 0..1 - type
// 2..  - value

DLL_EXPORT BOOL APIENTRY DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    switch (fdwReason)
    {
        case DLL_PROCESS_ATTACH:
            // attach to process
            // return FALSE to fail DLL load
            break;

        case DLL_PROCESS_DETACH:
            // detach from process
            break;

        case DLL_THREAD_ATTACH:
            // attach to thread
            break;

        case DLL_THREAD_DETACH:
            // detach from threadstatic void MeasuringTractSetEdit(void)

            break;
    }
    return TRUE; // succesful
}

//---------------------------------------------------------
void DLL_EXPORT Dll_Execute(void)
{
  if(SystemIsOn() == FALSE) {
    if(KeybrdIsPowerPressed() == TRUE) {
      MainInit();
      MainSetup();
    }
  }
  else {
    MainDo();
  }
}

//---------------------------------------------------------
DWORD DLL_EXPORT Dll_LcdGetPixel(INT32 x, INT32 y)
{
  union {
    TColor Color;
    BYTE   Bytes[2];
  } data;
  if(x < 0 || x >= LCD_W ||
     y < 0 || y >= LCD_H)
    return 0;
  data.Color = LcdGetPixel(x, y);
  return data.Bytes[0]*256 + data.Bytes[1];
}

//---------------------------------------------------------
void DLL_EXPORT Dll_KeybrdSetKeys(WORD keys)
{
  KeybrdSetKeys(keys);
}

//---------------------------------------------------------
WORD DLL_EXPORT Dll_SystemIsOn(void)
{
  return SystemIsOn();
}

//---------------------------------------------------------
void* DLL_EXPORT Dll_GetVar(LPCSTR* var_name)
{
  int eq(const char* curr_name)
  {
    return strcmp(curr_name, (const char*)var_name) == 0;
  }
  // wr_type
  void wr_type(WORD t)
  {
    memcpy(VarBuff, &t, 2);
  }
  // wr_value
  void wr_value(void* value, int size)
  {
    memcpy(VarBuff + 2, value, size);
  }
  // none
  void wr_none(void)
  {
    wr_type(VAR_TYPE_NONE);
  }
  // Bool
  void wr_bool(BOOL value)
  {
    wr_type(VAR_TYPE_BOOL);
    wr_value(&value, sizeof(BOOL));
  }
  // Byte
  void wr_byte(BYTE value)
  {
    wr_type(VAR_TYPE_BYTE);
    wr_value(&value, sizeof(BYTE));
  }
  // Word
  void wr_word(WORD value)
  {
    wr_type(VAR_TYPE_WORD);
    wr_value(&value, sizeof(WORD));
  }
  // Int32
  void wr_int32(INT32 value)
  {
    wr_type(VAR_TYPE_INT32);
    wr_value(&value, sizeof(INT32));
  }
  // Float32
  void wr_float32(FLOAT32 value)
  {
    wr_type(VAR_TYPE_FLOAT32);
    wr_value(&value, sizeof(FLOAT32));
  }

  if(eq("DebugFloat")) wr_float32(DebugFloat);
  else if(eq("DebugInt32")) wr_int32(DebugInt32);
  else if(eq("DebugBool"))  wr_bool(DebugBool);
  else if(eq("SupplySourceVoltage")) wr_float32(SupplySourceVoltage);
  else if(eq("MenuLevel")) wr_byte(MenuLevel);
  else if(eq("MeasureT_pd")) wr_float32(MeasureT_pd);
  else if(eq("MeasureNoise_pd")) wr_float32(MeasureNoise_pd);
  else if(eq("MeasureUavr")) wr_float32(MeasureUavr);
  else if(eq("MeasureUamp")) wr_float32(MeasureUamp);
  else if(eq("MeasureUpeak")) wr_float32(MeasureUpeak);
  else wr_none();

  return VarBuff;
}

//---------------------------------------------------------
BOOL DLL_EXPORT Dll_SetVar(LPSTR* var_name, void* var_value)
{
  WORD t;

  int eq(const char* curr_name)
  {
    return strcmp(curr_name, (const char*)var_name) == 0;
  }
  int check_type(WORD a)
  {
    return a == t;
  }
  BOOL rd_bool(BOOL* v)
  {
    if(!check_type(VAR_TYPE_BOOL))
      return FALSE;
    memcpy(v, var_value, 1);
    return TRUE;
  }
  BOOL rd_byte(BYTE* v)
  {
    if(!check_type(VAR_TYPE_BYTE))
      return FALSE;
    memcpy(v, var_value, 1);
    return TRUE;
  }
  BOOL rd_word(WORD *v)
  {
    if(!check_type(VAR_TYPE_WORD))
      return FALSE;
    memcpy(v, var_value, 2);
    return TRUE;
  }
  BOOL rd_int32(INT32* v)
  {
    if(!check_type(VAR_TYPE_INT32))
      return FALSE;
    memcpy(v, var_value, 4);
    return TRUE;

  }
  BOOL rd_float32(FLOAT32* v)
  {
    if(!check_type(VAR_TYPE_FLOAT32))
      return FALSE;
    memcpy(v, var_value, 4);
    return TRUE;
  }

  t = *(WORD*)var_value;
  var_value += 2;

  if(eq("DebugFloat")) return rd_float32(&DebugFloat);
  else if(eq("DebugInt32")) return rd_int32(&DebugInt32);
  else if(eq("DebugBool")) return rd_bool(&DebugBool);
  else if(eq("SupplySourceVoltage")) return rd_float32(&SupplySourceVoltage);
  else if(eq("MenuLevel")) return rd_byte(&MenuLevel);
  else if(eq("MeasureT_pd")) return rd_float32(&MeasureT_pd);
  else if(eq("MeasureNoise_pd")) return rd_float32(&MeasureNoise_pd);
  else if(eq("MeasureUavr")) return rd_float32(&MeasureUavr);
  else if(eq("MeasureUamp")) return rd_float32(&MeasureUamp);
  else if(eq("MeasureUpeak")) return rd_float32(&MeasureUpeak);

  return FALSE;
}

//---------------------------------------------------------
BYTE DLL_EXPORT Dll_GetFlashByte(DWORD index)
{
  return FlashGetByte_pd(index);
}

//---------------------------------------------------------
void DLL_EXPORT Dll_SetFlashByte(DWORD index, BYTE value)
{
  FlashSetByte_pd(index, value);
}

//---------------------------------------------------------
DWORD DLL_EXPORT Dll_GetFlashSize(void)
{
  return FlashGetSize();
}
