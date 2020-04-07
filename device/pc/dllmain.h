#ifndef __MAIN_H__
#define __MAIN_H__

#include <windows.h>
#include "xmain.h"

/*  To use this exported function of dll, include this header
 *  in your project.
 */

#ifdef BUILD_DLL
    #define DLL_EXPORT __declspec(dllexport)
#else
    #define DLL_EXPORT __declspec(dllimport)
#endif


#ifdef __cplusplus
extern "C"
#endif

void DLL_EXPORT Dll_Execute(void);
DWORD DLL_EXPORT Dll_LcdGetPixel(INT32 x, INT32 y);
void DLL_EXPORT Dll_KeybrdSetKeys(WORD keys);
WORD DLL_EXPORT Dll_SystemIsOn(void);

#ifdef __cplusplus
}
#endif

#endif // __MAIN_H__
