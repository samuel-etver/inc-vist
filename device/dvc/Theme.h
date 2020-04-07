#ifndef THEME_H_INCLUDED
#define THEME_H_INCLUDED

/*---------------------------------------------------------
  Theme.h
---------------------------------------------------------*/

#include "xmain.h"

void InitTheme(void);
void ShowThemeIni(void);
void ShowTheme(void);

void LoadTheme(void);
void SaveTheme(void);

void LoadFirstProducedTheme(void);

void ThemeChange(void);

typedef enum {
  themeSimple = 0, themeLight, themeDark,  themeLast
} TTheme;

#define THEME_COUNT         ((BYTE)themeLast)

extern NOINIT TTheme Theme;

#endif

