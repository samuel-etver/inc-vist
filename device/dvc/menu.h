#ifndef MENU_H_INCLUDED
#define MENU_H_INCLUDED

/*---------------------------------------------------------
  menu.h
---------------------------------------------------------*/

#include "xmain.h"

void InitMenu(void);
void ShowMenuIni(void);
void ShowMenu(void);

void MenuPasswordEntered(void);
void MenuDeviceTypeChanged(void);
void MenuThemeChange(void);

extern NOINIT BYTE MenuLevel;

#endif
