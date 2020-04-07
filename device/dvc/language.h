#ifndef LANGUAGE_H_INCLUDED
#define LANGUAGE_H_INCLUDED

/*---------------------------------------------------------
  language.h
---------------------------------------------------------*/

#include "xmain.h"

typedef enum {
  lRussian=0, lEnglish, LANG_COUNT
} TLanguage;

extern NOINIT TLanguage LangId;

void InitLanguage(void);
void ShowLanguageIni(void);
void ShowLanguage(void);

void LoadFirstProducedLanguage(void);

void LoadLanguage(void);
void SaveLanguage(void);

#endif
