/*---------------------------------------------------------
  Factory.c
---------------------------------------------------------*/

#include "Factory.h"
#include "ask.h"
#include "language.h"

//---------------------------------------------------------
void ShowFactoryIni(void)
{
  TempWord = 180;
  TempByte = 1;

  ShowAskIni();
}

//---------------------------------------------------------
void ShowFactory(void)
{
  ShowAsk();
}
