/*---------------------------------------------------------
  pga113_pd.c
---------------------------------------------------------*/

#include "pga113_pd.h"
#include "pga113.h"

NOINIT WORD Pga113Index;

//---------------------------------------------------------
void Pga113Init_pd(void)
{
  Pga113Index = 0;
}

//---------------------------------------------------------
void Pga113On_pd(void)
{
}

//---------------------------------------------------------
void Pga113Off_pd(void)
{
}

//---------------------------------------------------------
void Pga113SetK_pd(FLOAT32 value)
{
}

//---------------------------------------------------------
void Pga113SetKmin_pd(void)
{
  Pga113Index = 0;
}

//---------------------------------------------------------
void Pga113SetKmax_pd(void)
{
  Pga113Index = Pga113GetIndexMax();
}

//---------------------------------------------------------
BOOL Pga113SetKLower_pd(void)
{
  if(Pga113Index) {
    Pga113Index--;
    return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
BOOL Pga113SetKLower2_pd(void)
{
  BOOL f1,f2;
  f1 = Pga113SetKLower_pd();
  f2 = Pga113SetKLower_pd();
  return TO_BOOL(f1 == TRUE || f2 == TRUE);
}

//---------------------------------------------------------
BOOL Pga113SetKHigher_pd(void)
{
  if(Pga113Index < Pga113GetIndexMax()) {
    Pga113Index++;
    return TRUE;
  }
  Pga113Index = Pga113GetIndexMax();
  return FALSE;
}

//---------------------------------------------------------
FLOAT32 Pga113GetK_pd(void)
{
  return 1;
}

//---------------------------------------------------------
void Pga113SetKbyIndex_pd(BYTE index)
{
  if(index > Pga113GetIndexMax())
    index = Pga113GetIndexMax();
  Pga113Index = index;
}
