/*---------------------------------------------------------
  pga113.c
---------------------------------------------------------*/

#include "pga113.h"
#include "pga113_pd.h"

const FLOAT32 Pga113K[] = {
  1, 2, 5, 10, 20, 50, 100, 200
};

//---------------------------------------------------------
void Pga113Init(void)
{
  Pga113Init_pd();
}

//---------------------------------------------------------
void Pga113On(void)
{
  Pga113On_pd();
}

//---------------------------------------------------------
void Pga113Off(void)
{
  Pga113Off_pd();
}

//---------------------------------------------------------
void Pga113SetK(FLOAT32 value)
{
  Pga113SetK_pd(value);
}

//---------------------------------------------------------
void Pga113SetKmin(void)
{
  Pga113SetKmin_pd();
}

//---------------------------------------------------------
void Pga113SetKmax(void)
{
  Pga113SetKmax_pd();
}

//---------------------------------------------------------
BOOL Pga113SetKLower(void)
{
  return Pga113SetKLower_pd();
}

//---------------------------------------------------------
BOOL Pga113SetKLower2(void)
{
  return Pga113SetKLower2_pd();
}

//---------------------------------------------------------
BOOL Pga113SetKHigher(void)
{
  return Pga113SetKHigher_pd();
}

//---------------------------------------------------------
FLOAT32 Pga113GetK(void)
{
  return Pga113GetK_pd();
}

//---------------------------------------------------------
void Pga113SetKbyIndex(BYTE index)
{
  Pga113SetKbyIndex_pd(index);
}

//---------------------------------------------------------
BYTE Pga113GetIndexMax(void)
{
  return ARRAY_LENGTH(Pga113K) - 1U;
}

//---------------------------------------------------------
FLOAT32 Pga113GetKbyIndex(BYTE index)
{
  if(index > Pga113GetIndexMax())
    index = Pga113GetIndexMax();
  return Pga113K[index];
}

