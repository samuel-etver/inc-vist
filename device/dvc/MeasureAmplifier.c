/*---------------------------------------------------------
  MeasureAmplifier.h
---------------------------------------------------------*/

#include "MeasureAmplifier.h"
#include "pga113.h"

//---------------------------------------------------------
void MeasureAmplifierInit(void)
{
  Pga113Init();
}

//---------------------------------------------------------
void MeasureAmplifierSetOn(void)
{
  Pga113On();
}

//---------------------------------------------------------
void MeasureAmplifierSetOff(void)
{
  Pga113Off();
}

//---------------------------------------------------------
void MeasureAmplifierSetK(FLOAT32 value)
{
  Pga113SetK(value);
}

//---------------------------------------------------------
void MeasureAmplifierSetKmin(void)
{
  Pga113SetKmin();
}

//---------------------------------------------------------
void MeasureAmplifierSetKmax(void)
{
  Pga113SetKmax();
}

//---------------------------------------------------------
BOOL MeasureAmplifierSetKLower(void)
{
  return Pga113SetKLower();
}

//---------------------------------------------------------
BOOL MeasureAmplifierSetKLower2(void)
{
  return Pga113SetKLower2();
}

//---------------------------------------------------------
BOOL MeasureAmplifierSetKHigher(void)
{
  return Pga113SetKHigher();
}

//---------------------------------------------------------
FLOAT32 MeasureAmplifierGetK(void)
{
  return Pga113GetK();
}

//---------------------------------------------------------
FLOAT32 MeasureAmplifierGetKmin(void)
{
  return MeasureAmplifierGetKbyIndex(0);
}

//---------------------------------------------------------
FLOAT32 MeasureAmplifierGetKmax(void)
{
  return MeasureAmplifierGetKbyIndex(MeasureAmplifierGetIndexMax());
}

//---------------------------------------------------------
void MeasureAmplifierSetKbyIndex(BYTE index)
{
  Pga113SetKbyIndex(index);
}

//---------------------------------------------------------
FLOAT32 MeasureAmplifierGetKbyIndex(BYTE index)
{
  return Pga113GetKbyIndex(index);
}

//---------------------------------------------------------
BYTE MeasureAmplifierGetIndexMax(void)
{
  return Pga113GetIndexMax();
}
