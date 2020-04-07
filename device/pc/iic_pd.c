/*---------------------------------------------------------
  iic_pd.c
---------------------------------------------------------*/

#include "iic_pd.h"
#include "iic.h"

static void IicStartWrite_pd(BYTE slave, DWORD address, BYTE address_size);
static void IicStopWrite_pd(void);
static void IicStartRead_pd(BYTE slave, DWORD address, BYTE address_size);
static void IicStopRead_pd(void);
static void IicWriteByte_pd(BYTE data);
static BYTE IicReadByte_pd(void);

//--------------------------------------------------------
void InitIic_pd(void)
{

}

//--------------------------------------------------------
BOOL IicIsSoft_pd(void)
{
  return TRUE;
}

//--------------------------------------------------------
void IicSetSoft_pd(void)
{
  IicStartWrite = IicStartWrite_pd;
  IicStopWrite  = IicStopWrite_pd;
  IicStartRead  = IicStartRead_pd;
  IicStopRead   = IicStopRead_pd;
  IicWriteByte  = IicWriteByte_pd;
  IicReadByte   = IicReadByte_pd;
}

//--------------------------------------------------------
void IicSetHard_pd(void)
{
  IicSetSoft_pd();
}

//--------------------------------------------------------
static void IicStartWrite_pd(BYTE slave, DWORD address, BYTE address_size)
{
}

//---------------------------------------------------------
static void IicStopWrite_pd(void)
{
}

//---------------------------------------------------------
static void IicStartRead_pd(BYTE slave, DWORD address, BYTE address_size)
{
}

//---------------------------------------------------------
static void IicStopRead_pd(void)
{
}

//---------------------------------------------------------
static void IicWriteByte_pd(BYTE data)
{
}

//---------------------------------------------------------
static BYTE IicReadByte_pd(void)
{
  return 0;
}
