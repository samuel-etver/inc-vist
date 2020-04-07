/*---------------------------------------------------------
  adxl343_pd.c
---------------------------------------------------------*/

#include "adxl343_pd.h"
#include "adxl343.h"
#include "iic.h"
#include "system.h"

#define ADXL343_INT1_PORT           GPIOB
#define ADXL343_INT1_PIN            GPIO_PIN_6

#define ADXL343_SLAVE_ADDRESS1      0x3A
#define ADXL343_SLAVE_ADDRESS2      0xA6  

#define ADXL343_REG_DEVID           0x00
#define ADXL343_REG_TRESH_TAP       0x1D
#define ADXL343_REG_OFSX            0x1E
#define ADXL343_REG_OFSY            0x1F
#define ADXL343_REG_OFSZ            0x20
#define ADXL343_REG_DUR             0x21
#define ADXL343_REG_LATENT          0x22
#define ADXL343_REG_WINDOW          0x23
#define ADXL343_REG_THRESH_ACT      0x24
#define ADXL343_REG_THRESH_INACT    0x25
#define ADXL343_REG_TIME_INACT      0x26
#define ADXL343_REG_INACT_CTL       0x27
#define ADXL343_REG_THRESH_FF       0x28
#define ADXL343_REG_TIME_FF         0x29
#define ADXL343_REG_TAP_AXES        0x2A
#define ADXL343_REG_ACT_TAP_STATUS  0x2B
#define ADXL343_REG_BW_RATE         0x2C
#define ADXL343_REG_POWER_CTL       0x2D
#define ADXL343_REG_INT_ENABLE      0x2E
#define ADXL343_REG_INT_MAP         0x2F
#define ADXL343_REG_INT_SOURCE      0x30
#define ADXL343_REG_DATA_FORMAT     0x31
#define ADXL343_REG_DATAX0          0x32
#define ADXL343_REG_DATAX1          0x33
#define ADXL343_REG_DATAY0          0x34
#define ADXL343_REG_DATAY1          0x35
#define ADXL343_REG_DATAZ0          0x36
#define ADXL343_REG_DATAZ1          0x37
#define ADXL343_REG_FIFO_CTL        0x38
#define ADXL343_REG_FIFO_STATUS     0x39

static NOINIT BYTE Adxl343Address;

static BOOL Adxl343CheckAvailable_pd(void);
static void Adxl343Write(BYTE reg_addr, BYTE* buff, WORD n);

//---------------------------------------------------------
void Adxl343Init_pd(void)
{
}

//---------------------------------------------------------
void Adxl343Setup_pd(void)
{
  BYTE buff[10];
  
  //Датчик есть, настроить на определение активности на выход INT1
  buff[0] = 0;    // DUR    Запрет тапа
  buff[1] = 0;    // Latent Запрет двойного тапа
  buff[2] = 0;    // Window Запрет двойного тапа
  buff[3] = 4;    // TRESH_ACT Activity threshold (62.5 mG на бит, FF=16 G)
  buff[4] = 10;   // TRESH_INACT Inactivity threshold (62.5 mG на бит, FF=16 G)
  buff[5] = 10;   // TIME_INACT Inactivity time (1 сек на бит)
  buff[6] = 0xF0; // RW_ACT_INACT_CTL 
                  // Axis enable control for activity and inactivity detection
                  // (Определение активности, все оси активны,
                  // определение по AC)
  buff[7] = 7;    // THRESH_FF Free-fall threshold 
                  // (62.5 mG на бит, рекомендуется 5..9, 0.3..0.6 G)
  buff[8] = 0x20; // TIME_FF Free-fall time 
                  // (5 мс на бит, рекомендуется 0x14..0x46, 100..350 мс)
  Adxl343Write(ADXL343_REG_DUR, buff, 9);
  
  buff[0] = 1 << 4; // INT_ENABLE Прерывание активного режима разрешено
  buff[1] = 0;      // INT_MAP
  Adxl343Write(ADXL343_REG_INT_ENABLE, buff, 2);
  
  buff[0] = 1 << 3; // DATA_FORMAT Диапазон 2G, полное разрешение?
  Adxl343Write(ADXL343_REG_DATA_FORMAT, buff, 1);
  
  buff[0] = (1 << 4) | 7; // BW_RATE 12.5 Гц, режим low power (34 мкА)
  buff[1] = 1 << 3;       // POWER_CTL Измерение разрешено
  Adxl343Write(ADXL343_REG_BW_RATE, buff, 2);
}

//---------------------------------------------------------
BOOL Adxl343IsActive_pd(void)
{
  if(GPIO_IS_IN_LOW(ADXL343_INT1_PORT, ADXL343_INT1_PIN))
    return FALSE;
  Adxl343ReadStatus_pd();
  return TRUE;
}

//---------------------------------------------------------
BYTE Adxl343ReadStatus_pd(void)
{
  BYTE b;
  IicStartRead(Adxl343Address, ADXL343_REG_INT_SOURCE, 1);
  b = IicReadByte();
  IicStopRead();
  return b;
}

//---------------------------------------------------------
BOOL Adxl343CheckSlave_pd(void)
{
  BOOL f = FALSE;
  Adxl343Address = ADXL343_SLAVE_ADDRESS1;
  if(IicCheckSlave(ADXL343_SLAVE_ADDRESS1) == TRUE) {
    f = TRUE;   
  }
  else if(IicCheckSlave(ADXL343_SLAVE_ADDRESS2) == TRUE) {
    Adxl343Address = ADXL343_SLAVE_ADDRESS2;
    f = TRUE;
  }  
  if(f == FALSE)
    return FALSE;
  return Adxl343CheckAvailable_pd();
}

//---------------------------------------------------------
static BOOL Adxl343CheckAvailable_pd(void)
{
  WORD i;
  BYTE b;
  for(i = 0; i < 2; i++) {
    IicStartRead(Adxl343Address, ADXL343_REG_DEVID, 1);
    b = IicReadByte();
    IicStopRead();
    if(b == 0xE5) return TRUE;
  }
  return FALSE;
}

//---------------------------------------------------------
static void Adxl343Write(BYTE reg_addr, BYTE* buff, WORD n)
{
  IicStartWrite(Adxl343Address, reg_addr, 1);
  do {
    IicWriteByte(*buff++);
  } while(--n);
  IicStopWrite();
}

