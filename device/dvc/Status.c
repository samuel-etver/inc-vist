/*---------------------------------------------------------
  Status.c
---------------------------------------------------------*/

#include "Status.h"
#include "system.h"
#include "delay.h"
#include "lcd.h"
#include "calendar.h"
#include "SupplySource.h"
#include "utils.h"
#include "usb.h"
#include <math.h>

#define STATUS_ACC_W                12U
#define STATUS_ACC_H                (LCD_STATUS_H - 2)
#define STATUS_ACC_X                1U
#define STATUS_ACC_Y                1
#define STATUS_ACC_BORDER_COLOR     0xA514
#define STATUS_ACC_BODY             (STATUS_ACC_H - 5)
#define STATUS_ACC_LEVEL_FULL       STATUS_ACC_BODY
#define STATUS_ACC_LEVEL1           ((WORD)(STATUS_ACC_LEVEL_FULL*0.0))
#define STATUS_ACC_LEVEL2           ((WORD)(STATUS_ACC_LEVEL_FULL*0.15))
#define STATUS_ACC_LEVEL3           ((WORD)(STATUS_ACC_LEVEL_FULL*0.25))
#define STATUS_ACC_LEVEL1_COLOR     (LCD_RGB_TO_COLOR(0xFF, 0x00, 0x00))
#define STATUS_ACC_LEVEL2_COLOR     (LCD_RGB_TO_COLOR(0xFF, 0xFF, 0x00))
#define STATUS_ACC_LEVEL3_COLOR     (LCD_RGB_TO_COLOR(0x00, 0xFF, 0x00))
#define STATUS_ACC_FONT             FONT08

#define STATUS_DATE_FONT            FONT11

#define STATUS_CHARGE_W             13
#define STATUS_CHARGE_H             15
#define STATUS_CHARGE_X             (STATUS_ACC_X + STATUS_ACC_W + 2U)
#define STATUS_CHARGE_Y             1

#define STATUS_USB_X                80
#define STATUS_USB_Y                0

static NOINIT BOOL     StatusStarted;
static NOINIT DWORD    StatusTimeTicks;
static NOINIT TLcdRect StatusTimeRect;
static NOINIT TLcdRect StatusDateRect;
static NOINIT BYTE     StatusTimeLastSec;
static NOINIT BYTE     StatusDateLastMon;
static NOINIT BYTE     StatusDateLastDay;
static NOINIT BYTE     StatusUsbLastState;
static NOINIT WORD     StatusUsbTicks;
static NOINIT BYTE     StatusPowerLastLevel;
static NOINIT WORD     StatusPowerTicks;
static NOINIT BYTE     StatusPowerTextLastLevel;
static NOINIT TLcdRect StatusPowerTextRect;
static NOINIT BYTE     StatusChargeLastState;
static NOINIT WORD     StatusChargeTicks;

static void StatusDrawAcc(WORD level);

//---------------------------------------------------------
void InitStatus(void)
{
  StatusStarted = FALSE;
}

//---------------------------------------------------------
void StatusRestart(void)
{
  StatusStarted = FALSE;
}

//---------------------------------------------------------
void DoStatus(void)
{
  BOOL    redraw_time;
  BOOL    redraw_date;
  BYTE    usb_state;
  BYTE    charge_state;
  FLOAT32 charge_level;
  WORD    power_level;
  WORD    power_text_level;
  DWORD   ticks;
  BYTE    n;
  WORD    new_right;

  if(!PrgFlags.CentiSec) return;
  if(PrgVerb < VB_MENU)  return;

  redraw_time  = FALSE;
  redraw_date  = FALSE;
  ticks        = GetTicks();

  if(StatusStarted == FALSE) {
    StatusStarted = TRUE;
    redraw_time   = TRUE;
    redraw_date   = TRUE;

    LcdDrawBegin();

    LcdSetFgColor(LcdStatusSepColor);
    LcdDrawHorzLine(0, LCD_STATUS_H - 1, LCD_W);

    LcdSetFont(STATUS_DATE_FONT);
    StatusTimeRect.Bottom =
     (StatusTimeRect.Top  = 2) + LcdFontHeight;
    StatusDateRect.Top    = StatusTimeRect.Top;
    StatusDateRect.Bottom = StatusTimeRect.Bottom;
    StatusDateRect.Right  = LCD_W - 1;
    StatusDateRect.Left   =
     StatusDateRect.Right - LcdGetTextWidth((BYTE*)"00 WWW", 6);
    StatusTimeRect.Right  = StatusDateRect.Left - 1;
    StatusTimeRect.Left =
     StatusTimeRect.Right - LcdGetTextWidth((BYTE*)"00000", 5);

    StatusUsbLastState = 0xFF;
    StatusUsbTicks     = 0;

    LcdSetFont(STATUS_ACC_FONT);
    StatusPowerLastLevel = 0xFF;
    StatusPowerTicks     = 0;
    StatusPowerTextLastLevel  = 0xFF;    
    StatusPowerTextRect.Right =
    StatusPowerTextRect.Left  = STATUS_ACC_X + STATUS_ACC_W + 3U;
    StatusPowerTextRect.Bottom = STATUS_ACC_Y + STATUS_ACC_H - 1;
    StatusPowerTextRect.Top    = StatusTimeRect.Bottom - LcdFontHeight + 4;

    StatusChargeLastState = 0xFF;
    StatusChargeTicks     = 0;

    ReadCalendar();

    LcdDrawEnd();
  }

  if((DWORD)(ticks - StatusTimeTicks) > SYS_CONVERT_MCS(500000L) ||
     redraw_time == TRUE) {
    StatusTimeTicks = ticks;
    ReadCalendar();
    if(CalendarSec != StatusTimeLastSec) {
      StatusTimeLastSec = CalendarSec;
      redraw_time = TRUE;
    }
    if(CalendarMon != StatusDateLastMon ||
       CalendarDay != StatusDateLastDay) {
      StatusDateLastDay = CalendarDay;
      StatusDateLastMon = CalendarMon;
      redraw_date = TRUE;
    }
  }

  if(redraw_time == TRUE) {
    LcdDrawBegin();

    WordToStrWithLeadingZeroes(LcdText,     CalendarHour, 2);
    WordToStrWithLeadingZeroes(LcdText + 4, CalendarMin,  2);
    LcdText[2] = LcdText[3] = ' ';
    LcdSetFont(STATUS_DATE_FONT);
    LcdSetBgColor(LcdStatusBgColor);
    LcdSetTextColor(LcdStatusFgColor);
    LcdDrawLaRectText(&StatusTimeRect, 0, 0, LcdText, 6);
    if(CalendarSec & 1)
      LcdDrawCaText(StatusTimeRect.Left + LcdLettersPos[3],
       StatusTimeRect.Top - 1, (BYTE*)":", 1);

    LcdDrawEnd();
  }

  if(redraw_date == TRUE) {
    LcdDrawBegin();

    CalendarDateToStr2(LcdText, CalendarDay, CalendarMon);
    LcdSetFont(STATUS_DATE_FONT);
    LcdSetBgColor(LcdStatusBgColor);
    LcdSetTextColor(LcdStatusFgColor);
    LcdDrawRaRectText(&StatusDateRect, 0, 0, LcdText, 6);

    LcdDrawEnd();
  }

  if(++StatusUsbTicks == 10) {
    StatusUsbTicks = 0;

    usb_state = 0;
    if(UsbIsInserted() == TRUE) {
      usb_state = UsbFlags.Connected ? 2 : 1;
    }

    if(usb_state != StatusUsbLastState) {
      LcdDrawBegin();
      LcdSetBgColor(LcdStatusBgColor);

      switch(StatusUsbLastState = usb_state) {
        case 1:
          LcdDrawTrPic(STATUS_USB_X, STATUS_USB_Y, PicturesPower);
          break;
        case 2:
          LcdDrawTrPic(STATUS_USB_X, STATUS_USB_Y, PicturesUsb);
          break;
        default:
          LcdSetFgColor(LcdStatusBgColor);
          LcdDrawRect(STATUS_USB_X, STATUS_USB_Y, PicturesUsb[0], PicturesUsb[1]);
      }

      LcdDrawEnd();
    }
  }

  if(++StatusPowerTicks == 11) {
    StatusPowerTicks = 0;

    charge_level = GetSupplySourceChargeLevel();

    power_level = (WORD)roundf(charge_level*STATUS_ACC_LEVEL_FULL);
    if(power_level != StatusPowerLastLevel) {
      if(power_level > STATUS_ACC_LEVEL_FULL)
        power_level = STATUS_ACC_LEVEL_FULL;
      StatusDrawAcc(StatusPowerLastLevel = power_level);
    }

    power_text_level = (WORD)(charge_level*100.0f);
    if(power_text_level != StatusPowerTextLastLevel) {
      if(power_text_level > 100)
        power_text_level = 100;
      StatusPowerTextLastLevel = power_text_level;

      LcdDrawBegin();

      LcdSetFont(STATUS_ACC_FONT);
      LcdSetBgColor(LcdStatusBgColor);
      LcdSetTextColor(LcdStatusFgColor);
      if(power_text_level < 10) {
        LcdText[0] = '0' + power_text_level;
        n = 1;
      }
      else if(power_text_level < 100) {
        WordToStrWithLeadingSpaces(LcdText, power_text_level, n = 2);
      }
      else {
        memcpy(LcdText, "100", n = 3);
      }
      LcdText[n++] = '%';

      new_right = StatusPowerTextRect.Left + LcdGetTextWidth(LcdText, n);
      if(new_right > StatusPowerTextRect.Right)
        StatusPowerTextRect.Right = new_right;

      LcdDrawLaRectText(&StatusPowerTextRect, 0, 0, LcdText, n);

      StatusPowerTextRect.Right = new_right;

      LcdDrawEnd();
    }
  }


  charge_state = 0;
  if(PrgFlags.LowSupplySource)
    charge_state = 1;
  if(PrgFlags.Charging)
    charge_state = 2;

  if(charge_state != StatusChargeLastState) {
    switch(StatusChargeLastState = charge_state) {
      case 1:
      case 2:
        StatusChargeTicks = 0;
        break;
      default:;
        LcdDrawBegin();
        LcdSetFgColor(LcdStatusBgColor);
        LcdDrawRect(STATUS_CHARGE_X, STATUS_CHARGE_Y, STATUS_CHARGE_W,
         STATUS_CHARGE_H);
        LcdDrawEnd();
    }
  }

  switch(StatusChargeLastState) {
    case 1:
    case 2:
      if(++StatusChargeTicks == 1) {
        LcdDrawBegin();
        LcdSetBgColor(LcdStatusBgColor);
        LcdDrawTrPic(STATUS_CHARGE_X, STATUS_CHARGE_Y,
         StatusChargeLastState == 2 ? LcdPictureCharge : PicturesLowPower);
        LcdDrawEnd();
      }
      else if(StatusChargeTicks == 8) {
        if(StatusChargeLastState != 2) {
          LcdDrawBegin();
          LcdSetFgColor(LcdStatusBgColor);
          LcdDrawRect(STATUS_CHARGE_X, STATUS_CHARGE_Y, STATUS_CHARGE_W,
           STATUS_CHARGE_H);
          LcdDrawEnd();
        }
      }
      else if(StatusChargeTicks == 11) {
        StatusChargeTicks = 0;
      }
  }
}

//---------------------------------------------------------
static void StatusDrawAcc(WORD level)
{
  TColor color;
  WORD i;

  LcdDrawBegin();
  LcdBeginDrawRegion(STATUS_ACC_X, STATUS_ACC_Y, STATUS_ACC_W, STATUS_ACC_H);

  if(level > STATUS_ACC_LEVEL3) {
    color = STATUS_ACC_LEVEL3_COLOR;
  }
  else if(level > STATUS_ACC_LEVEL2) {
    color = STATUS_ACC_LEVEL2_COLOR;
  }
  else {
    color = STATUS_ACC_LEVEL1_COLOR;
  }

  LcdSetBgColor(LcdStatusBgColor);
  LcdSetFgColor(color);

  LcdFillBg(3);
  LcdFillFg(STATUS_ACC_W - 2U*3U);
  LcdFillBg(3);

  for(i = 0; i < STATUS_ACC_H - 1 - STATUS_ACC_BODY - 1; i++) {
    LcdFillBg(3);
    LcdFillFg(1);
    LcdFillBg(STATUS_ACC_W - 2U*3U - 2U);
    LcdFillFg(1);
    LcdFillBg(3);
  }

  if(level == STATUS_ACC_BODY) {
    LcdFillFg(STATUS_ACC_W);
  }
  else {
    LcdFillFg(4);
    LcdFillBg(STATUS_ACC_W - 4U*2U);
    LcdFillFg(4);
  }

  for(i = 0; i < STATUS_ACC_BODY - 1 - level; i++) {
    LcdFillFg(1);
    LcdFillBg(STATUS_ACC_W - 2U);
    LcdFillFg(1);
  }
  for(; i < STATUS_ACC_BODY; i++) {
    LcdFillFg(STATUS_ACC_W);
  }

  LcdEndDrawRegion();
  LcdDrawEnd();
}
