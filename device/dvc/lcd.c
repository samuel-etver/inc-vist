/*---------------------------------------------------------
  lcd.c
---------------------------------------------------------*/

#include "lcd.h"
#include "lcd_pd.h"
#include "system.h"
#include "light.h"
#include "flash.h"
#include "delay.h"
#include "SupplySource.h"
#include "language.h"
#include "keybrd.h"
#include "utils.h"
#include <math.h>

#define LCD_LETTERS_SIZE      100

/*static const WORD LcdPalette[] = {
#include "palette.dat"
};*/
/*static const WORD LcdBalloonPalette[] = {
#include "BalloonPalette.dat"
};*/

NOINIT BYTE LcdText[LCD_TEXT_LEN];
NOINIT TLcdFlags LcdFlags;
static NOINIT BYTE LcdBgColorB0;
static NOINIT BYTE LcdBgColorB1;
static NOINIT BYTE LcdFgColorB0;
static NOINIT BYTE LcdFgColorB1;
static NOINIT BYTE LcdTextColorB0;
static NOINIT BYTE LcdTextColorB1;
NOINIT TColor LcdBgColorDef;
NOINIT TColor LcdFgColorDef;
NOINIT TColor LcdTextColorDef;
NOINIT TColor LcdLabelTextColorDef;
NOINIT TColor LcdCaptionTextColorDef;
NOINIT TColor LcdHintTextColorDef;
NOINIT TColor LcdHintLineColorDef;
NOINIT TColor LcdEvenRowTextColorDef;
NOINIT TColor LcdButtonTextColorDef;
NOINIT TColor LcdSelButtonTextColorDef;
NOINIT TColor LcdEditTextColorDef;
NOINIT TColor LcdGaugeBgColorDef;
NOINIT TColor LcdGaugeFgColorDef;
NOINIT TColor LcdStatusBgColor;
NOINIT TColor LcdStatusFgColor;
NOINIT TColor LcdStatusSepColor;
NOINIT TColor LcdWebLinkColor;
NOINIT BOOL LcdButtonFlashing;
NOINIT const TPicture* LcdPictureCharge;
NOINIT const TBalloon* LcdMenuBalloon;
NOINIT const TBalloon* LcdEditBalloon;
NOINIT const TBalloon* LcdEditSmallBalloon;
NOINIT const TBalloon* LcdSpinDarkBalloon;
NOINIT const TBalloon* LcdSpinLightBalloon;
NOINIT const TBalloon* LcdButtonBalloon;
NOINIT const TBalloon* LcdSelButtonBalloon;
NOINIT WORD LcdLineThickness;
NOINIT BYTE LcdFont;
NOINIT BYTE LcdFontDef;
NOINIT WORD LcdFontHeight;
NOINIT WORD LcdFontAscent;
NOINIT WORD LcdFontDescent;
static NOINIT const BYTE* LcdFontData;
static NOINIT const WORD* LcdFontCharIndexes;
NOINIT WORD LcdLetterSpacing;
NOINIT WORD LcdLettersPos[LCD_LETTERS_SIZE];
NOINIT WORD LcdLettersWidth[LCD_LETTERS_SIZE];
static NOINIT const BYTE* LcdLettersTable[LCD_LETTERS_SIZE];
NOINIT TLcdMenu LcdMenu;
NOINIT TLcdSpinEdit LcdSpinEdit;
NOINIT TLcdGauge LcdGauge;
NOINIT TLcdSlider LcdSlider;
NOINIT TLcdRect LcdRect1;
NOINIT TLcdRect LcdRect2;
NOINIT TLcdRect LcdRect3;
static NOINIT WORD LcdDrawClk;
static NOINIT WORD LcdBlinkClk;
static NOINIT WORD LcdBalloonPalette[256];
static NOINIT WORD LcdGaugeLessPalette[256];
static NOINIT WORD LcdGaugeMorePalette[256];
static NOINIT WORD LcdGaugeRangePalette[256];

static void LcdPowerOn(void);
__ramfunc static WORD LcdBuildLetters(BYTE* txt, BYTE n);
__ramfunc static void LcdDrawLetters(WORD n);
__ramfunc static void LcdDrawRectLetters(const TLcdRect* rect, WORD x, WORD y,
 WORD txt_w, WORD n);
__ramfunc static void LcdDrawBalloonLetters(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD txt_w, WORD n);
static void LcdMenuApplyStyle(TLcdMenu* menu);
static void LcdMenuDrawItem(TLcdMenu* menu, TLcdRect* rect, WORD index);
static BYTE* LcdSpinEditGetText(void* o);
static BYTE* LcdSpinEditSignGetText(void* o);
static void LcdSpinEditResizeEdit(TLcdSpinEdit* spin_edit);
static void LcdSpinEditDrawInactive(TLcdSpinEdit* spin_edit);
static void LcdSpinEditDoNumber(TLcdSpinEdit* spin_edit);
static void LcdSpinEditDoResIds(TLcdSpinEdit* spin_edit);
static void LcdSpinEditDoFunc(TLcdSpinEdit* spin_edit);
static void LcdSpinEditDoExp(TLcdSpinEdit* spin_edit);

//---------------------------------------------------------
void InitLcd(void)
{
  InitLcd_pd();
}
//---------------------------------------------------------
void SetupLcd(void)
{
  LcdPowerOn();
}

//---------------------------------------------------------
void DoLcd(void)
{
  LcdFlags.Repaint = 0;
  LcdFlags.Draw    = 0;
  LcdFlags.Blinked = 0;

  if(!PrgFlags.CentiSec) return;

  if(++LcdDrawClk > 2) {
    LcdDrawClk    = 0;
    LcdFlags.Draw = 1;
  }

  if(LcdFlags.Blink) {
    if(++LcdBlinkClk > 3) {
      LcdBlinkClk      = 0;
      LcdFlags.Blink   = 0;
      LcdFlags.Blinked = 1;
    }
  }
  else {
    if(++LcdBlinkClk > 6) {
      LcdBlinkClk      = 0;
      LcdFlags.Blink   = 1;
      LcdFlags.Blinked = 1;
    }
  }
}

//---------------------------------------------------------
static void LcdPowerOn(void)
{
  LcdPowerOn_pd();
  LcdApplyTheme();
}

//---------------------------------------------------------
void LcdApplyTheme(void)
{
  WORD i;

  switch(Theme) {
    case themeSimple:
      LcdFontDef               = FONT12;
      LcdBgColorDef            = 0xA50F;//LCD_RGB_TO_COLOR(0xBB,0xBB,0xBB);
      LcdFgColorDef            = 0x0000;
      LcdTextColorDef          = 0x0000;
      LcdLabelTextColorDef     = LCD_RGB_TO_COLOR(0x00, 0x00, 0x00);
      LcdCaptionTextColorDef   = LCD_RGB_TO_COLOR(0x64, 0x00, 0xA3);
      LcdHintTextColorDef      = LCD_RGB_TO_COLOR(0x99, 0x00, 0x99);
      LcdHintLineColorDef      = LCD_RGB_TO_COLOR(0x80, 0x80, 0x80);
      LcdEvenRowTextColorDef   = LcdTextColorDef;
      LcdButtonTextColorDef    = LCD_RGB_TO_COLOR(0x00, 0x00, 0x00);
      LcdSelButtonTextColorDef = LCD_RGB_TO_COLOR(0xFF, 0xFF, 0xFF);
      LcdEditTextColorDef      = LCD_RGB_TO_COLOR(0xFF, 0xFF, 0xFF);
      LcdGaugeBgColorDef       = LCD_RGB_TO_COLOR(0xCC, 0xCC, 0xCC);
      LcdGaugeFgColorDef       = LCD_RGB_TO_COLOR(0x11, 0x11, 0xAA);
      LcdStatusBgColor         = LCD_RGB_TO_COLOR(0x00, 0x00, 0x84);
      LcdStatusFgColor         = LCD_RGB_TO_COLOR(0xFF, 0xFF, 0xFF);
      LcdStatusSepColor        = LcdStatusBgColor;
      LcdWebLinkColor          = LCD_RGB_TO_COLOR(0x00, 0x00, 0xFF);
      LcdPictureCharge         = PicturesChargeDark;
      LcdMenuBalloon           = &Balloons36Simple;
      LcdEditBalloon           = &BalloonsEditSimple;
      LcdEditSmallBalloon      = &BalloonsEdit24Simple;
      LcdSpinDarkBalloon       = &BalloonsSmallSpinDarkSimple;
      LcdSpinLightBalloon      = &BalloonsSmallSpinLightSimple;
      LcdButtonBalloon         = &Balloons36DarkSimple;
      LcdSelButtonBalloon      = &Balloons36Simple;
      LcdButtonFlashing        = FALSE;
      for(i = 0; i < 256; i++) {
        LcdBalloonPalette[i]    = LCD_RGB_TO_COLOR(i, i, i);
        LcdGaugeLessPalette[i]  = LCD_RGB_TO_COLOR(i, i, 0);
        LcdGaugeMorePalette[i]  = LCD_RGB_TO_COLOR(i, 0, 0);
        LcdGaugeRangePalette[i] = LCD_RGB_TO_COLOR(0, i, 0);
      }
      break;
    case themeLight:
      LcdFontDef               = FONT12;
      LcdBgColorDef            = 0xFFFF;
      LcdFgColorDef            = 0x0000;
      LcdTextColorDef          = 0x0000;
      LcdLabelTextColorDef     = LCD_RGB_TO_COLOR(0x80, 0x80, 0x80);
      LcdCaptionTextColorDef   = LCD_RGB_TO_COLOR(0x64, 0x00, 0xA3);
      LcdHintTextColorDef      = LCD_RGB_TO_COLOR(0x99, 0x00, 0x99);
      LcdHintLineColorDef      = LCD_RGB_TO_COLOR(0x80, 0x80, 0x80);
      LcdEvenRowTextColorDef   = LCD_RGB_TO_COLOR(0x64, 0x64, 0x64);
      LcdButtonTextColorDef    = LCD_RGB_TO_COLOR(0x00, 0x00, 0xFF);
      LcdSelButtonTextColorDef = LCD_RGB_TO_COLOR(0x00, 0x00, 0x00);
      LcdEditTextColorDef      = LCD_RGB_TO_COLOR(0x44, 0xFF, 0x44);
      LcdGaugeBgColorDef       = LCD_RGB_TO_COLOR(0xCC, 0xCC, 0xCC);
      LcdGaugeFgColorDef       = LCD_RGB_TO_COLOR(0x11, 0x11, 0xAA);
      LcdStatusBgColor         = LcdBgColorDef;
      LcdStatusFgColor         = LcdTextColorDef;
      LcdStatusSepColor        = LcdHintLineColorDef;
      LcdWebLinkColor          = LCD_RGB_TO_COLOR(0x00, 0x00, 0xFF);
      LcdPictureCharge         = PicturesChargeLight;
      LcdMenuBalloon           = &Balloons36;
      LcdEditBalloon           = &BalloonsEdit;
      LcdEditSmallBalloon      = &BalloonsEdit24;
      LcdSpinDarkBalloon       = &BalloonsSmallSpinDark;
      LcdSpinLightBalloon      = &BalloonsSmallSpinLight;
      LcdButtonBalloon         = &Balloons36Dark;
      LcdSelButtonBalloon      = &Balloons36;
      LcdButtonFlashing        = TRUE;
      for(i = 0; i < 256; i++) {
        LcdBalloonPalette[i]    = LCD_RGB_TO_COLOR(0, i, i);
        LcdGaugeLessPalette[i]  = LCD_RGB_TO_COLOR(i, i, 0);
        LcdGaugeMorePalette[i]  = LCD_RGB_TO_COLOR(i, 0, 0);
        LcdGaugeRangePalette[i] = LCD_RGB_TO_COLOR(0, i, 0);
      }
      break;
    default:
      LcdFontDef               = FONT12;
      LcdBgColorDef            = 0x0000;
      LcdFgColorDef            = 0xFFFF;
      LcdTextColorDef          = 0xFFFF;
      LcdLabelTextColorDef     = LCD_RGB_TO_COLOR(0x80, 0x80, 0x80);
      LcdCaptionTextColorDef   = LCD_RGB_TO_COLOR(255, 255, 0);
      LcdHintTextColorDef      = LCD_RGB_TO_COLOR(0x00, 0xFF, 0xCC);
      LcdHintLineColorDef      = LCD_RGB_TO_COLOR(0x40, 0x40, 0x40);
      LcdEvenRowTextColorDef   = LCD_RGB_TO_COLOR(0xAA, 0xAA, 0xAA);
      LcdButtonTextColorDef    = LCD_RGB_TO_COLOR(0xFF, 0x00, 0x00);
      LcdSelButtonTextColorDef = LCD_RGB_TO_COLOR(0x00, 0x00, 0x00);
      LcdEditTextColorDef      = LCD_RGB_TO_COLOR(0xFF, 0xAA, 0xAA);
      LcdGaugeBgColorDef       = LCD_RGB_TO_COLOR(0x33, 0x33, 0x33);
      LcdGaugeFgColorDef       = LCD_RGB_TO_COLOR(0x88, 0x88, 0xFF);
      LcdStatusBgColor         = LcdBgColorDef;
      LcdStatusFgColor         = LcdTextColorDef;
      LcdStatusSepColor        = LcdHintLineColorDef;
      LcdWebLinkColor          = LCD_RGB_TO_COLOR(0x66, 0xDD, 0xFF);
      LcdPictureCharge         = PicturesChargeDark;
      LcdMenuBalloon           = &Balloons36;
      LcdEditBalloon           = &BalloonsEdit;
      LcdEditSmallBalloon      = &BalloonsEdit24;
      LcdSpinDarkBalloon       = &BalloonsSmallSpinDark;
      LcdSpinLightBalloon      = &BalloonsSmallSpinLight;
      LcdButtonBalloon         = &Balloons36Dark;
      LcdSelButtonBalloon      = &Balloons36;
      LcdButtonFlashing        = TRUE;
      for(i = 0; i < 256; i++) {
        LcdBalloonPalette[i]    = LCD_RGB_TO_COLOR(i, i, i);
        LcdGaugeLessPalette[i]  = LCD_RGB_TO_COLOR(i, i, 0);
        LcdGaugeMorePalette[i]  = LCD_RGB_TO_COLOR(i, 0, 0);
        LcdGaugeRangePalette[i] = LCD_RGB_TO_COLOR(0, i, 0);
      }
  }
}

//---------------------------------------------------------
TColor LcdRgbToColor(BYTE r, BYTE g, BYTE b)
{
  return LCD_RGB_TO_COLOR(r, g, b);
}

//---------------------------------------------------------
TColor LcdGetBgColor(void)
{
  return (TColor)(LcdBgColorB0 | 0x100*LcdBgColorB1);
}

//---------------------------------------------------------
void LcdSetBgColor(TColor color)
{
  LcdBgColorB0 = (BYTE)(color);
  LcdBgColorB1 = (BYTE)(color >> 8);
}

//---------------------------------------------------------
void LcdSetBgColorDef(void)
{
  LcdSetBgColor(LcdBgColorDef);
}

//---------------------------------------------------------
TColor LcdGetFgColor(void)
{
  return (TColor)(LcdFgColorB0 | 0x100*LcdFgColorB1);
}

//---------------------------------------------------------
void LcdSetFgColor(TColor color)
{
  LcdFgColorB0 = (BYTE)(color);
  LcdFgColorB1 = (BYTE)(color >> 8);
}

//---------------------------------------------------------
void LcdSetFgColorDef(void)
{
  LcdSetFgColor(LcdFgColorDef);
}

//---------------------------------------------------------
TColor LcdGetTextColor(void)
{
  return (TColor)(LcdTextColorB0 | 0x100*LcdTextColorB1);
}

//---------------------------------------------------------
void LcdSetTextColor(TColor color)
{
  LcdTextColorB0 = (BYTE)(color);
  LcdTextColorB1 = (BYTE)(color >> 8);
}

//---------------------------------------------------------
void LcdSetTextColorDef(void)
{
  LcdSetTextColor(LcdTextColorDef);
}

//---------------------------------------------------------
void LcdSetLabelTextColorDef(void)
{
  LcdSetTextColor(LcdLabelTextColorDef);
}

//---------------------------------------------------------
void LcdSetEvenRowTextColorDef(void)
{
  LcdSetTextColor(LcdEvenRowTextColorDef);
}

//---------------------------------------------------------
void LcdSetFont(BYTE font)
{
  LcdFontHeight = FontsGetHeight(LcdFont = font);
  LcdFontAscent = FontsGetAscent(font);
  LcdFontDescent = FontsGetDescent(font);
  LcdFontData = FontsGetData(font);
  LcdFontCharIndexes = FontsGetCharIndexes(font);
}

//---------------------------------------------------------
void LcdSetFontDef(void)
{
  LcdSetFont(LcdFontDef);
}

//---------------------------------------------------------
void LcdSetLineThickness(WORD thickness)
{
  LcdLineThickness = thickness;
}

//---------------------------------------------------------
void LcdSetLetterSpacing(BYTE spacing)
{
  LcdLetterSpacing = spacing;
}

//---------------------------------------------------------
void LcdSetLetterSpacingDef(void)
{
  LcdSetLetterSpacing(LCD_LETTER_SPACING_DEF);
}

//---------------------------------------------------------
WORD LcdGetTextWidth(BYTE* txt, BYTE n)
{
  return LcdBuildLetters(txt, n);
}

//---------------------------------------------------------
WORD LcdGetResTextWidth(WORD id)
{
  return LcdGetTextWidth(LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
WORD LcdGetLangTextWidth(WORD id)
{
  return LcdGetResTextWidth(id + LangId);
}

//---------------------------------------------------------
WORD LcdGetLangTextWidthMax(WORD id, WORD n)
{
  WORD i;
  WORD w;
  WORD w_max = 0;

  id += LangId;

  for(i = 0; i < n; i++) {
    w = LcdGetResTextWidth(id);
    if(w > w_max)
      w_max = w;
    id += LANG_COUNT;
  }

  return w_max;
}
//---------------------------------------------------------
void LcdDrawBegin(void)
{
  LcdSetBgColorDef();
  LcdSetFgColorDef();
  LcdSetTextColorDef();
  LcdSetLineThickness(LCD_LINE_THICKNESS_DEF);
  LcdSetLetterSpacing(LCD_LETTER_SPACING_DEF);
  LcdSetFontDef();
  LcdDrawBegin_pd();
}

//---------------------------------------------------------
void LcdDrawEnd(void)
{
  LcdDrawEnd_pd();
}

//---------------------------------------------------------
void LcdDrawBackground(void)
{
  TColor saved_bg = LcdGetBgColor();
  LcdBeginDrawRegion(0, 0, LCD_W, LCD_H);
  LcdSetBgColor(LcdStatusBgColor);
  LcdFillBg(LCD_W*LCD_STATUS_H);
  LcdSetBgColor(saved_bg);
  LcdFillBg(LCD_W*(LCD_H/2 - LCD_STATUS_H));
  LcdFillBg(LCD_W*LCD_H/2);
  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawWorkspace(void)
{
  LcdBeginDrawRegion(0, LCD_STATUS_H + 1, LCD_W, LCD_H - LCD_STATUS_H - 1);
  LcdFillBg((LCD_H - LCD_STATUS_H - 1)*LCD_W/2);
  LcdFillBg((LCD_H - LCD_STATUS_H - 1)*LCD_W/2);
  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawWorkspaceWithLangCaption(WORD id)
{
  LcdDrawWorkspace();
  LcdDrawLangCaption(id);
}

//---------------------------------------------------------
void LcdDrawDot(WORD x, WORD y)
{
  LcdBeginDrawRegion(x, y, 1, 1);
  LCD_WRITE_COLOR2(LcdFgColorB0, LcdFgColorB1);
  LcdEndDrawRegion();
}

//---------------------------------------------------------
DWORD LcdGetPixel(WORD x, WORD y)
{
  DWORD clr;
  LcdBeginReadRegion(x, y, 1, 1);
  clr = LcdGetPixel_pd();
  LcdEndReadRegion();
  return clr;
}

//---------------------------------------------------------
void LcdGetPixels(WORD x, WORD y, WORD w, WORD h, BYTE* buff)
{
  LcdBeginReadRegion(x, y, w, h);
  LcdGetPixels_pd(buff, w*h);
  LcdEndReadRegion();
}

//---------------------------------------------------------
void LcdDrawLine(INT16 x0, INT16 y0, INT16 x1, INT16 y1)
{
  LcdDrawLineImpl(x0, y0, x1, y1, LcdDrawDot);
}

//---------------------------------------------------------
void LcdDrawLineImpl(INT16 x0, INT16 y0, INT16 x1, INT16 y1,
 void (*proc)(WORD x, WORD y))
{
  INT16 increment_x;
  INT16 increment_y;
  INT16 distance_x;
  INT16 distance_y;
  INT16 distance_min;
  INT16 distance_max;
  INT16 correction;
  INT16 correction_st;
  INT16 correction_di;

  if(x1 > x0) {
    increment_x = 1;
    distance_x = x1 - x0;
  }
  else {
    increment_x = -1;
    distance_x = x0 - x1;
  }


  if(y1 > y0) {
    increment_y = 1;
    distance_y = y1 - y0;
  }
  else {
    increment_y = -1;
    distance_y = y0 - y1;
  }


  if(distance_x > distance_y) {
    distance_min = distance_y;
    distance_max = distance_x;
  }
  else {
    distance_min = distance_x;
    distance_max = distance_y;
  }


  correction_st = (WORD)distance_min*2;
  correction = (INT16)correction_st - distance_max;
  correction_di = correction - distance_max;


  while(distance_max--) {
    (*proc)(x0, y0);

    if(correction < 0) {
      correction += correction_st;
      if(distance_x > distance_y)
        x0 += increment_x;
      else
        y0 += increment_y;
   }
   else {
     correction += correction_di;
     x0 += increment_x;
     y0 += increment_y;
   }
  }
}

//---------------------------------------------------------
void LcdDrawHorzLine(WORD x, WORD y, WORD len)
{
  LcdDrawRect(x, y, len, LcdLineThickness);
}

//---------------------------------------------------------
void LcdDrawVertLine(WORD x, WORD y, WORD len)
{
  LcdDrawRect(x, y, LcdLineThickness, len);
}

//---------------------------------------------------------
void LcdDrawRect(WORD x, WORD y, WORD w, WORD h)
{
  WORD i = w*h;
  LcdBeginDrawRegion(x, y, w, h);
  do {
    LCD_WRITE_COLOR2(LcdFgColorB0, LcdFgColorB1);
  } while(--i);
  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawFrame(WORD x, WORD y, WORD w, WORD h)
{
  LcdDrawHorzLine(x, y, w);
  LcdDrawHorzLine(x, y + h - LcdLineThickness, w);
  LcdDrawVertLine(x, y, h);
  LcdDrawVertLine(x + w - LcdLineThickness, y, h);
}

//---------------------------------------------------------
void LcdDrawPic8(WORD x, WORD y, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawPic16(WORD x, WORD y, const TPicture16* pic)
{
  WORD w = *pic;
  WORD h = *++pic;
  WORD i = w*h;
  TColor color;
  BYTE b0;
  BYTE b1;

  LcdBeginDrawRegion(x, y, w, h);

  do {
    color = *++pic;
    b0 = (BYTE)(color);
    b1 = (BYTE)(color >> 8);
    LCD_WRITE_COLOR2(b0, b1);
  } while(--i);

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawTrPic8(WORD x, WORD y, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawTrPic16(WORD x, WORD y, const TPicture16* pic)
{
  WORD w = *pic;
  WORD h = *++pic;
  WORD i = w*h;
  TColor color;
  BYTE b0;
  BYTE b1;

  LcdBeginDrawRegion(x, y, w, h);

  do {
    color = *++pic;
    if(color == LCD_TRANSPARENT_COLOR16) {
      LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
    }
    else {
      b0 = (BYTE)(color);
      b1 = (BYTE)(color >> 8);
      LCD_WRITE_COLOR2(b0, b1);
    }
  } while(--i);

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaRectPic8(TLcdRect* rect, WORD x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaRectPic16(TLcdRect* rect, WORD x, const TPicture16* pic)
{
  WORD w = rect->Right - rect->Left;
  WORD h = rect->Bottom - rect->Top;
  WORD pic_w = *pic;
  WORD pic_h = *++pic;
  WORD x_tmp;
  WORD y_tmp;
  WORD margin_top = (h - pic_h)/2;
  WORD margin_bottom = h - margin_top - pic_h;
  WORD margin_right = w - x - pic_w;
  TColor color;

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  if(margin_top)
    LcdFillBg(margin_top*w);

  for(y_tmp = 0; y_tmp < pic_h; y_tmp++) {
    if(x)
      LcdFillBg(x);

    for(x_tmp = 0; x_tmp < pic_w; x_tmp++) {
      color = *++pic;
      if(color == LCD_TRANSPARENT_COLOR16) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        LCD_WRITE_COLOR1(color);
      }
    }

    if(margin_right)
      LcdFillBg(margin_right);
  }

  if(margin_bottom)
    LcdFillBg(margin_bottom*w);

  LcdEndDrawRegion();
}

//---------------------------------------------------------
__ramfunc void LcdFillBg(WORD n)
{
  do {
    LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
  } while(--n);
}

//---------------------------------------------------------
__ramfunc void LcdFillFg(WORD n)
{
  do {
    LCD_WRITE_COLOR2(LcdFgColorB0, LcdFgColorB1);
  } while(--n);
}

//---------------------------------------------------------
__ramfunc static WORD LcdBuildLetters(BYTE* txt, BYTE n)
{
  WORD i;
  const BYTE* letter_data;
  WORD pos = 0;

  if(!n) return 0;

  for(i = 0; i < n; i++) {
    letter_data = LcdFontData + LcdFontCharIndexes[*txt++];
    LcdLettersTable[i] = letter_data + 1;
    LcdLettersPos[i]   = pos;
    pos += LcdLetterSpacing + (LcdLettersWidth[i] = letter_data[0]);
  }

  return pos - LcdLetterSpacing;
}

//---------------------------------------------------------
__ramfunc static void LcdDrawLetters(WORD n)
{
  WORD i, j;
  const BYTE *tbl;
  WORD letter_w;
  WORD row_size;
  BYTE mask;

  for(i = 0; i < LcdFontHeight; i++) {
    for(j = 0; j < n; j++) {
      letter_w = LcdLettersWidth[j];
      row_size = letter_w >> 3U;
      if(letter_w & 0x7)
        row_size++;
      tbl = LcdLettersTable[j] + row_size*i;
      mask = 0x01;
      do {
        if(mask & *tbl) {
          LCD_WRITE_COLOR2(LcdTextColorB0, LcdTextColorB1);
        }
        else {
          LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
        }
        if((mask <<= 1) == 0) {
          mask = 0x01;
          tbl++;
        }
      } while(--letter_w);

      if(j != n - 1U)
        if(LcdLetterSpacing)
          LcdFillBg(LcdLetterSpacing);
    }
  }
}

//---------------------------------------------------------
void LcdDrawLaText(WORD x, WORD y, BYTE* txt, BYTE n)
{
  WORD w;
  if((w = LcdBuildLetters(txt, n))) {
    LcdBeginDrawRegion(x, y, w, LcdFontHeight);
    LcdDrawLetters(n);
    LcdEndDrawRegion();
  }
}

//---------------------------------------------------------
void LcdDrawRaText(WORD x, WORD y, BYTE* txt, BYTE n)
{
  WORD w;
  if((w = LcdBuildLetters(txt, n))) {
    LcdBeginDrawRegion(x - w, y, w, LcdFontHeight);
    LcdDrawLetters(n);
    LcdEndDrawRegion();
  }
}

//---------------------------------------------------------
void LcdDrawCaText(WORD x, WORD y, BYTE* txt, BYTE n)
{
  WORD w;
  if((w = LcdBuildLetters(txt, n))) {
    LcdBeginDrawRegion(x - w/2, y, w, LcdFontHeight);
    LcdDrawLetters(n);
    LcdEndDrawRegion();
  }
}

//---------------------------------------------------------
void LcdDrawLaResText(WORD x, WORD y, WORD id)
{
  LcdDrawLaText(x, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawRaResText(WORD x, WORD y, WORD id)
{
  LcdDrawRaText(x, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawCaResText(WORD x, WORD y, WORD id)
{
  LcdDrawCaText(x, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawLaLangText(WORD x, WORD y, WORD id)
{
  LcdDrawLaResText(x, y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawRaLangText(WORD x, WORD y, WORD id)
{
  LcdDrawRaResText(x, y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawCaLangText(WORD x, WORD y, WORD id)
{
  LcdDrawCaResText(x, y, id + LangId);
}

//---------------------------------------------------------
__ramfunc static void LcdDrawRectLetters(const TLcdRect* rect, WORD x, WORD y,
 WORD txt_w, WORD n)
{
  WORD i, j;
  const BYTE *tbl;
  WORD letter_w, letter_h;
  BYTE mask;
  WORD row_size;
  WORD w = rect->Right - rect->Left;
  WORD h = rect->Bottom - rect->Top;
  WORD margin_bottom;
  WORD curr_x;
  WORD tmp;

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  letter_h = LcdFontHeight;
  if(y + letter_h > h)
    letter_h = y >= h ? 0 : (h - y);
  margin_bottom = h - y - letter_h;

  if(y)
    LcdFillBg(w*y);

  for(i = 0; i < letter_h; i++) {
    curr_x = x;
    if(curr_x)
      LcdFillBg(curr_x);

    for(j = 0; j < n; j++) {
      letter_w = LcdLettersWidth[j];
      row_size = letter_w >> 3U;
      if(letter_w & 0x7)
        row_size++;
      tbl = LcdLettersTable[j] + row_size*i;
      mask = 0x01;
      do {
        if(curr_x == w) break;

        if(mask & *tbl) {
          LCD_WRITE_COLOR2(LcdTextColorB0, LcdTextColorB1);
        }
        else {
          LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
        }
        if((mask <<= 1) == 0) {
          mask = 0x01;
          tbl++;
        }

        curr_x++;
      } while(--letter_w);

      tmp = 0;
      if(j != n - 1U)
        if(curr_x < w && LcdLetterSpacing) {
          tmp = w - curr_x;
          if(tmp > LcdLetterSpacing)
            tmp = LcdLetterSpacing;
          LcdFillBg(tmp);
        }
      curr_x += tmp;
    }

    if(curr_x < w)
      LcdFillBg(w - curr_x);
  }

  if(margin_bottom)
    LcdFillBg(w*margin_bottom);

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaRectText(const TLcdRect* rect, WORD x, WORD y, BYTE* txt, BYTE n)
{
  LcdDrawRectLetters(rect, x, y, LcdBuildLetters(txt, n), n);
}

//---------------------------------------------------------
void LcdDrawRaRectText(const TLcdRect* rect, WORD x, WORD y, BYTE* txt, BYTE n)
{
  WORD txt_w = LcdBuildLetters(txt, n);
  LcdDrawRectLetters(rect, rect->Right - rect->Left - txt_w - x, y, txt_w, n);
}

//---------------------------------------------------------
void LcdDrawCaRectText(const TLcdRect* rect, WORD y, BYTE* txt, BYTE n)
{
  WORD txt_w = LcdBuildLetters(txt, n);
  LcdDrawRectLetters(rect, (rect->Right - rect->Left - txt_w)/2, y, txt_w, n);
}

//---------------------------------------------------------
void LcdDrawLaRectResText(const TLcdRect* rect, WORD x, WORD y, WORD id)
{
  LcdDrawLaRectText(rect, x, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawRaRectResText(const TLcdRect* rect, WORD x, WORD y, WORD id)
{
  LcdDrawRaRectText(rect, x, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawCaRectResText(const TLcdRect* rect, WORD y, WORD id)
{
  LcdDrawCaRectText(rect, y, LcdText, LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawLaRectLangText(const TLcdRect* rect, WORD x, WORD y, WORD id)
{
  LcdDrawLaRectResText(rect, x, y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawRaRectLangText(const TLcdRect* rect, WORD x, WORD y, WORD id)
{
  LcdDrawRaRectResText(rect, x, y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawCaRectLangText(const TLcdRect* rect, WORD y, WORD id)
{
  LcdDrawCaRectResText(rect, y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawLaRectTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 BYTE* txt, BYTE n, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaRectResTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaRectLangTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaRectTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 BYTE* txt, BYTE n, WORD pic_x, const TPicture16* pic)
{
  TLcdRect r;
  WORD txt_w;

  txt_w   = LcdBuildLetters(txt, n);
  r       = *rect;
  r.Right = r.Left + x + txt_w;
  LcdDrawRectLetters(&r, x, y, txt_w, n);

  r.Left  = r.Right;
  r.Right = rect->Right;
  LcdDrawLaRectPic16(&r, pic_x - (r.Left - rect->Left), pic);
}

//---------------------------------------------------------
void LcdDrawLaRectResTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture16* pic)
{
  LcdDrawLaRectTextWithTrPic16(rect, x, y, LcdText,
   LoadTextResource(LcdText, id), pic_x, pic);
}

//---------------------------------------------------------
void LcdDrawLaRectLangTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture16* pic)
{
  LcdDrawLaRectResTextWithTrPic16(rect, x, y, id + LangId, pic_x, pic);
}

//---------------------------------------------------------
__ramfunc static void LcdDrawBalloonLetters(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD txt_w, WORD n)
{
  WORD w = rect->Right - rect->Left;
  WORD h = balloon->Height;
  WORD balloon_x0 = balloon->SideWidth;
  WORD balloon_x1 = w - balloon_x0;
  const BYTE* balloon_body_data = balloon->BodyData - 1;
  const BYTE* balloon_left_side_data = balloon->LeftSideData - 1;
  const BYTE* balloon_right_side_data = balloon->RightSideData - 1;
  WORD txt_h = LcdFontHeight;
  WORD txt_x0 = x;
  WORD txt_y0 = y;
  WORD txt_y1 = txt_y0 + txt_h;
  WORD x_tmp, y_tmp;
  BYTE b0;
  TColor color;
  const BYTE *tbl;
  WORD letter_w;
  BYTE mask;
  BYTE row_size;
  WORD i, j;

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  for(y_tmp = 0; y_tmp < h; y_tmp++) {
    for(x_tmp = 0; x_tmp < balloon_x0 && x_tmp < txt_x0; x_tmp++) {
      if((b0 = *++balloon_left_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }


    color = LcdBalloonPalette[*++balloon_body_data];
    for(; x_tmp < txt_x0; x_tmp++) {
      LCD_WRITE_COLOR1(color);
    }

    if(y_tmp >= txt_y0 && y_tmp < txt_y1) {
      for(i = 0; i < n; i++) {
        letter_w = LcdLettersWidth[i];
        row_size = letter_w >> 3U;
        if(letter_w & 0x07)
          row_size++;
        tbl = LcdLettersTable[i] + row_size*(y_tmp - txt_y0);
        mask = 0x01;
        do {
          if(mask & *tbl) {
            LCD_WRITE_COLOR2(LcdTextColorB0, LcdTextColorB1);
            if(x_tmp < balloon_x0)
              ++balloon_left_side_data;
            else if(x_tmp >= balloon_x1)
              ++balloon_right_side_data;
          }
          else {
            if(x_tmp < balloon_x0) {
              if((b0 = *++balloon_left_side_data) == LCD_TRANSPARENT_COLOR8) {
                LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
              }
              else {
                LCD_WRITE_COLOR1(LcdBalloonPalette[b0]);
              }
            } else if(x_tmp >= balloon_x1) {
              if((b0 = *++balloon_right_side_data) == LCD_TRANSPARENT_COLOR8) {
                LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
              }
              else {
                LCD_WRITE_COLOR1(LcdBalloonPalette[b0]);
              }
            }
            else {
              LCD_WRITE_COLOR1(color);
            }
          }
          if((mask <<= 1) == 0) {
            mask = 0x01;
            tbl++;
          }
          x_tmp++;
        } while(--letter_w);

        if(i != n - 1U) {
          for(j = 0; j < LcdLetterSpacing; j++) {
            LCD_WRITE_COLOR1(color);
          }
          x_tmp += LcdLetterSpacing;
        }
      }
    }

    for(; x_tmp < balloon_x0; x_tmp++) {
      if((b0 = *++balloon_left_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        LCD_WRITE_COLOR1(LcdBalloonPalette[b0]);
      }
    }

    for(; x_tmp < balloon_x1; x_tmp++) {
      LCD_WRITE_COLOR1(color);
    }

    for(; x_tmp < w; x_tmp++) {
      if((b0 = *++balloon_right_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaBalloonText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n)
{
  LcdDrawBalloonLetters(rect, x, y, balloon, LcdBuildLetters(txt, n), n);
}

//---------------------------------------------------------
void LcdDrawRaBalloonText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n)
{
  WORD txt_w = LcdBuildLetters(txt, n);
  LcdDrawBalloonLetters(rect, rect->Right - rect->Left - x - txt_w, y,
   balloon, LcdBuildLetters(txt, n), n);
}

//---------------------------------------------------------
void LcdDrawCaBalloonText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n)
{
  WORD txt_w = LcdBuildLetters(txt, n);
  LcdDrawBalloonLetters(rect, (rect->Right - rect->Left - txt_w)/2, y,
   balloon, LcdBuildLetters(txt, n), n);
}

//---------------------------------------------------------
void LcdDrawLaBalloonResText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawLaBalloonText(rect, x, y, balloon, LcdText,
   LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawRaBalloonResText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawRaBalloonText(rect, x, y, balloon, LcdText,
   LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawCaBalloonResText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawCaBalloonText(rect, y, balloon, LcdText,
   LoadTextResource(LcdText, id));
}

//---------------------------------------------------------
void LcdDrawLaBalloonLangText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawLaBalloonResText(rect, x, y, balloon, id + LangId);
}

//---------------------------------------------------------
void LcdDrawRaBalloonLangText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawRaBalloonResText(rect, x, y, balloon, id + LangId);
}

//---------------------------------------------------------
void LcdDrawCaBalloonLangText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, WORD id)
{
  LcdDrawCaBalloonResText(rect, y, balloon, id + LangId);
}

//---------------------------------------------------------
void LcdDrawCaBalloonPic8(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawCaBalloonPic16(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture16* pic)
{
  WORD w = rect->Right - rect->Left;
  WORD h = balloon->Height;
  WORD balloon_x0 = balloon->SideWidth;
  WORD balloon_x1 = w - balloon_x0;
  const BYTE* balloon_data;
  WORD pic_w = *pic;
  WORD pic_h = *++pic;
  WORD pic_x0 = (w - pic_w)/2;
  WORD pic_x1 = pic_x0 + pic_w;
  WORD pic_y0 = (h - pic_h)/2;
  WORD pic_y1 = pic_y0 + pic_h;
  WORD x_tmp, y_tmp;
  BYTE b0;
  TColor color;

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  balloon_data = balloon->LeftSideData - 1;
  for(x_tmp = 0; x_tmp < balloon_x0; x_tmp++) {
    for(y_tmp = h; y_tmp; y_tmp--) {
      if((b0 = *++balloon_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }
  }

  for(; x_tmp < pic_x0; x_tmp++) {
    balloon_data = balloon->BodyData - 1;
    for(y_tmp = h; y_tmp; y_tmp--) {
      color = LcdBalloonPalette[*++balloon_data];
      LCD_WRITE_COLOR1(color);
    }
  }

  for(; x_tmp < pic_x1; x_tmp++) {
    balloon_data = balloon->BodyData - 1;
    for(y_tmp = h; y_tmp > pic_y1; y_tmp--) {
      color = LcdBalloonPalette[*++balloon_data];
      LCD_WRITE_COLOR1(color);
    }
    for(; y_tmp > pic_y0; y_tmp--) {
      ++balloon_data;
      color = *++pic;
      LCD_WRITE_COLOR1(color);
    }
    for(; y_tmp; y_tmp--) {
      color = LcdBalloonPalette[*++balloon_data];
      LCD_WRITE_COLOR1(color);
    }
  }

  for(; x_tmp < balloon_x1; x_tmp++) {
    balloon_data = balloon->BodyData - 1;
    for(y_tmp = h; y_tmp; y_tmp--) {
      color = LcdBalloonPalette[*++balloon_data];
      LCD_WRITE_COLOR1(color);
    }
  }

  balloon_data = balloon->RightSideData - 1;
  for(; x_tmp < w; x_tmp++) {
    for(y_tmp = h; y_tmp; y_tmp--) {
      b0 = *++balloon_data;
      if(b0 == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawCaBalloonTrPic8(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawCaBalloonTrPic16(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture16* pic)
{
  WORD w = rect->Right - rect->Left;
  WORD h = balloon->Height;
  WORD balloon_x0 = balloon->SideWidth;
  WORD balloon_x1 = w - balloon_x0;
  const BYTE* balloon_body_data = balloon->BodyData - 1;
  const BYTE* balloon_left_side_data = balloon->LeftSideData - 1;
  const BYTE* balloon_right_side_data = balloon->RightSideData - 1;
  WORD x_tmp, y_tmp;
  BYTE b0;
  TColor color;
  WORD pic_w = *pic;
  WORD pic_h = *++pic;
  WORD pic_x0 = (w - pic_w)/2;
  WORD pic_x1 = pic_x0 + pic_w;
  WORD pic_y0 = (h - pic_h)/2;
  WORD pic_y1 = pic_y0 + pic_h;
  TColor pic_color;

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  for(y_tmp = 0; y_tmp < h; y_tmp++) {
    for(x_tmp = 0; x_tmp < balloon_x0; x_tmp++) {
      if((b0 = *++balloon_left_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }


    color = LcdBalloonPalette[*++balloon_body_data];

    if(y_tmp >= pic_y0 && y_tmp < pic_y1) {
      for(; x_tmp < pic_x0; x_tmp++) {
        LCD_WRITE_COLOR1(color);
      }
      for(; x_tmp < pic_x1; x_tmp++) {
        if((pic_color = *++pic) == LCD_TRANSPARENT_COLOR16) {
          LCD_WRITE_COLOR1(color);
        }
        else {
          LCD_WRITE_COLOR1(pic_color);
        }
      }
    }


    for(; x_tmp < balloon_x1; x_tmp++) {
      LCD_WRITE_COLOR1(color);
    }

    for(; x_tmp < w; x_tmp++) {
      if((b0 = *++balloon_right_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else
      {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaBalloonTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaBalloonResTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaBalloonLangTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture8* pic)
{
}

//---------------------------------------------------------
void LcdDrawLaBalloonTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n, WORD pic_x, const TPicture16* pic)
{
  WORD w = rect->Right - rect->Left;
  WORD h = balloon->Height;
  WORD balloon_x0 = balloon->SideWidth;
  WORD balloon_x1 = w - balloon_x0;
  const BYTE* balloon_body_data = balloon->BodyData - 1;
  const BYTE* balloon_left_side_data = balloon->LeftSideData - 1;
  const BYTE* balloon_right_side_data = balloon->RightSideData - 1;
  WORD txt_h = LcdFontHeight;
  WORD txt_x0 = x;
  WORD txt_y0 = y;
  WORD txt_y1 = txt_y0 + txt_h;
  WORD x_tmp, y_tmp;
  BYTE b0;
  TColor color;
  const BYTE *tbl;
  WORD letter_w;
  BYTE mask;
  BYTE row_size;
  WORD i, j;
  WORD pic_w = *pic;
  WORD pic_h = *++pic;
  WORD pic_x1 = pic_x + pic_w;
  WORD pic_y0 = (h - pic_h)/2;
  WORD pic_y1 = pic_y0 + pic_h;
  TColor pic_color;

  LcdBuildLetters(txt, n);

  LcdBeginDrawRegion(rect->Left, rect->Top, w, h);

  for(y_tmp = 0; y_tmp < h; y_tmp++) {
    for(x_tmp = 0; x_tmp < balloon_x0; x_tmp++) {
      if((b0 = *++balloon_left_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }


    color = LcdBalloonPalette[*++balloon_body_data];
    for(; x_tmp < txt_x0; x_tmp++) {
      LCD_WRITE_COLOR1(color);
    }

    if(y_tmp >= txt_y0 && y_tmp < txt_y1) {
      for(i = 0; i < n; i++) {
        letter_w = LcdLettersWidth[i];
        x_tmp += letter_w;
        row_size = letter_w >> 3U;
        if(letter_w & 0x07)
          row_size++;
        tbl = LcdLettersTable[i] + row_size*(y_tmp - txt_y0);
        mask = 0x01;
        do {
          if(mask & *tbl) {
            LCD_WRITE_COLOR2(LcdTextColorB0, LcdTextColorB1);
          }
          else {
            LCD_WRITE_COLOR1(color);
          }
          if((mask <<= 1) == 0) {
            mask = 0x01;
            tbl++;
          }
        } while(--letter_w);

        if(i != n - 1U) {
          for(j = 0; j < LcdLetterSpacing; j++) {
            LCD_WRITE_COLOR1(color);
          }
          x_tmp += LcdLetterSpacing;
        }
      }
    }


    if(y_tmp >= pic_y0 && y_tmp < pic_y1) {
      for(; x_tmp < pic_x; x_tmp++) {
        LCD_WRITE_COLOR1(color);
      }
      for(; x_tmp < pic_x1; x_tmp++) {
        if((pic_color = *++pic) == LCD_TRANSPARENT_COLOR16) {
          LCD_WRITE_COLOR1(color);
        }
        else {
          LCD_WRITE_COLOR1(pic_color);
        }
      }
    }


    for(; x_tmp < balloon_x1; x_tmp++) {
      LCD_WRITE_COLOR1(color);
    }

    for(; x_tmp < w; x_tmp++) {
      if((b0 = *++balloon_right_side_data) == LCD_TRANSPARENT_COLOR8) {
        LCD_WRITE_COLOR2(LcdBgColorB0, LcdBgColorB1);
      }
      else
      {
        color = LcdBalloonPalette[b0];
        LCD_WRITE_COLOR1(color);
      }
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaBalloonResTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture16* pic)
{
  LcdDrawLaBalloonTextWithTrPic16(rect, x, y, balloon, LcdText,
   LoadTextResource(LcdText, id), pic_x, pic);
}

//---------------------------------------------------------
void LcdDrawLaBalloonLangTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture16* pic)
{
  LcdDrawLaBalloonResTextWithTrPic16(rect, x, y, balloon, id + LangId, pic_x, pic);
}

//---------------------------------------------------------
void LcdDrawCaption(BYTE* txt, BYTE n)
{
  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdSetLetterSpacing(LCD_LETTER_SPACING_DEF);
  LcdDrawCaText(LCD_W/2, LCD_CAPTION_Y, txt, n);
}

//---------------------------------------------------------
void LcdDrawResCaption(WORD id)
{
  BYTE n = LoadTextResource(LcdText, id);
  LcdText[n] = ':';
  LcdDrawCaption(LcdText, n + 1);
}

//---------------------------------------------------------
void LcdDrawLangCaption(WORD id)
{
  LcdDrawResCaption(id + LangId);
}

//---------------------------------------------------------
void LcdDrawLangCaptionN(WORD id, WORD rows)
{
  WORD i;

  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdSetLetterSpacing(LCD_LETTER_SPACING_DEF);

  for(i = 0; i < rows; i++) {
    LcdDrawCaLangText(LCD_W/2, LCD_CAPTION_Y + i*LcdFontHeight,
     id + i*LANG_COUNT);
  }
}

//---------------------------------------------------------
void LcdDrawCaptionY(WORD y, BYTE* txt, BYTE n)
{
  LcdSetFont(LCD_CAPTION_FONT);
  LcdSetTextColor(LcdCaptionTextColorDef);
  LcdSetLetterSpacing(LCD_LETTER_SPACING_DEF);
  LcdDrawCaText(LCD_W/2, y, txt, n);
}

//---------------------------------------------------------
void LcdDrawResCaptionY(WORD y, WORD id)
{
  BYTE n = LoadTextResource(LcdText, id);
  LcdText[n] = ':';
  LcdDrawCaptionY(y, LcdText, n + 1);
}

//---------------------------------------------------------
void LcdDrawLangCaptionY(WORD y, WORD id)
{
  LcdDrawResCaptionY(y, id + LangId);
}

//---------------------------------------------------------
void LcdDrawCustom(WORD x, WORD y, WORD w, WORD h, void* data,
 TColor (*proc)(WORD x, WORD y, void* data))
{
  WORD i;
  WORD j;
  TColor color;

  LcdBeginDrawRegion(x, y, w, h);

  for(i = 0; i < w; i++) {
    for(j = h - 1; j != 0xFFFF; j--) {
      color = (*proc)(i, j, data);
      LCD_WRITE_COLOR1(color);
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdDrawLaFactor(WORD x, WORD y, BYTE* factor_str, BYTE n)
{
  TLcdRect r;
  BYTE ch;
  BYTE i;
  WORD letter_size = LcdGetTextWidth((BYTE*)"0", 1) + LcdLetterSpacing;

  r.Left = x;
  r.Bottom = (r.Top = y) + LcdFontHeight;

  for(i = 0; i < n; i++) {
    ch = LcdText[i];
    if(ch == '.') {
      r.Right =
       r.Left + LcdGetTextWidth((BYTE*)".", 1) + LcdSpinEdit.Edit.LetterSpacing;
    }
    else
      r.Right = r.Left + letter_size;
    if(ch == 'e')
      LcdDrawLaRectText(&r, 0, 0, &ch, 1);
    else
      LcdDrawCaRectText(&r, 0, &ch, 1);
    r.Left = r.Right;
  }
}

//---------------------------------------------------------
void LcdDrawLaAutoSizedRectText(TLcdRect* r, BYTE* txt, BYTE n, BOOL redraw)
{
  WORD new_right;

  if(redraw == TRUE) {
    r->Bottom = r->Top + LcdFontHeight;
    r->Right = r->Left;
  }

  new_right = r->Left + LcdGetTextWidth(txt, n);
  if(new_right > r->Right)
    r->Right = new_right;
  LcdDrawLaRectText(r, 0, 0, txt, n);
  r->Right = new_right;
}

//---------------------------------------------------------
void LcdDrawRaAutoSizedRectText(TLcdRect* r, BYTE* txt, BYTE n, BOOL redraw)
{
  WORD new_left;

  if(redraw == TRUE) {
    r->Bottom = r->Top + LcdFontHeight;
    r->Left = r->Right;
  }

  new_left = r->Right - LcdGetTextWidth(txt, n);
  if(new_left < r->Left)
    r->Left = new_left;
  LcdDrawRaRectText(r, 0, 0, txt, n);
  r->Left = new_left;
}

//---------------------------------------------------------
void LcdDrawCaAutoSizedRectText(TLcdRect* r, WORD x, BYTE* txt, BYTE n,
 BOOL redraw)
{
  WORD new_left;
  WORD new_right;
  WORD txt_w = LcdGetTextWidth(txt, n);

  if(redraw == TRUE) {
    r->Bottom = r->Top + LcdFontHeight;
    r->Left = (r->Right = x);
  }

  new_left = x - txt_w/2U;
  new_right = new_left + txt_w;
  if(new_left < r->Left)
    r->Left = new_left;
  if(new_right > r->Right)
    r->Right = new_right;
  LcdDrawCaRectText(r, 0, txt, n);
  r->Left = new_left;
  r->Right = new_right;
}

//---------------------------------------------------------
void LcdMenuCreate(TLcdMenu* menu, const WORD* items)
{
  if(menu == NULL)
    menu = &LcdMenu;

  menu->Items         = items;
  menu->Pics          = NULL;
  menu->Font          = LCD_MENU_FONT;
  menu->Selected      = 0;
  menu->Wrap          = TRUE;
  menu->Balloon       = LcdMenuBalloon;
  menu->LetterSpacing = 1;
  LcdSetFont(menu->Font);
  menu->ItemH         = LcdFontHeight;
  menu->GetText       = NULL;
  menu->AlignLeft     = FALSE;
  menu->MarginLeft    =
  menu->MarginRight   = 6;
  menu->VisibleSize   = 8;
  menu->FirstVisible  = 0;
}

//---------------------------------------------------------
void LcdMenuSetup(TLcdMenu* menu)
{
  WORD i;
  WORD w;
  BYTE saved_font    = LcdFont;
  BYTE saved_spacing = LcdLetterSpacing;
  WORD id;
  BYTE* txt;
  BYTE size;

  if(menu == NULL)
    menu = &LcdMenu;

  LcdSetLetterSpacing(menu->LetterSpacing);
  LcdSetFont(menu->Font);

  for(i = 0; menu->Items[i] != TEXTRESOURCE_EOF; i++);

  menu->Size = i;
  size = menu->Size > menu->VisibleSize ? menu->VisibleSize : menu->Size;
  menu->H    = (size - 1)*menu->ItemH + menu->Balloon->Height;
  if(i <= 4) {
    menu->Y = (LCD_H - LCD_CAPTION_Y - LCD_CAPTION_H - menu->H)/2 +
     LCD_CAPTION_Y + LCD_CAPTION_H - LCD_CAPTION_H/2;
  }
  else {
    menu->Y = (LCD_H - LCD_STATUS_H - menu->H)/2 + LCD_STATUS_H;
  }
  menu->W    = 0;

  for(i = 0; i < menu->Size; i++) {
    id = menu->Items[i];
    if(id == LCD_MENU_ITEM_CB_ID) {
      txt = menu->GetText(i);
      w = LcdBuildLetters(txt + 1, txt[0]);
    }
    else {
      w = LcdBuildLetters(LcdText, LoadTextResource(LcdText, id + LangId));
    }
    if(w > menu->W)
      menu->W = w;
  }
  menu->W += LCD_MENU_ITEM_LEFT_MARGIN + LCD_MENU_ITEM_RIGHT_MARGIN;
  menu->X = (LCD_W - menu->W)/2;

  LcdSetFont(saved_font);
  LcdSetLetterSpacing(saved_spacing);
}

//---------------------------------------------------------
static void LcdMenuApplyStyle(TLcdMenu* menu)
{
  LcdSetFont(menu->Font);
  LcdSetLetterSpacing(menu->LetterSpacing);
  LcdSetBgColorDef();
}

//---------------------------------------------------------
static void LcdMenuDrawItem(TLcdMenu* menu, TLcdRect* rect, WORD index)
{
  WORD id = menu->Items[index];
  BYTE* txt;
  WORD h;
  WORD y;
  const TPicture* pic;

  if(index == menu->Selected) {
    h = menu->Balloon->Height;
    y = (h - LcdFontHeight)/2;
    LcdSetTextColor(LcdSelButtonTextColorDef);
    if(id == LCD_MENU_ITEM_CB_ID) {
      txt = menu->GetText(index);
      if(menu->AlignLeft == FALSE) {
        LcdDrawCaBalloonText(rect, y, menu->Balloon, txt + 1, txt[0]);
      }
      else {
        pic = NULL;
        if(menu->Pics != NULL)
          pic = menu->Pics[index];
        if(pic != NULL) {
          LcdDrawLaBalloonTextWithTrPic(rect, menu->MarginLeft, y,
           menu->Balloon, txt + 1, txt[0],
           rect->Right - rect->Left - menu->MarginRight, pic);
        }
        else
          LcdDrawLaBalloonText(rect, menu->MarginLeft, y, menu->Balloon,
           txt + 1, txt[0]);
      }
    }
    else {
      if(menu->AlignLeft == FALSE) {
        LcdDrawCaBalloonLangText(rect, y, menu->Balloon, id);
      }
      else {
        if(menu->AlignLeft == FALSE) {
          LcdDrawLaBalloonLangText(rect, menu->MarginLeft, y, menu->Balloon, id);
        }
        else {
          pic = NULL;
          if(menu->Pics != NULL)
            pic = menu->Pics[index];
          if(pic != NULL) {
            LcdDrawLaBalloonLangTextWithTrPic(rect, menu->MarginLeft, y,
             menu->Balloon, id, rect->Right - rect->Left - menu->MarginRight,
             pic);
          }
          else
            LcdDrawLaBalloonLangText(rect, menu->MarginLeft, y, menu->Balloon,
             id);
        }
      }
    }
  }
  else {
    h = menu->ItemH;
    y = (h - LcdFontHeight)/2;
    LcdSetTextColor((index & 1) ? LcdEvenRowTextColorDef : LcdTextColorDef);
    if(id == LCD_MENU_ITEM_CB_ID) {
      txt = menu->GetText(index);
      if(menu->AlignLeft == FALSE) {
        LcdDrawCaRectText(rect, y, txt + 1, txt[0]);
      }
      else {
        pic = NULL;
        if(menu->Pics != NULL)
          pic = menu->Pics[index];
        if(pic != NULL) {
          LcdDrawLaRectTextWithTrPic(rect, menu->MarginLeft, y, txt + 1, txt[0],
           rect->Right - rect->Left - menu->MarginRight, pic);
        }
        else
          LcdDrawLaRectText(rect, menu->MarginLeft, y, txt + 1, txt[0]);
      }
    }
    else {
      if(menu->AlignLeft == FALSE) {
        LcdDrawCaRectLangText(rect, y, id);
      }
      else {
        pic = NULL;
        if(menu->Pics != NULL)
          pic = menu->Pics[index];
        if(pic != NULL) {
          LcdDrawLaRectLangTextWithTrPic(rect, menu->MarginLeft, y, id,
           rect->Right - rect->Left - menu->MarginRight, pic);
        }
        else
          LcdDrawLaRectLangText(rect, menu->MarginLeft, y, id);
      }
    }
  }
}

//---------------------------------------------------------
void LcdMenuDraw(TLcdMenu* menu)
{
  WORD i;
  TLcdRect rect;
  WORD y;
  WORD index;
  WORD n;

  if(menu == NULL)
    menu = &LcdMenu;

  LcdMenuApplyStyle(menu);

  y = menu->Y;

  rect.Left  = menu->X;
  rect.Right = rect.Left + menu->W;

  n = menu->Size < menu->VisibleSize ? menu->Size : menu->VisibleSize;

  for(i = 0; i < n; i++) {
    index = i + menu->FirstVisible;
    rect.Bottom = (rect.Top = y) +
     (index == menu->Selected ? menu->Balloon->Height : menu->ItemH);
    LcdMenuDrawItem(menu, &rect, index);
    y = rect.Bottom;
  }
}

//---------------------------------------------------------
void LcdMenuDrawSelected(TLcdMenu* menu)
{
  TLcdRect rect;

  if(menu == NULL)
    menu = &LcdMenu;

  LcdMenuApplyStyle(menu);

  rect.Left   = menu->X;
  rect.Right  = rect.Left + menu->W;
  rect.Top    = menu->Y + menu->ItemH*menu->Selected;
  rect.Bottom = rect.Top + menu->Balloon->Height;
  LcdMenuDrawItem(menu, &rect, menu->Selected);
}

//---------------------------------------------------------
void LcdMenuNext(TLcdMenu* menu)
{
  WORD i;
  TLcdRect rect;

  if(menu == NULL)
    menu = &LcdMenu;

  if(menu->Selected + 1 == menu->Size) {
    if(menu->Wrap == TRUE) {
      menu->Selected = 0;
      LcdMenuDraw(menu);
    }
    return;
  }

  LcdMenuApplyStyle(menu);

  i = menu->Selected++;

  if(menu->Size > menu->VisibleSize) {
    if(menu->FirstVisible + menu->VisibleSize <= menu->Selected) {
      menu->FirstVisible = menu->Selected - menu->VisibleSize + 1;
      LcdMenuDraw(menu);
      return;
    }
  }

  rect.Left   = menu->X;
  rect.Right  = rect.Left + menu->W;
  rect.Top    = menu->Y + menu->ItemH*(i - menu->FirstVisible);
  rect.Bottom = rect.Top + menu->ItemH;
  LcdMenuDrawItem(menu, &rect, i);

  rect.Top    = rect.Bottom;
  rect.Bottom = rect.Top + menu->Balloon->Height;
  LcdMenuDrawItem(menu, &rect, i + 1);
}

//---------------------------------------------------------
void LcdMenuPrev(TLcdMenu* menu)
{
  WORD i;
  TLcdRect rect;

  if(menu == NULL)
    menu = &LcdMenu;

  if(menu->Selected == 0) {
    if(menu->Wrap == TRUE) {
      menu->Selected = menu->Size - 1;
      LcdMenuDraw(menu);
    }
    return;
  }

  LcdMenuApplyStyle(menu);

  i = --menu->Selected;

  if(menu->Selected < menu->FirstVisible) {
    menu->FirstVisible = menu->Selected;
    LcdMenuDraw(menu);
    return;
  }

  rect.Left   = menu->X;
  rect.Right  = rect.Left + menu->W;
  rect.Top    = menu->Y + menu->ItemH*(i - menu->FirstVisible);
  rect.Bottom = rect.Top + menu->Balloon->Height;
  LcdMenuDrawItem(menu, &rect, i);

  rect.Top    = rect.Bottom;
  rect.Bottom = rect.Top + menu->ItemH;
  LcdMenuDrawItem(menu, &rect, i + 1);
}

//---------------------------------------------------------
void LcdMenuDo(TLcdMenu* menu)
{
  if(KeyDown == KB_UP) {
    LcdMenuPrev(menu);
  } else if(KeyDown == KB_DOWN) {
    LcdMenuNext(menu);
  }
}

//---------------------------------------------------------
void LcdMenuFactoryCreateEasy(const WORD* items, WORD selected)
{
  LcdMenuFactoryCreateExt(items, selected, NULL);
}

//---------------------------------------------------------
void LcdMenuFactoryCreateExt(const WORD* items, WORD selected,
 BYTE* (get_text)(WORD index))
{
  LcdMenuCreate(NULL, items);
  LcdMenu.Selected = selected;
  LcdMenu.GetText = get_text;
  LcdMenuSetup(NULL);
  LcdMenuDraw(NULL);
}

//---------------------------------------------------------
void LcdButtonCreate(TLcdButton* btn, WORD id)
{
  btn->Id              = id;
  btn->Selected        = FALSE;
  btn->Ticks           = 0;
  btn->Font            = FONT12;
  btn->Flashing        = LcdButtonFlashing;
  btn->Balloon         = LcdButtonBalloon;
  btn->SelectedBalloon = LcdSelButtonBalloon;
}

//---------------------------------------------------------
void LcdButtonSetup(TLcdButton* btn)
{
  LcdDrawBegin();
  LcdSetFont(btn->Font);
  btn->Rect.Bottom = btn->Rect.Top + btn->Balloon->Height;
  btn->Rect.Right = btn->Rect.Left +
   LcdBuildLetters(LcdText, LoadTextResource(LcdText, btn->Id)) + 24;
  LcdDrawEnd();
}

//---------------------------------------------------------
void LcdButtonDraw(TLcdButton* btn)
{
  LcdDrawBegin();

  LcdSetFont(btn->Font);
  LcdSetTextColor(LcdButtonTextColorDef);

  if(btn->Selected == FALSE) {
    LcdDrawCaBalloonLangText(&btn->Rect, (btn->Rect.Bottom - btn->Rect.Top -
     LcdFontHeight)/2 + 2, btn->Balloon, btn->Id);
  }
  else if(btn->Ticks < 9 || btn->Flashing == FALSE) {
    LcdSetTextColor(LcdSelButtonTextColorDef);
    LcdDrawCaBalloonLangText(&btn->Rect, (btn->Rect.Bottom - btn->Rect.Top -
     LcdFontHeight)/2 + 2, btn->SelectedBalloon, btn->Id);
  }
  else {
    LcdDrawCaBalloonLangText(&btn->Rect, (btn->Rect.Bottom - btn->Rect.Top -
     LcdFontHeight)/2 + 2, btn->Balloon, btn->Id);
  }

  LcdDrawEnd();
}

//---------------------------------------------------------
void LcdButtonSelect(TLcdButton* btn, BOOL selected)
{
  if(btn->Selected != selected) {
    btn->Selected = selected;
    LcdButtonDraw(btn);
  }
}

//---------------------------------------------------------
void LcdButtonDo(TLcdButton* btn)
{
  if(btn->Selected == TRUE && PrgFlags.CentiSec) {
    switch(btn->Ticks++) {
      case 9:
        LcdButtonDraw(btn);
        break;
      case 12:
        btn->Ticks = 0;
        LcdButtonDraw(btn);
    }
  }
}

//---------------------------------------------------------
void LcdDrawLangHintIni(WORD f1_id, WORD f2_id, WORD f3_id)
{
  LcdDrawLangHint(f1_id, f2_id, f3_id);
}

//---------------------------------------------------------
void LcdDrawLangHint(WORD f1_id, WORD f2_id, WORD f3_id)
{
  WORD y;

  LcdDrawBegin();
  LcdSetFont(LCD_HINT_FONT);
  LcdSetFgColor(LcdHintLineColorDef);
  LcdDrawHorzLine(0, LCD_HINT_Y, LCD_W);
  LcdSetTextColor(LcdHintTextColorDef);

  y = LCD_H - LcdFontHeight - 1;

  if(f1_id) {
    LcdDrawLaLangText(1, y, f1_id);
  }
  if(f2_id) {
    LcdDrawCaLangText(LCD_W/2, y, f2_id);
  }
  if(f3_id) {
    LcdDrawRaLangText(LCD_W - 1, y, f3_id);
  }
}

//---------------------------------------------------------
void LcdSpinCreate(TLcdSpin* spin, const TPicture* pic)
{
  spin->Pic            = pic;
  spin->Key            = 0xFFFF;
  spin->Pressed        = FALSE;
  spin->Balloon        = LcdSpinDarkBalloon;
  spin->PressedBalloon = LcdSpinLightBalloon;
  spin->H              = spin->Balloon->Height;
}

//---------------------------------------------------------
void LcdSpinSetup(TLcdSpin* spin)
{
  spin->H = spin->Balloon->Height;
}

//---------------------------------------------------------
void LcdSpinDraw(TLcdSpin* spin)
{
  TLcdRect rect;

  LcdDrawBegin();

  rect.Right  = (rect.Left = spin->X) + spin->W;
  rect.Bottom = (rect.Top  = spin->Y) + spin->Balloon->Height;

  LcdDrawCaBalloonTrPic(&rect, (spin->Pressed == TRUE) ?
   spin->PressedBalloon : spin->Balloon, spin->Pic);

  LcdDrawEnd();
}

//---------------------------------------------------------
void LcdSpinDo(TLcdSpin* spin)
{
  if(KeyHold != spin->Key) {
    if(spin->Pressed == TRUE) {
      spin->Pressed = FALSE;
      LcdSpinDraw(spin);
    }
  }
  else {
    if(spin->Pressed == FALSE) {
      spin->Pressed = TRUE;
      LcdSpinDraw(spin);
    }
  }
}

//---------------------------------------------------------
void LcdEditCreate(TLcdEdit* edit)
{
  edit->Font          = LCD_ITEM_FONT;
  edit->GetText       = NULL;
  edit->Balloon       = LcdEditBalloon;
  edit->LetterSpacing = LCD_LETTER_SPACING_DEF;
  edit->TextAlignment = lcdHaCenter;
  edit->Tag           = 0;
}

//---------------------------------------------------------
void LcdEditSetup(TLcdEdit* edit)
{
}

//---------------------------------------------------------
void LcdEditDraw(TLcdEdit* edit)
{
  TLcdRect rect;
  BYTE *txt = NULL;

  if(edit->GetText != NULL)
    txt = edit->GetText(edit);

  rect.Right = (rect.Left = edit->X) + edit->W;
  rect.Bottom = (rect.Top = edit->Y) + edit->Balloon->Height;

  LcdDrawBegin();

  LcdSetTextColor(LcdEditTextColorDef);
  LcdSetLetterSpacing(edit->LetterSpacing);
  LcdSetFont(edit->Font);

  if(txt == NULL)
    LcdDrawCaBalloonText(&rect, 0, edit->Balloon, (BYTE*)" ", 1);
  else {
    switch(edit->TextAlignment) {
      case lcdHaLeft:
        LcdDrawLaBalloonText(&rect, 2, 0, edit->Balloon, txt + 1, txt[0]);
        break;
      case lcdHaRight:
        LcdDrawRaBalloonText(&rect, 2, 0, edit->Balloon, txt + 1, txt[0]);
        break;
      default:
        LcdDrawCaBalloonText(&rect, 0, edit->Balloon, txt + 1, txt[0]);
    }
  }

  LcdDrawEnd();
}

//---------------------------------------------------------
void LcdEditDo(TLcdEdit* edit)
{
  if(KeyDown == KB_PLUS ||
     KeyDown == KB_MINUS)
    LcdEditDraw(edit);
}

//---------------------------------------------------------
void LcdSpinEditCreate(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  LcdSpinCreate(&spin_edit->UpSpin, PicturesSpinUp);
  LcdSpinCreate(&spin_edit->DnSpin, PicturesSpinDown);
  LcdEditCreate(&spin_edit->Edit);
  spin_edit->Y = LCD_H/2 - 20;
  spin_edit->UpSpin.Key = KB_PLUS;
  spin_edit->DnSpin.Key = KB_MINUS;
  spin_edit->Edit.TextAlignment = lcdHaRight;
  spin_edit->Edit.Tag = (DWORD)spin_edit;
  spin_edit->Edit.GetText = LcdSpinEditGetText;
  spin_edit->Edit.LetterSpacing = 2;
  spin_edit->Value    =
  spin_edit->Min      = 0;
  spin_edit->Max      = 1;
  spin_edit->Comma    = 0;
  spin_edit->Pos      = 0;
  spin_edit->TextLen  = 0;
  spin_edit->Wrap     = FALSE;
  spin_edit->EnableLR = FALSE;
  spin_edit->WrapLR   = TRUE;
  spin_edit->ResIds   = NULL;
  spin_edit->GetText  = NULL;
  spin_edit->InactiveTextColor = LcdTextColorDef;
  spin_edit->Exp      = NULL;
}

//---------------------------------------------------------
void LcdSpinEditCreateSign(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;
  LcdSpinEditCreate(spin_edit);
  spin_edit->Max = 1;
  spin_edit->Min = 0;
  spin_edit->GetText = LcdSpinEditSignGetText;
}

//---------------------------------------------------------
static BYTE* LcdSpinEditGetText(void* o)
{
  TLcdSpinEdit* spin_edit = (TLcdSpinEdit*)((TLcdEdit*)o)->Tag;
  BYTE* buff;
  BYTE pos = spin_edit->Pos;
  BYTE comma = spin_edit->Comma;


  if(spin_edit->GetText != NULL) {
    return spin_edit->GetText(spin_edit);
  }


  buff = spin_edit->Text;

  if(spin_edit->ResIds != NULL) {
    buff[0] = LoadLangResource(buff + 1,
     spin_edit->ResIds[spin_edit->Value - spin_edit->Min]);
    return buff;
  }


  if(spin_edit->Exp != NULL) {
    BYTE* exp = spin_edit->Exp;
    if(pos <= 1) {
      buff[0] = 1;
      buff[1] = exp[pos];
    }
    else if(pos < 3 + comma) {
      buff[0] = pos;
      buff[1] = exp[1];
      buff[2] = '.';
      memcpy(buff + 3, exp + 2, comma);
    }
    else if(pos == 3 + comma) {
      buff[0] = 1;
      buff[1] = 'e';
    }
    else if(pos == 4 + comma) {
      buff[0] = 1;
      buff[1] = exp[pos - 2];
    }
    else {
      buff[0] = pos - 4 - comma;
      memcpy(buff + 1, exp + 3 + comma, buff[0]);
    }
    return buff;
  }


  if(comma) {
    DwordToStrAsFloatWithLeadingZeroes(buff + 1, (DWORD)spin_edit->Value,
     spin_edit->TextLen, comma);
  }
  else {
    DwordToStrWithLeadingZeroes(buff + 1, (DWORD)spin_edit->Value,
     spin_edit->TextLen);
  }
  buff[0] = spin_edit->TextLen - pos;
  return buff;
}

//---------------------------------------------------------
static BYTE* LcdSpinEditSignGetText(void* o)
{
  TLcdSpinEdit* spin_edit = (TLcdSpinEdit*)o;
  BYTE* buff = spin_edit->Text;

  buff[0] = 1;
  buff[1] = (spin_edit->Value == 0) ? '-' : '+';

  return buff;
}

//---------------------------------------------------------
void LcdSpinEditSetup(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  LcdSetFont(spin_edit->Edit.Font);
  spin_edit->LetterW = LcdGetTextWidth((BYTE*)"0", 1);
  spin_edit->CommaLetterW = LcdGetTextWidth((BYTE*)".", 1);

  LcdSpinEditResizeEdit(spin_edit);

  LcdSpinSetup(&spin_edit->UpSpin);
  LcdSpinSetup(&spin_edit->DnSpin);
  LcdEditSetup(&spin_edit->Edit);
}

//---------------------------------------------------------
void LcdSpinEditDraw(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  LcdSpinDraw(&spin_edit->UpSpin);
  LcdSpinDraw(&spin_edit->DnSpin);
  LcdEditDraw(&spin_edit->Edit);
  LcdSpinEditDrawInactive(spin_edit);
}

//---------------------------------------------------------
static void LcdSpinEditDrawInactive(TLcdSpinEdit* spin_edit)
{
  TLcdRect r;
  BYTE n;
  BYTE pos;
  BYTE letter_w;
  BYTE letter_spacing;
  BYTE y;
  BYTE* exp = spin_edit->Exp;
  WORD one_pos_edit_w;
  WORD x = spin_edit->X;
  WORD txt_len = spin_edit->TextLen;
  WORD comma = spin_edit->Comma;

  LcdSetLetterSpacing(spin_edit->Edit.LetterSpacing);
  LcdSetTextColor(spin_edit->InactiveTextColor);
  LcdSetFont(spin_edit->Edit.Font);

  r.Bottom = (r.Top = spin_edit->Y) + spin_edit->H;
  y = spin_edit->Edit.Y - r.Top;

  if(exp != NULL) {
    letter_w = spin_edit->LetterW;
    letter_spacing = spin_edit->Edit.LetterSpacing;
    one_pos_edit_w = letter_w + letter_spacing;
    pos = spin_edit->Pos;
    if(pos != 0) {
      r.Left = x;
      r.Right = r.Left + one_pos_edit_w;
      LcdDrawCaRectText(&r, y, exp, 1);
    }
    if(1 <= pos && pos < 3U + comma) {
      if(pos == 1) {
        r.Left = x + one_pos_edit_w*2U;
        r.Right = r.Left + (spin_edit->CommaLetterW + letter_spacing) +
         one_pos_edit_w*comma;
        LcdText[0] = '.';
        memcpy(LcdText + 1, exp + 2U, comma);
        LcdDrawCaRectText(&r, y, LcdText, comma + 1);
      }
      else {
        r.Left = x + one_pos_edit_w*pos +
         (spin_edit->CommaLetterW + letter_spacing);
        r.Right = x + one_pos_edit_w*(comma + 2U) +
          (spin_edit->CommaLetterW + letter_spacing);
        LcdDrawCaRectText(&r, y, exp + 2U + (pos + 1U) - 3U,
         comma + 3U - (pos + 1U));
      }
    }
    else {
      r.Left = x + one_pos_edit_w;
      r.Right = r.Left + (spin_edit->CommaLetterW + letter_spacing) +
       one_pos_edit_w*(comma + 1U);
      LcdText[0] = exp[1];
      LcdText[1] = '.';
      memcpy(LcdText + 2U, exp + 2U, comma);
      LcdDrawCaRectText(&r, y, LcdText, comma + 2U);
    }
    r.Left = x + (2 + comma)*one_pos_edit_w +
      (spin_edit->CommaLetterW + letter_spacing);
    r.Right = r.Left + letter_w;
    LcdDrawCaRectText(&r, y, (BYTE*)"e", 1U);
    if(pos != comma + 4U) {
      r.Left = x + (3U + comma)*one_pos_edit_w +
       (spin_edit->CommaLetterW + letter_spacing);
      r.Right = r.Left + one_pos_edit_w;
      LcdDrawCaRectText(&r, y, exp + 2U + comma, 1U);
    }
    if(pos > comma + 4U) {
      r.Left = x + pos*one_pos_edit_w +
       (spin_edit->CommaLetterW + letter_spacing);
      r.Right = r.Left + one_pos_edit_w*(n = txt_len - 1U - pos);
      LcdDrawCaRectText(&r, y, exp + pos - 1U, n);
    }
    else {
      r.Left = x + (4U + comma)*one_pos_edit_w +
       (spin_edit->CommaLetterW + letter_spacing);
      r.Right = r.Left + one_pos_edit_w*(n = txt_len - 5U - comma);
      LcdDrawCaRectText(&r, y, exp + 3U + comma, n);
    }
    return;
  }

  pos = txt_len - spin_edit->Pos;
  n = txt_len - pos;
  if(n == 0)
    return;

  r.Left = spin_edit->Edit.X + spin_edit->Edit.W;
  r.Right = x + spin_edit->W;
  if(r.Right <= r.Left)
    return;
  LcdDrawRaRectText(&r, 0, y, spin_edit->Text + 1 + pos, n);
}

static const INT32 LcdSpinDelta[] = {
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000
};

//---------------------------------------------------------
void LcdSpinEditDo(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  if(spin_edit->GetText != NULL)
    LcdSpinEditDoFunc(spin_edit);
  else if(spin_edit->ResIds != NULL)
    LcdSpinEditDoResIds(spin_edit);
  else if(spin_edit->Exp != NULL)
    LcdSpinEditDoExp(spin_edit);
  else
    LcdSpinEditDoNumber(spin_edit);
}

//---------------------------------------------------------
static void LcdSpinEditDoNumber(TLcdSpinEdit* spin_edit)
{
  INT32 val;
  INT32 delta = 0;
  BYTE pos;
  BOOL resize_edit;
  BOOL draw_inactive = FALSE;

  pos = spin_edit->Pos;
  if(spin_edit->Comma) {
    if(pos >= spin_edit->Comma)
      pos--;
  }
  if(pos < ARRAY_LENGTH(LcdSpinDelta))
    delta = LcdSpinDelta[pos];
  val = spin_edit->Value;

  if(KeyDown == KB_PLUS) {
    val += delta;
    if(spin_edit->Wrap == TRUE) {
      if(val > spin_edit->Max || val < spin_edit->Value)
        val = spin_edit->Min;
    }
    else {
      if(val > spin_edit->Max || val < spin_edit->Value)
        val = spin_edit->Max;
    }
    draw_inactive = TRUE;
  }

  if(KeyDown == KB_MINUS) {
    val -= delta;
    if(spin_edit->Wrap == TRUE) {
      if(val < spin_edit->Min || val > spin_edit->Value)
        val = spin_edit->Max;
    }
    else {
      if(val < spin_edit->Min || val > spin_edit->Value)
        val = spin_edit->Min;
    }
    draw_inactive = TRUE;
  }

  spin_edit->Value = val;

  if(spin_edit->EnableLR == TRUE) {
    resize_edit = TRUE;
    switch(KeyDown) {
      case KB_LEFT:
        if(spin_edit->TextLen) {
          if(spin_edit->Pos + 1 < spin_edit->TextLen) {
            spin_edit->Pos++;
            if(spin_edit->Comma) {
              if(spin_edit->Comma == spin_edit->Pos)
                spin_edit->Pos++;
            }
          } else if(spin_edit->WrapLR == TRUE) {
            spin_edit->Pos = 0;
          }
        }
        break;
      case KB_RIGHT:
        if(spin_edit->Pos) {
          spin_edit->Pos--;
          if(spin_edit->Comma) {
            if(spin_edit->Comma == spin_edit->Pos)
              spin_edit->Pos--;
          }
        } else if(spin_edit->WrapLR == TRUE && spin_edit->TextLen) {
          spin_edit->Pos = spin_edit->TextLen - 1;
        }
        break;
      default:
        resize_edit = FALSE;
    }

    if(resize_edit == TRUE) {
      LcdSpinEditResizeEdit(spin_edit);
      LcdSpinEditDraw(spin_edit);
    }
  }

  LcdSpinDo(&spin_edit->UpSpin);
  LcdSpinDo(&spin_edit->DnSpin);
  LcdEditDo(&spin_edit->Edit);
  if(draw_inactive == TRUE)
    LcdSpinEditDrawInactive(spin_edit);
}

//---------------------------------------------------------
static void LcdSpinEditDoResIds(TLcdSpinEdit* spin_edit)
{
  INT32 val;

  val = spin_edit->Value;

  if(KeyDown == KB_PLUS) {
    if(spin_edit->Wrap == TRUE) {
      if(val == spin_edit->Max)
        val = spin_edit->Min;
      else
        val++;
    }
    else {
      if(val < spin_edit->Max)
        val++;
    }
  }

  if(KeyDown == KB_MINUS) {
    if(spin_edit->Wrap == TRUE) {
      if(val == spin_edit->Min)
        val = spin_edit->Max;
      else
        val--;
    }
    else {
      if(val > spin_edit->Min)
        val--;
    }
  }

  spin_edit->Value = val;

  LcdSpinDo(&spin_edit->UpSpin);
  LcdSpinDo(&spin_edit->DnSpin);
  LcdEditDo(&spin_edit->Edit);
}

//---------------------------------------------------------
static void LcdSpinEditDoFunc(TLcdSpinEdit* spin_edit)
{
  LcdSpinEditDoResIds(spin_edit);
}

//---------------------------------------------------------
static void LcdSpinEditDoExp(TLcdSpinEdit* spin_edit)
{
  BOOL draw_inactive = FALSE;
  BOOL resize_edit = FALSE;
  BYTE pos = spin_edit->Pos;
  BYTE* exp = spin_edit->Exp;
  WORD i, j;
  BOOL carry = TRUE;
  BYTE comma = spin_edit->Comma;
  BYTE txt_len = spin_edit->TextLen;

  if(KeyDown == KB_RIGHT) {
    if(++pos == 2U)
      pos++;
    else if(pos == 3U + comma)
      pos++;
    if(pos >= txt_len)
      pos = 0;
    spin_edit->Pos = pos;
    draw_inactive = TRUE;
    resize_edit = TRUE;
  }

  if(KeyDown == KB_LEFT) {
    if(pos)
      pos--;
    else
      pos = txt_len - 1U;
    if(pos == 2U)
      pos--;
    else if(pos == 3U + comma)
      pos--;
    if(pos >= txt_len)
      pos = 0;
    spin_edit->Pos = pos;
    draw_inactive = TRUE;
    resize_edit = TRUE;
  }

  if(KeyDown == KB_PLUS || KeyDown == KB_MINUS) {
    BYTE dig1;
    BYTE dig2;
    BYTE delta;

    if(KeyDown == KB_PLUS) {
      dig1 = '9';
      dig2 = '0';
      delta = 1;
    }
    else {
      dig1 = '0';
      dig2 = '9';
      delta = (BYTE)-1;
    }

    if(pos == 0) {
      exp[0] = exp[0] == '+' ? '-' : '+';
    }
    else if(pos == 4U + comma) {
      i = 2U + spin_edit->Comma;
      exp[i] = exp[i] == '+' ? '-' : '+';
    }
    else if(pos >= 1 && pos <= 3U + comma) {
      for(j = pos; j >= 1U; j--) {
        if(j == 2U)
          continue;
        i = j > 2U ? j - 1U : j;
        if(exp[i] == dig1) {
          exp[i] = dig2;
        }
        else {
          exp[i] += delta;
          carry = FALSE;
          break;
        }
      }

      if(carry == TRUE) {
        memset(exp + 1U, dig1, 1U + comma);
        draw_inactive = TRUE;
      }
    }
    else if(pos > 4U + comma) {
      for(j = pos; j > 4U + comma; j--)
      {
        i = j - 2U;
        if(exp[i] == dig1) {
          exp[i] = dig2;
        }
        else {
          exp[i] += delta;
          carry = FALSE;
          break;
        }
      }

      if(carry == TRUE) {
        memset(exp + 3U + comma, dig1, txt_len - 5U - comma);
        draw_inactive = TRUE;
      }
    }
  }

  if(resize_edit == TRUE) {
    LcdSpinEditResizeEdit(spin_edit);
    LcdSpinEditDraw(spin_edit);
  }

  if(draw_inactive == TRUE) {
    LcdDrawBegin();
    LcdSpinEditDrawInactive(spin_edit);
    LcdDrawEnd();
  }
  LcdSpinDo(&spin_edit->UpSpin);
  LcdSpinDo(&spin_edit->DnSpin);
  LcdEditDo(&spin_edit->Edit);
}

//---------------------------------------------------------
static void LcdSpinEditResizeEdit(TLcdSpinEdit* spin_edit)
{
  TLcdSpin* up_spin = &spin_edit->UpSpin;
  TLcdSpin* dn_spin = &spin_edit->DnSpin;
  TLcdEdit* edit = &spin_edit->Edit;
  WORD      edit_x;
  WORD      edit_w;
  BYTE      pos = spin_edit->Pos;
  WORD      letter_w = spin_edit->LetterW;
  WORD      letter_spacing = edit->LetterSpacing;
  WORD      one_pos_edit_w = letter_w + letter_spacing;
  WORD      x = spin_edit->X;
  BYTE      comma = spin_edit->Comma;
  WORD      comma_letter_w = spin_edit->CommaLetterW;

  if(spin_edit->Exp != NULL) {
    if(pos <= 1U) {
      edit_x = x + pos*one_pos_edit_w;
      edit_w = one_pos_edit_w;
    }
    else if(pos > 1U && pos < 3U + comma) {
      edit_x = x + one_pos_edit_w;
      edit_w = (comma_letter_w + letter_spacing) +
       (pos - 1)*one_pos_edit_w;
    }
    else if(pos == 4U + comma) {
      edit_x = x + (3U + comma)*one_pos_edit_w +
       (comma_letter_w + letter_spacing);
      edit_w = one_pos_edit_w;
    }
    else {
      edit_x = x + (4U + comma)*one_pos_edit_w +
       (comma_letter_w + letter_spacing);
      edit_w = one_pos_edit_w*(pos - 4U - comma);
    }
  }
  else {
    edit_x = x;
    if(comma > 0 && comma < pos) {
      edit_w = spin_edit->W - (pos - 1)*one_pos_edit_w -
       (comma_letter_w + letter_spacing);
    }
    else {
      edit_w = spin_edit->W - pos*one_pos_edit_w;
    }
  }


  if(edit_w > 30U) {
    up_spin->Pic = PicturesSpinUp;
    dn_spin->Pic = PicturesSpinDown;
  }
  else {
    up_spin->Pic = PicturesSmallSpinUp;
    dn_spin->Pic = PicturesSmallSpinDown;
  }

  up_spin->X = edit_x;
  up_spin->W = edit_w;
  up_spin->Y = spin_edit->Y;

  dn_spin->X = edit_x;
  dn_spin->W = edit_w;
  dn_spin->Y = up_spin->Y + up_spin->Balloon->Height +
   edit->Balloon->Height + 2U*2U;

  edit->X    = edit_x;
  edit->W    = edit_w;
  edit->Y    = (dn_spin->Y + dn_spin->Balloon->Height - up_spin->Y -
   edit->Balloon->Height)/2U + up_spin->Y;

  spin_edit->H = dn_spin->Y + dn_spin->H - up_spin->Y;
}

//---------------------------------------------------------
void LcdSpinEditGetPos(TLcdSpinEdit* spin_edit, TLcdRect* rect)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  rect->Right  = (rect->Left = spin_edit->X) + spin_edit->W;
  rect->Top    = spin_edit->Y;
  rect->Bottom = spin_edit->DnSpin.Y + spin_edit->DnSpin.Balloon->Height;
}

//---------------------------------------------------------
void LcdSpinEditUnmap(TLcdSpinEdit* spin_edit)
{
  if(spin_edit == NULL)
    spin_edit = &LcdSpinEdit;

  LcdSetFgColor(LcdGetBgColor());
  LcdDrawRect(spin_edit->X, spin_edit->Y, spin_edit->W, spin_edit->H);
}

//---------------------------------------------------------
void LcdGaugeCreate(TLcdGauge* gauge)
{
  if(gauge == NULL)
    gauge = &LcdGauge;

  gauge->X            = 10;
  gauge->W            = LCD_W - 20;
  gauge->Y            = 197;
  gauge->Max          = 70.0f;
  gauge->Min          = 30.0f;
  gauge->Val          = 0.0f;
  gauge->ValPos       = 0;
  gauge->Balloon      = &BalloonsGauge;
  gauge->Range        = 0xFF;
  gauge->RangeEnabled = TRUE;
}

//---------------------------------------------------------
void LcdGaugeSetup(TLcdGauge* gauge)
{
  if(gauge == NULL)
    gauge = &LcdGauge;

  gauge->H  = gauge->Balloon->Height;
  gauge->KX = 0.01f*(gauge->W - 2);
}

//---------------------------------------------------------
void LcdGaugeDraw(TLcdGauge* gauge)
{
  WORD x;
  WORD y;
  WORD w;
  WORD h;
  WORD val_pos;
  WORD min_pos;
  WORD max_pos;
  FLOAT32 val;
  FLOAT32 min;
  FLOAT32 max;
  FLOAT32 kx;
  WORD xi, yi;
  WORD yn;
  const BYTE* data;
  WORD* palette;
  TColor color;

  if(gauge == NULL)
    gauge = &LcdGauge;

  x    = gauge->X;
  y    = gauge->Y;
  w    = gauge->W;
  h    = gauge->H;
  data = gauge->Balloon->BodyData;

  LcdSetFgColor(LcdGaugeFgColorDef);
  LcdSetLineThickness(1);
  LcdDrawHorzLine(x, y, w);
  LcdDrawHorzLine(x, y + h - 1, w);
  LcdDrawVertLine(x, y, h);
  LcdDrawVertLine(x + w - 1, y, h);

  val = gauge->Val;
  max = gauge->Max;
  min = gauge->Min;
  kx  = gauge->KX;

  if(val <   0.0f) val = 0.0f;
  if(val > 100.0f) val = 100.0f;
  if(max <   0.0f) max = 0.0f;
  if(max > 100.0f) max = 100.0f;
  if(min <   0.0f) min = 0.0f;
  if(min > 100.0f) min = 100.0f;

  gauge->ValPos = val_pos = (WORD)(kx*val);
  gauge->MinPos = min_pos = (WORD)(kx*min);
  gauge->MaxPos = max_pos = (WORD)(kx*max);

  LcdBeginDrawRegion(x + 1, y + 1, w - 2, yn = h - 2);

  if(gauge->RangeEnabled == TRUE) {
    if(val_pos < min_pos) {
      gauge->Range = 0;
    }
    else if(val_pos <= max_pos) {
      gauge->Range = 1;
    }
    else {
      gauge->Range = 2;
    }
  }

  switch(gauge->Range) {
    case 0:
      palette      = LcdGaugeLessPalette;
      break;
    case 1:
      palette      = LcdGaugeRangePalette;
      break;
    default:
      palette      = LcdGaugeMorePalette;
  }

  for(yi = 0; yi < yn; yi++) {
    color = palette[data[yi]];
    for(xi = 0; xi < val_pos; xi++) {
      LCD_WRITE_COLOR1(color);
    }
    for(; xi < w - 2U; xi++) {
      LCD_WRITE_COLOR1(LcdGaugeBgColorDef);
    }
  }

  if(gauge->RangeEnabled == TRUE) {
    if(min_pos <= max_pos) {
      LcdSetFgColor(LcdGaugeFgColorDef);
      LcdDrawVertLine(x + min_pos, y + h, 7);
      LcdDrawVertLine(x + max_pos, y + h, 7);
      if(max_pos - min_pos > 1) {
        LcdDrawHorzLine(x + min_pos, y + h + 7, max_pos - min_pos + 1);
        LcdSetFgColor(LCD_RGB_TO_COLOR(0xFF, 0x00, 0xFF));
        LcdDrawRect(x + min_pos + 1, y + h, max_pos - min_pos - 1, 7);
      }
    }
  }

  LcdEndDrawRegion();
}

//---------------------------------------------------------
void LcdGaugeSetValue(TLcdGauge* gauge, FLOAT32 value, BOOL redraw)
{
  WORD new_val_pos;
  WORD old_val_pos;
  BYTE new_range;
  WORD* palette;
  WORD xi, yi;
  WORD yn;
  TColor color;
  const BYTE* data;

  if(gauge == NULL)
    gauge = &LcdGauge;

 // value = gauge->Val;
  //if((value -= 1.0) < 0.0)
  //  value = 100.0;
  //if((value += 1.0) > 100.0)
  //  value = 0.0;

  if(value <= 0.0f)
    value = 0.0f;
  else if(value >= 100.0f)
    value = 100.0f;

  if(redraw == TRUE) {
    gauge->Val = value;
    LcdGaugeDraw(gauge);
    return;
  }

  if(value == gauge->Val) return;

  gauge->Val  = value;
  old_val_pos = gauge->ValPos;
  new_val_pos = (WORD)(gauge->KX*value);

  if(new_val_pos == old_val_pos) return;

  if(gauge->RangeEnabled == FALSE) {
    new_range = gauge->Range;
  }
  else {
    if(new_val_pos < gauge->MinPos) {
      new_range = 0;
    }
    else if(new_val_pos <= gauge->MaxPos) {
      new_range = 1;
    }
    else {
      new_range = 2;
    }
  }

  switch(new_range) {
    case 0:
      palette   = LcdGaugeLessPalette;
      break;
    case 1:
      palette   = LcdGaugeRangePalette;
      break;
    default:
      palette   = LcdGaugeMorePalette;
  }

  data = gauge->Balloon->BodyData;

  LcdDrawBegin();

  if(new_range != gauge->Range) {
    gauge->Range = new_range;

    LcdBeginDrawRegion(gauge->X + 1U, gauge->Y + 1U, gauge->W - 2U,
     yn = gauge->H - 2U);

    for(yi = 0; yi < yn; yi++) {
      color = palette[data[yi]];
      for(xi = 0; xi < new_val_pos; xi++) {
        LCD_WRITE_COLOR1(color);
      }
      for(; xi < gauge->W - 2U; xi++) {
        LCD_WRITE_COLOR1(LcdGaugeBgColorDef);
      }
    }

    LcdEndDrawRegion();
  }
  else {
    if(new_val_pos < old_val_pos) {
      LcdSetFgColor(LcdGaugeBgColorDef);
      LcdDrawRect(gauge->X + new_val_pos + 1U, gauge->Y + 1U,
       old_val_pos - new_val_pos, gauge->H - 2U);
    }
    else {
      LcdBeginDrawRegion(gauge->X + old_val_pos + 1U, gauge->Y + 1U,
       new_val_pos - old_val_pos, yn = gauge->H - 2U);
      for(yi = 0; yi < yn; yi++) {
        color = palette[data[yi]];
        for(xi = old_val_pos; xi < new_val_pos; xi++) {
          LCD_WRITE_COLOR1(color);
        }
      }
      LcdEndDrawRegion();
    }
  }

  gauge->ValPos = new_val_pos;

  LcdDrawEnd();
}

//---------------------------------------------------------
void LcdSliderCreate(TLcdSlider* slider)
{
  if(slider == NULL)
    slider = &LcdSlider;
}

//---------------------------------------------------------
void LcdSliderSetup(TLcdSlider* slider)
{
  if(slider == NULL)
    slider = &LcdSlider;
}

//---------------------------------------------------------
void LcdSliderDraw(TLcdSlider* slider)
{
  if(slider == NULL)
    slider = &LcdSlider;
}

//---------------------------------------------------------
void LcdSetContrast(BYTE contrast)
{
  LightSetContrast(contrast);
}
