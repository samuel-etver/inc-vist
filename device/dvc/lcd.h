#ifndef LCD_H_INCLUDED
#define LCD_H_INCLUDED

/*---------------------------------------------------------
 lcd.h
---------------------------------------------------------*/

#include "xmain.h"
#include "fonts.h"
#include "TextResources.h"
#include "Pictures.h"
#include "Balloons.h"
#include "Theme.h"

#define LCD_W                       240
#define LCD_H                       320

#define LCD_TEXT_LEN                128

#define LCD_LINE_THICKNESS_DEF      1
#define LCD_LETTER_SPACING_DEF      0

#define LCD_TRANSPARENT_COLOR8      0x20
#define LCD_TRANSPARENT_COLOR16     0x0001

#define LCD_RGB_TO_COLOR(r, g, b) \
  ((((WORD)(r) >> 3) << 11) | \
   (((WORD)(g) >> 2) <<  5) | \
   ((((WORD)(b) >> 3))))

#define LCD_WRITE_COLOR1(color) \
  LcdWriteDataByte((BYTE)(color >> 8)); \
  LcdWriteDataByte((BYTE)(color));

#define LCD_WRITE_COLOR2(b0, b1)  \
  LcdWriteDataByte(b1); \
  LcdWriteDataByte(b0)

#define LCD_CAPTION_FONT            FONT12
#define LCD_CAPTION_Y               48
#define LCD_CAPTION_H               30

#define LCD_ITEM_FONT               FONT12
#define LCD_MENU_FONT               FONT12
#define LCD_MENU_ITEM_LEFT_MARGIN   8
#define LCD_MENU_ITEM_RIGHT_MARGIN  LCD_MENU_ITEM_LEFT_MARGIN
#define LCD_MENU_ITEM_CB_ID         0xFFFE

#define LCD_STATUS_H                30

#define LCD_HINT_H                  22
#define LCD_HINT_Y                  (LCD_H - LCD_HINT_H - 1)
#define LCD_HINT_FONT               FONT10

#define LCD_LABEL_GAP               8
#define LCD_LABEL_TO_SPIN_EDIT_OFFS 15

#ifdef PICTURE8
#  define LCD_TRANSPARENT_COLOR               LCD_TRANSPARENT_COLOR8
#  define LcdDrawPic                          LcdDrawPic8
#  define LcdDrawTrPic                        LcdDrawTrPic8
#  define LcdDrawLaRectPic                    LcdDrawLaRectPic8
#  define LcdDrawCaBalloonPic                 LcdDrawCaBalloonPic8
#  define LcdDrawCaBalloonTrPic               LcdDrawCaBalloonTrPic8
#  define LcdDrawLaBalloonTextWithTrPic       LcdDrawLaBalloonTextWithTrPic8
#  define LcdDrawLaBalloonResTextWithTrPic    LcdDrawLaBalloonResTextWithTrPic8
#  define LcdDrawLaBalloonLangTextWithTrPic   LcdDrawLaBalloonLangTextWithTrPic8
#  define LcdDrawLaRectTextWithTrPic          LcdDrawLaRectTextWithTrPic8
#  define LcdDrawLaRectLangTextWithTrPic      LcdDrawLaRectLangTextWithTrPic8
#else
#  define LCD_TRANSPARENT_COLOR               LCD_TRANSPARENT_COLOR16
#  define LcdDrawPic                          LcdDrawPic16
#  define LcdDrawTrPic                        LcdDrawTrPic16
#  define LcdDrawLaRectPic                    LcdDrawLaRectPic16
#  define LcdDrawCaBalloonPic                 LcdDrawCaBalloonPic16
#  define LcdDrawCaBalloonTrPic               LcdDrawCaBalloonTrPic16
#  define LcdDrawLaBalloonTextWithTrPic       LcdDrawLaBalloonTextWithTrPic16
#  define LcdDrawLaBalloonResTextWithTrPic    LcdDrawLaBalloonResTextWithTrPic16
#  define LcdDrawLaBalloonLangTextWithTrPic   LcdDrawLaBalloonLangTextWithTrPic16
#  define LcdDrawLaRectTextWithTrPic          LcdDrawLaRectTextWithTrPic16
#  define LcdDrawLaRectLangTextWithTrPic      LcdDrawLaRectLangTextWithTrPic16
#endif

typedef WORD TColor;

typedef struct {
  WORD Left;
  WORD Top;
  WORD Right;
  WORD Bottom;
} TLcdRect;

typedef enum {
  lcdHaLeft,
  lcdHaRight,
  lcdHaCenter
} TLcdHorzAlignment;

typedef struct {
  BYTE              Size;
  BYTE              VisibleSize;
  BYTE              FirstVisible;
  BYTE              Selected;
  const WORD*      Items;
  const TPicture** Pics;
  WORD              Font;
  WORD              X;
  WORD              Y;
  WORD              W;
  WORD              H;
  BYTE              ItemH;
  BYTE              LetterSpacing;
  BOOL              Wrap;
  BYTE*             (*GetText)(WORD index);
  BOOL              AlignLeft;
  WORD              MarginLeft;
  WORD              MarginRight;
  const TBalloon*   Balloon;
} TLcdMenu;

typedef struct {
  BOOL              Selected;
  BYTE              Ticks;
  TLcdRect          Rect;
  WORD              Id;
  BYTE              Font;
  BOOL              Flashing;
  const TBalloon*   SelectedBalloon;
  const TBalloon*   Balloon;
} TLcdButton;

typedef struct {
  WORD              Key;
  BOOL              Pressed;
  WORD              X;
  WORD              Y;
  WORD              W;
  WORD              H;
  const TBalloon*   PressedBalloon;
  const TBalloon*   Balloon;
  const TPicture*   Pic;
} TLcdSpin;

typedef struct {
  WORD              X;
  WORD              Y;
  WORD              W;
  BYTE              Font;
  const TBalloon*   Balloon;
  BYTE*             (*GetText)(void* o);
  BYTE              LetterSpacing;
  TLcdHorzAlignment TextAlignment;
  DWORD             Tag;
} TLcdEdit;

typedef struct {
  TLcdSpin          UpSpin;
  TLcdSpin          DnSpin;
  TLcdEdit          Edit;
  WORD              X;
  WORD              Y;
  WORD              W;
  WORD              H;
  INT32             Value;
  INT32             Min;
  INT32             Max;
  BYTE              Comma;
  BYTE              Text[20];
  BYTE              TextLen;
  BYTE              Pos;
  BOOL              Wrap;
  BOOL              EnableLR;
  BOOL              WrapLR;
  BYTE              LetterW;
  BYTE              CommaLetterW;
  const WORD*       ResIds;
  BYTE*             (*GetText)(void* o);
  TColor            InactiveTextColor;
  BYTE*             Exp;
} TLcdSpinEdit;

typedef struct {
  WORD              X;
  WORD              Y;
  WORD              W;
  WORD              H;
  FLOAT32           KX;
  const TBalloon*   Balloon;
  FLOAT32           Min;
  FLOAT32           Max;
  FLOAT32           Val;
  WORD              ValPos;
  WORD              MinPos;
  WORD              MaxPos;
  BYTE              Range;
  BOOL              RangeEnabled;
} TLcdGauge;

typedef struct {
  WORD              X;
  WORD              Y;
  WORD              W;
  WORD              H;
  const TPicture*   PadPic;
  const TPicture*   BoxPic;
} TLcdSlider;

typedef struct {
  BIT Draw:             1;
  BIT Repaint:          1;
  BIT Blink:            1;
  BIT Blinked:          1;
} TLcdFlags;

#define LcdRepaint()    LcdFlags.Repaint = 1

extern NOINIT TLcdFlags LcdFlags;
extern NOINIT BYTE LcdText[LCD_TEXT_LEN];
extern NOINIT WORD LcdLineThickness;
extern NOINIT BYTE LcdFont;
extern NOINIT WORD LcdFontHeight;
extern NOINIT WORD LcdFontAscent;
extern NOINIT WORD LcdFontDescent;
extern NOINIT TColor LcdBgColorDef;
extern NOINIT TColor LcdFgColorDef;
extern NOINIT TColor LcdTextColorDef;
extern NOINIT TColor LcdLabelTextColorDef;
extern NOINIT TColor LcdCaptionTextColorDef;
extern NOINIT TColor LcdHintTextColorDef;
extern NOINIT TColor LcdHintLineColorDef;
extern NOINIT TColor LcdEvenRowTextColorDef;
extern NOINIT TColor LcdButtonTextColorDef;
extern NOINIT TColor LcdSelButtonTextColorDef;
extern NOINIT TColor LcdEditTextColorDef;
extern NOINIT TColor LcdGaugeBgColorDef;
extern NOINIT TColor LcdGaugeFgColorDef;
extern NOINIT TColor LcdStatusBgColor;
extern NOINIT TColor LcdStatusFgColor;
extern NOINIT TColor LcdStatusSepColor;
extern NOINIT TColor LcdWebLinkColor;
extern NOINIT BOOL LcdButtonFlashing;
extern NOINIT const TPicture* LcdPictureCharge;
extern NOINIT const TBalloon* LcdMenuBalloon;
extern NOINIT const TBalloon* LcdEditBalloon;
extern NOINIT const TBalloon* LcdEditSmallBalloon;
extern NOINIT const TBalloon* LcdSpinDarkBalloon;
extern NOINIT const TBalloon* LcdSpinLightBalloon;
extern NOINIT const TBalloon* LcdButtonBalloon;
extern NOINIT const TBalloon* LcdSelButtonBalloon;
extern NOINIT WORD LcdLetterSpacing;
extern NOINIT WORD LcdLettersPos[];
extern NOINIT WORD LcdLettersWidth[];
extern NOINIT TLcdMenu LcdMenu;
extern NOINIT TLcdSpinEdit LcdSpinEdit;
extern NOINIT TLcdGauge LcdGauge;
extern NOINIT TLcdSlider LcdSlider;
extern NOINIT TLcdRect LcdRect1;
extern NOINIT TLcdRect LcdRect2;
extern NOINIT TLcdRect LcdRect3;

void InitLcd(void);
void SetupLcd(void);
void DoLcd(void);

void LcdApplyTheme(void);
__ramfunc void LcdFillBg(WORD n);
__ramfunc void LcdFillFg(WORD n);
TColor LcdRgbToColor(BYTE r, BYTE g, BYTE b);
TColor LcdGetBgColor(void);
void LcdSetBgColor(TColor color);
void LcdSetBgColorDef(void);
TColor LcdGetFgColor(void);
void LcdSetFgColor(TColor color);
void LcdSetFgColorDef(void);
TColor LcdGetTextColor(void);
void LcdSetTextColor(TColor color);
void LcdSetTextColorDef(void);
void LcdSetLabelTextColorDef(void);
#define LcdSetOddRowTextColorDef() LcdSetLabelTextColorDef()
void LcdSetEvenRowTextColorDef(void);
void LcdSetFont(BYTE font);
void LcdSetFontDef(void);
void LcdSetLineThickness(WORD thickness);
void LcdSetLetterSpacing(BYTE spacing);
void LcdSetLetterSpacingDef(void);
WORD LcdGetTextWidth(BYTE* txt, BYTE n);
WORD LcdGetResTextWidth(WORD id);
WORD LcdGetLangTextWidth(WORD id);
WORD LcdGetLangTextWidthMax(WORD id, WORD n);
void LcdDrawBegin(void);
void LcdDrawEnd(void);
void LcdDrawBackground(void);
void LcdDrawWorkspace(void);
void LcdDrawWorkspaceWithLangCaption(WORD id);
void LcdDrawDot(WORD x, WORD y);
DWORD LcdGetPixel(WORD x, WORD y);
void LcdGetPixels(WORD x, WORD y, WORD w, WORD h, BYTE* buff);
void LcdDrawLine(INT16 x0, INT16 y0, INT16 x1, INT16 y1);
void LcdDrawLineImpl(INT16 x0, INT16 y0, INT16 x1, INT16 y1,
 void (*proc)(WORD x, WORD y));
void LcdDrawHorzLine(WORD x, WORD y, WORD len);
void LcdDrawVertLine(WORD x, WORD y, WORD len);
void LcdDrawRect(WORD x, WORD y, WORD w, WORD h);
void LcdDrawFrame(WORD x, WORD y, WORD w, WORD h);
void LcdDrawPic8(WORD x, WORD y, const TPicture8* pic);
void LcdDrawPic16(WORD x, WORD y, const TPicture16* pic);
void LcdDrawTrPic8(WORD x, WORD y, const TPicture8* pic);
void LcdDrawTrPic16(WORD x, WORD y, const TPicture16* pic);
void LcdDrawLaRectPic8(TLcdRect* rect, WORD x, const TPicture8* pic);
void LcdDrawLaRectPic16(TLcdRect* rect, WORD x, const TPicture16* pic);
void LcdDrawLaText(WORD x, WORD y, BYTE* txt, BYTE n);
void LcdDrawRaText(WORD x, WORD y, BYTE* txt, BYTE n);
void LcdDrawCaText(WORD x, WORD y, BYTE* txt, BYTE n);
void LcdDrawLaResText(WORD x, WORD y, WORD id);
void LcdDrawRaResText(WORD x, WORD y, WORD id);
void LcdDrawCaResText(WORD x, WORD y, WORD id);
void LcdDrawLaLangText(WORD x, WORD y, WORD id);
void LcdDrawRaLangText(WORD x, WORD y, WORD id);
void LcdDrawCaLangText(WORD x, WORD y, WORD id);
void LcdDrawLaRectText(const TLcdRect* rect, WORD x, WORD y, BYTE* txt, BYTE n);
void LcdDrawRaRectText(const TLcdRect* rect, WORD x, WORD y, BYTE* txt, BYTE n);
void LcdDrawCaRectText(const TLcdRect* rect, WORD y, BYTE* txt, BYTE n);
void LcdDrawLaRectResText(const TLcdRect* rect, WORD x, WORD y, WORD id);
void LcdDrawRaRectResText(const TLcdRect* rect, WORD x, WORD y, WORD id);
void LcdDrawCaRectResText(const TLcdRect* rect, WORD y, WORD id);
void LcdDrawLaRectLangText(const TLcdRect* rect, WORD x, WORD y, WORD id);
void LcdDrawRaRectLangText(const TLcdRect* rect, WORD x, WORD y, WORD id);
void LcdDrawCaRectLangText(const TLcdRect* rect, WORD y, WORD id);
void LcdDrawLaRectTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 BYTE* txt, BYTE n, WORD pic_x, const TPicture8* pic);
void LcdDrawLaRectResTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture8* pic);
void LcdDrawLaRectLangTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture8* pic);
void LcdDrawLaRectTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 BYTE* txt, BYTE n, WORD pic_x, const TPicture16* pic);
void LcdDrawLaRectResTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture16* pic);
void LcdDrawLaRectLangTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 WORD id, WORD pic_x, const TPicture16* pic);
void LcdDrawLaBalloonText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n);
void LcdDrawRaBalloonText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n);
void LcdDrawCaBalloonText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n);
void LcdDrawLaBalloonResText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawRaBalloonResText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawCaBalloonResText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawLaBalloonLangText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawRaBalloonLangText(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawCaBalloonLangText(const TLcdRect* rect, WORD y,
 const TBalloon* balloon, WORD id);
void LcdDrawCaBalloonPic8(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture8* pic);
void LcdDrawCaBalloonPic16(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture16* pic);
void LcdDrawCaBalloonTrPic8(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture8* pic);
void LcdDrawCaBalloonTrPic16(const TLcdRect* rect,
 const TBalloon* balloon, const TPicture16* pic);
void LcdDrawLaBalloonTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n, WORD pic_x, const TPicture8* pic);
void LcdDrawLaBalloonResTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture8* pic);
void LcdDrawLaBalloonLangTextWithTrPic8(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture8* pic);
void LcdDrawLaBalloonTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, BYTE* txt, BYTE n, WORD pic_x, const TPicture16* pic);
void LcdDrawLaBalloonResTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture16* pic);
void LcdDrawLaBalloonLangTextWithTrPic16(const TLcdRect* rect, WORD x, WORD y,
 const TBalloon* balloon, WORD id, WORD pic_x, const TPicture16* pic);
void LcdDrawCaption(BYTE* txt, BYTE n);
void LcdDrawResCaption(WORD id);
void LcdDrawLangCaption(WORD id);
void LcdDrawLangCaptionN(WORD id, WORD rows);
void LcdDrawCaptionY(WORD y, BYTE* txt, BYTE n);
void LcdDrawResCaptionY(WORD y, WORD id);
void LcdDrawLangCaptionY(WORD y, WORD id);
void LcdDrawCustom(WORD x, WORD y, WORD w, WORD h, void* data,
 TColor (*proc)(WORD x, WORD y, void* data));
void LcdDrawLaFactor(WORD x, WORD y, BYTE* factor_str, BYTE n);
void LcdDrawLaAutoSizedRectText(TLcdRect* r, BYTE* txt, BYTE n, BOOL redraw);
void LcdDrawRaAutoSizedRectText(TLcdRect* r, BYTE* txt, BYTE n, BOOL redraw);
void LcdDrawCaAutoSizedRectText(TLcdRect* r, WORD x, BYTE* txt, BYTE n,
 BOOL redraw);

void LcdMenuCreate(TLcdMenu* menu, const WORD* items);
void LcdMenuSetup(TLcdMenu* menu);
void LcdMenuDraw(TLcdMenu* menu);
void LcdMenuDrawSelected(TLcdMenu* menu);
void LcdMenuNext(TLcdMenu* menu);
void LcdMenuPrev(TLcdMenu* menu);
void LcdMenuFactoryCreateEasy(const WORD* items, WORD selected);
void LcdMenuFactoryCreateExt(const WORD* items, WORD selected,
 BYTE* (get_text)(WORD index));
void LcdMenuDo(TLcdMenu* menu);

void LcdButtonCreate(TLcdButton* btn, WORD id);
void LcdButtonSetup(TLcdButton* btn);
void LcdButtonDraw(TLcdButton* btn);
void LcdButtonSelect(TLcdButton* btn, BOOL selected);
void LcdButtonDo(TLcdButton* btn);

void LcdDrawLangHintIni(WORD f1_id, WORD f2_id, WORD f3_id);
void LcdDrawLangHint(WORD f1_id, WORD f2_id, WORD f3_id);

void LcdSpinCreate(TLcdSpin* spin, const TPicture* pic);
void LcdSpinSetup(TLcdSpin* spin);
void LcdSpinDraw(TLcdSpin* spin);
void LcdSpinDo(TLcdSpin* spin);

void LcdEditCreate(TLcdEdit* edit);
void LcdEditSetup(TLcdEdit* edit);
void LcdEditDraw(TLcdEdit* edit);
void LcdEditDo(TLcdEdit* edit);

void LcdSpinEditCreate(TLcdSpinEdit* spin_edit);
void LcdSpinEditCreateSign(TLcdSpinEdit* spin_edit);
void LcdSpinEditSetup(TLcdSpinEdit* spin_edit);
void LcdSpinEditDraw(TLcdSpinEdit* spin_edit);
void LcdSpinEditDo(TLcdSpinEdit* spin_edit);
void LcdSpinEditGetPos(TLcdSpinEdit* spin_edit, TLcdRect* rect);
void LcdSpinEditUnmap(TLcdSpinEdit* spin_edit);

void LcdGaugeCreate(TLcdGauge* gauge);
void LcdGaugeSetup(TLcdGauge* gauge);
void LcdGaugeDraw(TLcdGauge* gauge);
void LcdGaugeSetValue(TLcdGauge* gauge, FLOAT32 value, BOOL redraw);

void LcdSliderCreate(TLcdSlider* slider);
void LcdSliderSetup(TLcdSlider* slider);
void LcdSliderDraw(TLcdSlider* slider);
void LcdSliderSetPos(TLcdSlider* slider, FLOAT32 new_pos);

void LcdSetContrast(BYTE contrast);

#include "lcd_pd.h"

#endif
