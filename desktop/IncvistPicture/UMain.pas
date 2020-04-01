unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, Menus, ComCtrls, Windows, ClipBrd, FPImage, FPCanvas,
  FPWritePNG, FPReadBMP;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileMenuItem: TMenuItem;
    LangMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    ReadMenuItem: TMenuItem;
    OptionsMenuItem: TMenuItem;
    DevScreenPaintBox: TPaintBox;
    DevScreenPanel: TPanel;
    MainToolBar: TToolBar;
    ReadBtn: TToolButton;
    CopyBtn: TToolButton;
    SaveAsBtn: TToolButton;
    SaveDlg: TSaveDialog;
    ToolButton1: TToolButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure DevScreenPaintBoxPaint(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LangMenuItemClick(Sender: TObject);
    procedure ReadMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
  private
    { private declarations }
    FShadowBmp: Graphics.TBitmap;
    procedure LanguageChanged;
    procedure ResizeForm;
    procedure SetMenuItemsState;
  protected
    procedure Loaded; override;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  UGlobal, UAbout, ULang, URead;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Loaded;
begin
  inherited;

  ResizeForm;
end;

procedure TMainForm.LanguageChanged;
begin
  Caption := Gti(1402) + ' ' + Gti(1406);
  FileMenuItem.Caption := Gti(100);
  CopyMenuItem.Caption := Gti(189);
  SaveAsMenuItem.Caption := Gti(123);
  ReadMenuItem.Caption := Gti(107);
  ExitMenuItem.Caption := Gti(112);
  OptionsMenuItem.Caption := Gti(102);
  LangMenuItem.Caption := Gti(108);
  HelpMenuItem.Caption := Gti(104);
  AboutMenuItem.Caption := Gti(110);
  ReadBtn.Hint := Gti(142);
  SaveAsBtn.Hint := Gti(144);
  CopyBtn.Hint := Gti(190);
  SaveDlg.Filter := Gti(1100);
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  TAboutForm.Execute;
end;

procedure TMainForm.CopyMenuItemClick(Sender: TObject);
begin
  Clipboard.Assign(DevScreenBmp);
end;

procedure TMainForm.DevScreenPaintBoxPaint(Sender: TObject);
var
  Pb: TPaintBox;
  X,Y: Integer;
  W,H: Integer;
  DevScrW, DevScrH: Integer;
  BorderPoints: array[0..4] of TPoint;
begin
  Pb := Sender as TPaintBox;
  W := Pb.Width;
  H := Pb.Height;
  if W < 1 then W := 1;
  if H < 1 then H := 1;

  DevScrW := GetDevScreenWidth;
  DevScrH := GetDevScreenHeight;

  if not Assigned(FShadowBmp) then
  begin
    FShadowBmp := Graphics.TBitmap.Create;
    FShadowBmp.PixelFormat := TPixelFormat.pf32bit;
  end;
  if (W <> FShadowBmp.Width) or (H <> FShadowBmp.Height) then
    FShadowBmp.SetSize(W, H);

  with FShadowBmp.Canvas do
  begin
    Brush.Color := clAppWorkspace;
    FillRect(0, 0, W, H);

    Pen.Width := DevScreenBorderWidth;
    Pen.Color := DevScreenBorderColor;
    X := (W - DevScrW) div 2;
    Y := (H - DevScrH) div 2;
    if X < DevScreenOutset then
      X := DevScreenOutset;
    if Y < DevScreenOutset then
      Y := DevScreenOutset;
    BorderPoints[0].x := X;
    BorderPoints[0].y := Y;
    BorderPoints[1].x := X + DevScrW + DevScreenBorderWidth;
    BorderPoints[1].y := BorderPoints[0].y;
    BorderPoints[2].x := BorderPoints[1].x;
    BorderPoints[2].y := Y + DevScrH + DevScreenBorderWidth;
    BorderPoints[3].x := BorderPoints[0].x;
    BorderPoints[3].y := BorderPoints[2].y;
    BorderPoints[4] := BorderPoints[0];
    Polyline(BorderPoints);

    Brush.Color := DevScreenColorDef;
    FillRect(X + DevScreenBorderWidth,
             Y + DevScreenBorderWidth,
             BorderPoints[1].x,
             BorderPoints[2].y);
    if Assigned(DevScreenBmp) then
      Draw(X + DevScreenBorderWidth,
           Y + DevScreenBorderWidth,
           DevScreenBmp);
  end;

  Pb.Canvas.Draw(0, 0, FShadowBmp);
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FShadowBmp := nil;
  BorderStyle := bsSingle;

  LoadTextRes;
  LoadPicRes;

  LanguageChanged;
  SetMenuItemsState;
  ResizeForm;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeObj(@FShadowBmp);
end;

procedure TMainForm.LangMenuItemClick(Sender: TObject);
var
  SavedLang: TLanguage;
begin
  SavedLang := Lang;
  TLangForm.Execute;
  if SavedLang <> Lang then
  begin
    LoadTextRes;
    LanguageChanged;
  end;
end;

procedure TMainForm.ReadMenuItemClick(Sender: TObject);
begin
  if TReadForm.Execute then
  begin
    if not Assigned(DevScreenBmp) then
    begin
      DevScreenBmp := Graphics.TBitmap.Create;
      DevScreenBmp.PixelFormat := TPixelFormat.pf32bit;
    end;

    DevScreenBmp.SetSize(DevScreenCachedBmp.Width,
                         DevScreenCachedBmp.Height);

    with DevScreenBmp.Canvas do
      Draw(0, 0, DevScreenCachedBmp);

    SetMenuItemsState;
    ResizeForm;
    DevScreenPaintBox.Invalidate;
  end;
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
var
  FileName: UTF8String;
  FileExt: UTF8String;
  JpgImg: TJpegImage = nil;
  FpImg: TFPCustomImage = nil;
  PngWriter: TFPWriterPNG = nil;
  BmpReader: TFPCustomImageReader = nil;
  MemStream: TMemoryStream = nil;
begin
  if DevScreenBmpFileNameAvailable then
    FileName := DevScreenBmpFileName
  else
    FileName := Gti(127);

  SaveDlg.FileName := FileName;
  if not SaveDlg.Execute then
    Exit;

  try
    DevScreenBmpFileNameAvailable := True;
    DevScreenBmpFileName := SaveDlg.FileName;
    FileExt := LowerCase(ExtractFileExt(DevScreenbmpFileName));
    if (CompareStr(FileExt, '.jpg')  = 0) or
       (CompareStr(FileExt, '.jpeg') = 0) then
    begin
      JpgImg := TJpegImage.Create;
      JpgImg.Assign(DevScreenBmp);
      JpgImg.SaveToFile(DevScreenBmpFileName);
    end
    else if CompareStr(FileExt, '.png') = 0 then
    begin
      MemStream := TMemoryStream.Create;
      DevScreenBmp.SaveToStream(MemStream);
      MemStream.Position := 0;
      FpImg := TFPMemoryImage.Create(DevScreenBmp.Width, DevScreenBmp.Height);
      BmpReader := TFPReaderBMP.Create;
      FpImg.LoadFromStream(MemStream, BmpReader);
      PngWriter := TFPWriterPNG.Create;
      PngWriter.Indexed := True;
      PngWriter.UseAlpha := False;
      FpImg.SaveToFile(DevScreenBmpFileName, PngWriter);
    end
    else
    begin
      DevScreenBmp.SaveToFile(DevScreenBmpFileName);
    end;
  except
    MessageBoxW(Handle, PWideChar(WideString(Gti(180) + DevScreenbmpFileName)),
     PWideChar(WideString(Gti(181))), MB_OK or MB_ICONERROR);
  end;

  FreeObj(@JpgImg);
  FreeObj(@MemStream);
  FreeObj(@BmpReader);
  FreeObj(@PngWriter);
  FreeObj(@FpImg);
end;

procedure TMainForm.ResizeForm;
const
  CanvasWmin = 200;
  CanvasHmin = 200;
var
  ReqCanvasW, ReqCanvasH: Integer;
  Tmp: Integer;
  Wp: TWindowPlacement;
  HorzInset: Integer;
  VertInset: Integer;
  ReqWidth: Integer;
  ReqHeight: Integer;
  Rect: TRect;
begin
  Tmp := 2*(DevScreenBorderWidth + DevScreenOutset);
  ReqCanvasW := GetDevScreenWidth + Tmp;
  ReqCanvasH := GetDevScreenHeight + Tmp;
  if ReqCanvasW < CanvasWmin then ReqCanvasW := CanvasWmin;
  if ReqCanvasH < CanvasHmin then ReqCanvasH := CanvasHmin;

  Wp.length := SizeOf(Wp);
  GetWindowPlacement(Handle, Wp);
  Rect := GetClientRect;
  HorzInset := (Wp.rcNormalPosition.Right - Wp.rcNormalPosition.Left) -
   (Rect.Right - Rect.Left);
  VertInset :=
   Wp.rcNormalPosition.Bottom - Wp.rcNormalPosition.Top - ClientHeight;
  ReqWidth := HorzInset + ReqCanvasW +
   DevScreenPanel.Width - DevScreenPaintBox.Width;
  ReqHeight := VertInset + ReqCanvasH +
   DevScreenPanel.Height - DevScreenPaintBox.Height;
  Width := ReqWidth;
  Height := ReqHeight;
end;

procedure TMainForm.SetMenuItemsState;
var
  F: Boolean;
begin
  F := Assigned(DevScreenBmp);
  CopyMenuItem.Enabled := F;
  SaveAsMenuItem.Enabled := F;
  CopyBtn.Enabled := F;
  SaveAsBtn.Enabled := F
end;

end.

