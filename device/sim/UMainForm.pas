unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus, StdCtrls, UGlobal, UArchiveMemForm,
  UDeviceVarsForm, UTypes, UDeviceVars, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnF3DnImage: TImage;
    BtnLeftPaintBox: TPaintBox;
    BtnF1PaintBox: TPaintBox;
    BtnF3UpImage: TImage;
    BtnDownDnImage: TImage;
    BtnMenuPaintBox: TPaintBox;
    BtnDownPaintBox: TPaintBox;
    BtnMinusUpImage: TImage;
    BtnF3PaintBox: TPaintBox;
    BtnMinusDnImage: TImage;
    BtnRightPaintBox: TPaintBox;
    BtnMinusPaintBox: TPaintBox;
    BtnPlusUpImage: TImage;
    BtnMaskImage: TImage;
    BtnMeasurePaintBox: TPaintBox;
    BtnUpPaintBox: TPaintBox;
    BtnMenuUpImage: TImage;
    BtnPowerPaintBox: TPaintBox;
    BtnPlusPaintBox: TPaintBox;
    BtnF2PaintBox: TPaintBox;
    BtnUpUpImage: TImage;
    BtnDownUpImage: TImage;
    BtnLeftUpImage: TImage;
    BtnRightUpImage: TImage;
    BtnF1UpImage: TImage;
    BtnMeasureUpImage: TImage;
    BtnF2UpImage: TImage;
    BtnPowerUpImage: TImage;
    BtnF1DnImage: TImage;
    BtnPlusDnImage: TImage;
    BtnMenuDnImage: TImage;
    BtnLeftDnImage: TImage;
    BtnF2DnImage: TImage;
    BtnRightDnImage: TImage;
    BtnUpDnImage: TImage;
    BtnMeasureDnImage: TImage;
    BtnPowerDnImage: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CopyBtn: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MeasureTEdit: TEdit;
    MeasureNoiseEdit: TEdit;
    MeasureNoiseScrollBar: TScrollBar;
    MeasureUavrEdit: TEdit;
    MeasureUampEdit: TEdit;
    MeasureUpeakEdit: TEdit;
    MeasureTLbl1: TLabel;
    MeasureTLbl2: TLabel;
    MeasureTLbl3: TLabel;
    MeasureUavrScrollBar: TScrollBar;
    MeasureUampScrollBar: TScrollBar;
    MeasureUpeakScrollBar: TScrollBar;
    Panel1: TPanel;
    MeasureTLbl: TLabel;
    MeasureTScrollBar: TScrollBar;
    MeasureNoiseLbl: TLabel;
    SupplySourceVoltageScrollBar: TScrollBar;
    SupplySourceVoltageEdit: TEdit;
    SupplySourceVoltageLbl: TLabel;
    Label2: TLabel;
    LoadDeviceBtn: TButton;
    MainMenu: TMainMenu;
    LcdPaintBox: TPaintBox;
    BtnsPanel: TPanel;
    MenuItem1: TMenuItem;
    DeviceVarsMenuItem: TMenuItem;
    ArchiveMemMenuItem: TMenuItem;
    RedrawTimer: TTimer;
    procedure ArchiveMemMenuItemClick(Sender: TObject);
    procedure BtnPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BtnPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnPaintBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMeasurePaintBoxPaint(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure LcdPaintBoxPaint(Sender: TObject);
    procedure DeviceVarsMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadDeviceBtnClick(Sender: TObject);
    procedure MainMenuClick(Sender: TObject);
    procedure MeasureUavrEditEditingDone(Sender: TObject);
    procedure MeasureUampEditEditingDone(Sender: TObject);
    procedure MeasureUpeakEditEditingDone(Sender: TObject);
    procedure MeasureTEditEditingDone(Sender: TObject);
    procedure MeasureUavrScrollBarChange(Sender: TObject);
    procedure MeasureUampScrollBarChange(Sender: TObject);
    procedure MeasureUpeakScrollBarChange(Sender: TObject);
    procedure MeasureTScrollBarChange(Sender: TObject);
    procedure MeasureNoiseEditEditingDone(Sender: TObject);
    procedure MeasureNoiseScrollBarChange(Sender: TObject);
    procedure RedrawTimerTimer(Sender: TObject);
    procedure SupplySourceVoltageEditEditingDone(Sender: TObject);
    procedure SupplySourceVoltageScrollBarChange(Sender: TObject);
  private
    { private declarations }
    FBufferedBtnBitmap: TBitmap;
    FBufferedLcdBitmap: TBitmap;
    FDeviceBtnDown:  array[0..DeviceButtonsCount - 1] of Boolean;
    FDeviceLcdBitmap: TBitmap;
    FActiveDeviceBtnIndex: Integer;
    FHoldDeviceBtns: set of Byte;
    function GetDeviceBtnIndex(Pb: TPaintBox): Integer;
    function GetDeviceBtnIndex(BtnName: String): Integer;
    function IsPointOnBtn(X, Y: Integer): Boolean;
    function IsDeviceBtnHolding(Index: Integer): Boolean;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure RedrawDeviceLcd;
    procedure DetectDeviceKeys;
    function SupplySourceVoltageToStr(Value: Float32): String;
    function MeasureTtoStr(Value: Float32): String;
    function MeasureNoiseToStr(Value: Float32): String;
    function MeasureUtoStr(Value: Float32): String;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses Windows, UDevice;

const
  LoadBtnCaption = 'Load dll';
  UnloadBtnCaption = 'Unload dll';

{$R *.lfm}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  FBufferedBtnBitmap.Free;
  FBufferedLcdBitmap.Free;
  FDeviceLcdBitmap.Free;
  UGlobal.Save;
end;

procedure TMainForm.LoadDeviceBtnClick(Sender: TObject);
begin
  if IsDeviceLibLoaded then begin
    FreeDeviceLib;
    LoadDeviceBtn.Caption := LoadBtnCaption;
  end
  else begin
    LoadDeviceLib;
    if IsDeviceLibLoaded then begin
      LoadDeviceBtn.Caption := UnloadBtnCaption;
    end
    else begin
      MessageBox(Handle, 'Failed to load device library "incvist.dll"',
        'Error', MB_ICONSTOP or MB_OK);
    end;
  end;
end;

procedure TMainForm.MainMenuClick(Sender: TObject);
var
  F: Boolean;
begin
  F := False;
  if Assigned(DeviceVarsForm) then
    F := DeviceVarsForm.Visible;
  DeviceVarsMenuItem.Checked := F;

  F := False;
  if Assigned(ArchiveMemForm) then
    F := ArchiveMemForm.Visible;
  ArchiveMemMenuItem.Checked := F;
end;

procedure TMainForm.MeasureUavrEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    if Val < MeasureUmin then
      Val := MeasureUmin
    else if Val > MeasureUmax then
      Val := MeasureUmax;
    MeasureUavr := Val;
    MeasureUavrScrollBar.OnChange := nil;
    MeasureUavrScrollBar.Position := Trunc(MeasureUavr*1000);
    MeasureUavrScrollBar.OnChange := @MeasureUavrScrollBarChange;
  except
    Val := MeasureUavr;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 3);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.MeasureUampEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    if Val < MeasureUmin then
      Val := MeasureUmin
    else if Val > MeasureUmax then
      Val := MeasureUmax;
    MeasureUamp := Val;
    MeasureUampScrollBar.OnChange := nil;
    MeasureUampScrollBar.Position := Trunc(MeasureUamp*1000);
    MeasureUampScrollBar.OnChange := @MeasureUampScrollBarChange;
  except
    Val := MeasureUamp;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 3);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.MeasureUpeakEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    if Val < MeasureUmin then
      Val := MeasureUmin
    else if Val > MeasureUmax then
      Val := MeasureUmax;
    MeasureUpeak := Val;
    MeasureUpeakScrollBar.OnChange := nil;
    MeasureUpeakScrollBar.Position := Trunc(MeasureUpeak*1000);
    MeasureUpeakScrollBar.OnChange := @MeasureUpeakScrollBarChange;
  except
    Val := MeasureUpeak;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 3);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.MeasureTEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    if Val < 1.0 then
      Val := 1.0
    else if Val > 1000.0 then
      Val := 1000.0;
    MeasureT := Val/1000;
    MeasureTScrollBar.OnChange := nil;
    MeasureTScrollBar.Position := Trunc(Log10(MeasureT)*1000);
    MeasureTScrollBar.OnChange := @MeasureTScrollBarChange;
  except
    Val := MeasureT;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 3);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.MeasureUavrScrollBarChange(Sender: TObject);
begin
  MeasureUavrEdit.OnEditingDone := nil;
  MeasureUavr := MeasureUavrScrollBar.Position*0.001;
  MeasureUavrEdit.Text := MeasureUtoStr(MeasureUavr);
  MeasureUavrEdit.OnEditingDone := @MeasureUavrEditEditingDone;
end;

procedure TMainForm.MeasureUampScrollBarChange(Sender: TObject);
begin
  MeasureUampEdit.OnEditingDone := nil;
  MeasureUamp := MeasureUampScrollBar.Position*0.001;
  MeasureUampEdit.Text := MeasureUtoStr(MeasureUamp);
  MeasureUampEdit.OnEditingDone := @MeasureUampEditEditingDone;
end;

procedure TMainForm.MeasureUpeakScrollBarChange(Sender: TObject);
begin
  MeasureUpeakEdit.OnEditingDone := nil;
  MeasureUpeak := MeasureUpeakScrollBar.Position*0.001;
  MeasureUpeakEdit.Text := MeasureUtoStr(MeasureUpeak);
  MeasureUpeakEdit.OnEditingDone := @MeasureUpeakEditEditingDone;
end;

procedure TMainForm.MeasureTScrollBarChange(Sender: TObject);
begin
  MeasureTEdit.OnEditingDone := nil;
  MeasureT := Power(10, MeasureTScrollBar.Position*0.001);
  MeasureTEdit.Text := MeasureTtoStr(MeasureT);
  MeasureTEdit.OnEditingDone := @MeasureTEditEditingDone;
end;

procedure TMainForm.MeasureNoiseEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    MeasureNoise := Val;
    MeasureNoiseScrollBar.OnChange := nil;
    MeasureNoiseScrollBar.Position := Trunc(MeasureNoise*1000);
    MeasureNoiseScrollBar.OnChange := @MeasureNoiseScrollBarChange;
  except
    Val := MeasureNoise;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 2);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.MeasureNoiseScrollBarChange(Sender: TObject);
begin
  MeasureNoiseEdit.OnEditingDone := nil;
  MeasureNoise := MeasureNoiseScrollBar.Position*0.001;
  MeasureNoiseEdit.Text := MeasureNoiseToStr(MeasureNoise);
  MeasureNoiseEdit.OnEditingDone := @MeasureNoiseEditEditingDone;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I:  Integer;
  Dv: TDeviceVar;
begin
  UGlobal.Load;

  BtnsPanel.Color := clBlack;

  FBufferedBtnBitmap        := Graphics.TBitmap.Create;
  FBufferedBtnBitmap.Width  := BtnMeasureDnImage.Width;
  FBufferedBtnBitmap.Height := BtnMeasureDnImage.Height;

  FBufferedLcdBitmap        := Graphics.TBitmap.Create;
  FBufferedLcdBitmap.Width  := LcdPaintBox.Width;
  FBufferedLcdBitmap.Height := LcdPaintBox.Height;

  for I := 0 to DeviceButtonsCount - 1 do
  begin
    FDeviceBtnDown[I] := False;
  end;

  FActiveDeviceBtnIndex := -1;
  FHoldDeviceBtns       := [];

  FDeviceLcdBitmap := Graphics.TBitmap.Create;
  with FDeviceLcdBitmap do begin
    SetSize(DeviceLcdW, DeviceLcdH);
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, 0, DeviceLcdW, DeviceLcdH);
  end;

  if IsDeviceLibLoaded then begin
    LoadDeviceBtn.Caption := UnloadBtnCaption;
  end
  else begin
    LoadDeviceBtn.Caption := LoadBtnCaption;
  end;

  Dv := Device.Vars['SupplySourceVoltage'];
  if Assigned(Dv) then Dv.Float32Value := SupplySourceVoltage;

  SupplySourceVoltageEdit.OnEditingDone := nil;
  SupplySourceVoltageScrollBar.OnChange := nil;
  SupplySourceVoltageEdit.Text := SupplySourceVoltageToStr(SupplySourceVoltage);
  SupplySourceVoltageScrollBar.Position := Trunc(SupplySourceVoltage*1000);
  SupplySourceVoltageEdit.OnEditingDone := @SupplySourceVoltageEditEditingDone;
  SupplySourceVoltageScrollBar.OnChange := @SupplySourceVoltageScrollBarChange;
  MeasureTEdit.OnEditingdone := nil;
  MeasureTScrollBar.OnChange := nil;
  MeasureTEdit.Text := MeasureTtoStr(MeasureT);
  MeasureTScrollBar.Position := Trunc(Log10(MeasureT)*1000);;
  MeasureTEdit.OnEditingDone := @MeasureTEditEditingDone;
  MeasureTScrollBar.OnChange := @MeasureTScrollBarChange;
  MeasureNoiseEdit.OnEditingdone := nil;
  MeasureNoiseScrollBar.OnChange := nil;
  MeasureNoiseEdit.Text := MeasureNoiseToStr(MeasureNoise);
  MeasureNoiseScrollBar.Position := Trunc(MeasureNoise*1000);
  MeasureNoiseEdit.OnEditingDone := @MeasureNoiseEditEditingDone;
  MeasureNoiseScrollBar.OnChange := @MeasureNoiseScrollBarChange;
  MeasureUavrEdit.OnEditingDone := nil;
  MeasureUavrScrollBar.OnChange := nil;
  MeasureUavrEdit.Text := MeasureUtoStr(MeasureUavr);
  MeasureUavrScrollBar.Position := Trunc(MeasureUavr*1000);
  MeasureUavrEdit.OnEditingDone := @MeasureUavrEditEditingDone;
  MeasureUavrScrollBar.OnChange := @MeasureUavrScrollBarChange;
  MeasureUampEdit.OnEditingDone := nil;
  MeasureUampScrollBar.OnChange := nil;
  MeasureUampEdit.Text := MeasureUtoStr(MeasureUamp);
  MeasureUampScrollBar.Position := Trunc(MeasureUamp*1000);
  MeasureUampEdit.OnEditingDone := @MeasureUampEditEditingDone;
  MeasureUampScrollBar.OnChange := @MeasureUampScrollBarChange;
  MeasureUpeakEdit.OnEditingDone := nil;
  MeasureUpeakScrollBar.OnChange := nil;
  MeasureUpeakEdit.Text := MeasureUtoStr(MeasureUpeak);
  MeasureUpeakScrollBar.Position := trunc(MeasureUpeak*1000);
  MeasureUpeakEdit.OnEditingdone := @MeasureUpeakEditEditingDone;
  MeasureUpeakScrollBar.OnChange := @MeasureUpeakScrollBarChange;

  RedrawTimer.Enabled := True;
  Application.OnIdle := @Idle;
end;

procedure TMainForm.LcdPaintBoxPaint(Sender: TObject);
var
  Rect: TRect;
begin
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := LcdPaintBox.Width;
  Rect.Bottom := LcdPaintBox.Height;

  with FBufferedLcdBitmap.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect);
    Pen.Color := clBlack;
    MoveTo(0,              0);
    LineTo(Rect.Right - 1, 0);
    LineTo(Rect.Right - 1, Rect.Bottom - 1);
    LineTo(0,              Rect.Bottom - 1);
    LineTo(0,              0);
  end;

  LcdPaintBox.Canvas.Draw(0, 0, FBufferedLcdBitmap);
end;

procedure TMainForm.DeviceVarsMenuItemClick(Sender: TObject);
begin
  if not Assigned(DeviceVarsForm) then begin
    DeviceVarsForm := TDeviceVarsForm.Create(Application);
    DeviceVarsForm.Left := (Screen.Width - DeviceVarsForm.Width) div 2;
    DeviceVarsForm.Top := (Screen.Height - DeviceVarsForm.Height) div 2;
  end;
  DeviceVarsForm.Visible := not DeviceVarsForm.Visible;
end;

procedure TMainForm.ArchiveMemMenuItemClick(Sender: TObject);
begin
  if not Assigned(ArchiveMemForm) then begin
    ArchiveMemForm := TArchiveMemForm.Create(Application);
    with ArchiveMemForm do begin
      Left := (Screen.Width - Width) div 2;
      Top := (Screen.Height - Height) div 2;
    end;
  end;
  ArchiveMemForm.Visible := not ArchiveMemForm.Visible;
end;

procedure TMainForm.BtnPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Pb:      TPaintBox;
  Index:   Integer;
  OldDown: Boolean;
begin
  Pb    := Sender as TPaintBox;
  Index := GetDeviceBtnIndex(Pb);
  if Index < 0 then Exit;
  if FActiveDeviceBtnIndex = Index then
  begin
    if not IsDeviceBtnHolding(Index) then
    begin
      OldDown               := FDeviceBtnDown[Index];
      FDeviceBtnDown[Index] := IsPointOnBtn(X, Y);
      if OldDown <> FDeviceBtnDown[Index] then
        Pb.Invalidate;
    end;
  end;
  DetectDeviceKeys;
end;

procedure TMainForm.BtnPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pb:    TPaintBox;
  Index: Integer;
begin
  Pb := Sender as TPaintBox;
  Index := GetDeviceBtnIndex(Pb);
  if Index < 0 then Exit;
  if FActiveDeviceBtnIndex = Index then
  begin
    FActiveDeviceBtnIndex := -1;
    if not IsDeviceBtnHolding(Index) then
    begin
      FDeviceBtnDown[Index] := False;
      Pb.Invalidate;
    end;
  end;
  DetectDeviceKeys;
end;

procedure TMainForm.BtnPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pb:    TPaintBox;
  Index: Integer;
begin
  Pb    := Sender as TPaintBox;
  Index := GetDeviceBtnIndex(Pb);
  if Index < 0 then Exit;
  if IsPointOnBtn(X, Y) then
  begin
    if FDeviceBtnDown[Index] then
    begin
      FActiveDeviceBtnIndex := -1;
      FDeviceBtnDown[Index] := False;
      if IsDeviceBtnHolding(Index) then
        FHoldDeviceBtns := FHoldDeviceBtns - [Byte(Index)];
    end
    else
    begin
      FActiveDeviceBtnIndex := Index;
      FDeviceBtnDown[Index] := True;
      if ssCtrl in Shift then
        FHoldDeviceBtns := FHoldDeviceBtns + [Byte(Index)];
      if Index = GetDeviceBtnIndex('power') then
      begin
        if (not IsDeviceLibLoaded) or (Device.DeviceOn) then
          LoadDeviceBtn.Click;
      end;
    end;
    Pb.Refresh;
  end;
  DetectDeviceKeys;
end;

procedure TMainForm.BtnMeasurePaintBoxPaint(Sender: TObject);
var
  Pb:    TPaintBox;
  Rect:  TRect;
  Index: Integer;
  F:     Boolean;
  C:     TComponent;
  Txt:   String;
begin
  Pb    := Sender as TPaintBox;
  Index := GetDeviceBtnIndex(Pb);
  if Index < 0 then Exit;

  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := Pb.Width;
  Rect.Bottom := Pb.Height;

  with FBufferedBtnBitmap.Canvas do
  begin
    Brush.Color := BtnsPanel.Color;
    FillRect(Rect);
    CopyMode := cmSrcAnd;
    Draw(0, 0, BtnMaskImage.Picture.Bitmap);
    CopyMode := cmSrcPaint;
    F := FDeviceBtnDown[Index];
    if F
      then Txt := 'Dn'
      else Txt := 'Up';
    Txt := 'Btn' + DeviceButtonNames[Index] + Txt + 'Image';
    C := FindComponent(Txt);
    if Assigned(C) then
      Draw(0, 0, (C as TImage).Picture.Bitmap);
  end;

  Pb.Canvas.Draw(0, 0, FBufferedBtnBitmap);
end;

procedure TMainForm.CopyBtnClick(Sender: TObject);
begin
  FDeviceLcdBitmap.SaveToClipboardFormat(CF_BITMAP);
end;

procedure TMainForm.RedrawTimerTimer(Sender: TObject);
begin
  RedrawDeviceLcd;
end;

procedure TMainForm.SupplySourceVoltageEditEditingDone(Sender: TObject);
var
  Txt:    String;
  NewTxt: String;
  Val:    Float32;
begin
  Txt := (Sender as TEdit).Text;
  try
    Val := StrToFloat(Txt);
    SupplySourceVoltage := Val;
    SupplySourceVoltageScrollBar.OnChange := nil;
    SupplySourceVoltageScrollBar.Position := Trunc(SupplySourceVoltage*1000);
    SupplySourceVoltageScrollBar.OnChange := @SupplySourceVoltageScrollBarChange;
  except
    Val := SupplySourceVoltage;
  end;

  NewTxt := FloatToStrF(Val, ffFixed, 15, 2);
  if AnsiCompareStr(Txt, NewTxt) <> 0 then
    (Sender as TEdit).Text := NewTxt;
end;

procedure TMainForm.SupplySourceVoltageScrollBarChange(Sender: TObject);
begin
  SupplySourceVoltageEdit.OnEditingDone := nil;
  SupplySourceVoltage := SupplySourceVoltageScrollBar.Position*0.001;
  SupplySourceVoltageEdit.Text := SupplySourceVoltageToStr(SupplySourceVoltage);
  SupplySourceVoltageEdit.OnEditingDone := @SupplySourceVoltageEditEditingDone;
end;

procedure TMainForm.RedrawDeviceLcd;
begin
  LcdPaintBox.Canvas.Draw(0, 0, FDeviceLcdBitmap);
end;

function TMainForm.IsDeviceBtnHolding(Index: Integer): Boolean;
begin
  Result := False;
  if Index < 0 then Exit;
  if Index >= DeviceButtonsCount then Exit;
  Result := Byte(Index) in FHoldDeviceBtns;
end;

procedure TMainForm.DetectDeviceKeys;
var
  KeysTmp: Word;
  I:       Integer;

procedure SetBit(Index: Integer);
begin
  KeysTmp := KeysTmp or (1 shl Index);
end;

begin
  KeysTmp := 0;
  for I := 0 to DeviceButtonsCount - 1 do
    if FDeviceBtnDown[I] then SetBit(I);
  DeviceKeys := KeysTmp;
end;

procedure TMainForm.Idle(Sender: TObject; var Done: Boolean);
begin
  Done := False;
  DetectDeviceKeys;
  DeviceThread.Execute;
  if DeviceThread.LcdUpdated then
  begin
    DeviceThread.Draw(FDeviceLcdBitmap.Canvas);
    DeviceThread.LcdUpdated := False;
  end;
end;

function TMainForm.GetDeviceBtnIndex(Pb: TPaintBox): Integer;
var
  I:   Integer;
  Txt: String;
begin
  Result := -1;
  if not Assigned(Pb) then Exit;

  for I := 0 to DeviceButtonsCount - 1 do
  begin
    Txt := 'Btn' + DeviceButtonNames[I] + 'PaintBox';
    if AnsiCompareText(Txt, Pb.Name) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TMainForm.GetDeviceBtnIndex(BtnName: String): Integer;
var
  I:   Integer;
  Txt: String;
begin
  Result := -1;
  for I := 0 to DeviceButtonsCount - 1 do
  begin
    Txt := DeviceButtonNames[I + Low(DeviceButtonNames)];
    if AnsiCompareText(Txt, BtnName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TMainForm.IsPointOnBtn(X, Y: Integer): Boolean;
var
  BtnW,
  BtnH: Integer;
begin
  Result := False;

  BtnW := BtnMaskImage.Width;
  BtnH := BtnMaskImage.Height;

  if X < 0 then Exit;
  if Y < 0 then Exit;
  if X >= BtnW then Exit;
  if Y >= BtnH then Exit;

  Result := BtnMaskImage.Canvas.Pixels[X, Y] = clBlack;
end;

function TMainForm.SupplySourceVoltageToStr(Value: Float32): String;
begin
  Result := FloatToStrF(Value, ffFixed, 15, 2);
end;

function TMainForm.MeasureTtoStr(Value: Float32): String;
begin
  Result := FloatToStrF(Value*1000, ffFixed, 15, 3);
end;

function TMainForm.MeasureNoiseToStr(Value: Float32): String;
begin
  Result := FloatToStrF(Value, ffFixed, 15, 1);
end;

function TMainForm.MeasureUtoStr(Value: Float32): String;
begin
  Result := FloatToStrF(Value, ffFixed, 15, 3);
end;

end.

