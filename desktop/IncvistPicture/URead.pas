unit URead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, UGlobal, UUsb, Windows;

const
  TIMEOUT_ERROR        = 1;
  WRONG_DEVICE         = 2;
  WRONG_VERSION        = 3;
  WRONG_MODEL          = 4;
  USER_ABORT           = 5;
  DATA_ERROR           = 6;


type
  { TReadException }
  TReadException = class(Exception)
  private
    FError: Integer;
  public
    constructor Create(Err: Integer);
    property Error: Integer read FError;
  end;

  { TReadThread }
  TReadThread = class(TThread)
  private
    FUsb: TUsb;
    FError: Integer;
    FProgress: Integer;
    FBulkData: PChar;
    FBulkDataSize: Integer;
    FBulkBuff: PChar;
    FBulkBuffSize: Integer;
    FData: PChar;
    FDataSize: Integer;
    FAbort: Boolean;
    FJobDone: Boolean;
    FScreenW: Integer;
    FScreenH: Integer;
    FSingleDotRead: Boolean;
  protected
    procedure Execute; override;
    function Read: Boolean;
    function GetBulk(Size: Integer): Boolean;
    function ClearBulk: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Check(Value: Boolean; Err: Integer);
    property Usb: TUsb read FUsb write FUsb;
    property Progress: Integer read FProgress;
    property Error: Integer read FError;
    property Abort: Boolean read FAbort write FAbort;
    property JobDone: Boolean read FJobDone;
    property Data: PChar read FData;
    property DataSize: Integer read FDataSize;
    property ScreenW: Integer read FScreenW;
    property ScreenH: Integer read FScreenH;
    property SingleDotRead: Boolean read FSingleDotRead write FSingleDotRead;
  end;


 { TReadForm }
 TReadForm = class(TForm)
    CancelBtn: TButton;
    ReadLbl: TLabel;
    ReadProgressBar: TProgressBar;
    ProgressTimer: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
  private
    { private declarations }
    FOldBytesCount: Int64;
    FThread: TReadThread;
    FUsb: TUsb;
    FReadUnitsStr: UTF8String;
    FReadCaptionStr: UTF8String;
    procedure LanguageChanged;
    procedure SetBytesCountLblCaption;
    function IsOpened: Boolean;
    function IsJobDone: Boolean;
    function GetError: Integer;
  public
    { public declarations }
    class function Execute: Boolean;
    property Opened: Boolean read IsOpened;
    property JobDone: Boolean read IsJobDone;
    property Error: Integer read GetError;
  end;

var
  ReadForm: TReadForm;

implementation

{$R *.lfm}

uses UMain;

const
  ClrSize = 4;

{ TReadException }
constructor TReadException.Create(Err: Integer);
begin
  inherited Create('');
  FError := Err;
end;

{ TReadThread }
constructor TReadThread.Create;
begin
  inherited Create(True);
  FUsb := nil;
  FProgress := 0;
  FError := 0;
  FBulkData := nil;
  FBulkDataSize := 0;
  FBulkBuff := nil;
  FBulkBuffSize := 0;
  FData := nil;
  FDataSize := 0;
  FScreenW := 0;
  FScreenH := 0;
  FAbort := False;
  FJobDone := False;
  FSingleDotRead := False;
end;

destructor TReadThread.Destroy;
var
  TempBulkData: Pointer;
  TempBulkBuff: Pointer;
begin
  TempBulkData := FBulkData;
  TempBulkBuff := FBulkBuff;
  inherited;

  if Assigned(TempBulkData) then
    FreeMem(TempBulkData);
  if Assigned(TempBulkBuff) then
    FreeMem(TempBulkBuff);
end;

procedure TReadThread.Check(Value: Boolean; Err: Integer);
begin
  if not Value then
    raise TReadException.Create(Err);
end;

procedure TReadThread.Execute;
begin
  try
    FJobDone := Read;
  except
    On E: TReadException do
    begin
      FError := E.Error;
    end;
    else
      FError := -1;
  end;
end;

function TReadThread.Read: Boolean;
const
  BytesPerPixel = 3;
  RowsPerBulk = 2;
var
  ProductSpecification: array[0..2] of Word;
  Done: Boolean = False;
  F: Boolean;
  X,Y: Integer;
  Clr: Uint32 = 0;
  Index: Integer;
  ReadDot: Boolean = False;
  ReadDotDone: Boolean = False;
  I,J,N: Integer;
begin
  Result := False;

  Check(FUsb.Opened, -1);
  Check(FUsb.Recover, -1);

  Check(FUsb.GetProductSpecification(@ProductSpecification), -1);

  UGlobal.ProductId      := Integer(ProductSpecification[0]);
  UGlobal.ProductVersion := Integer(ProductSpecification[1]);
  UGlobal.ProductModel   := Integer(ProductSpecification[2]);

  F := (ProductId = 61)
    or (ProductId = 62)
    or (ProductId = 63);
  Check(F, WRONG_DEVICE);

  F := ProductVersion = $0300;
  Check(F, WRONG_VERSION);

  F := ProductModel = $0001;
  Check(F, WRONG_MODEL);

  Check(FUsb.GetScreenWidth(FScreenW), TIMEOUT_ERROR);
  Check(FUsb.GetScreenHeight(FScreenH), TIMEOUT_ERROR);
  Check(FScreenW > 0, DATA_ERROR);
  Check(FScreenH > 0, DATA_ERROR);
  if Assigned(FData) then
    FreeMem(FData);
  FDataSize := ScreenW*ScreenH*ClrSize;
  FData := AllocMem(FDataSize);
  Check(Assigned(FData), DATA_ERROR);

  if not FSingleDotRead then
  begin
    FBulkDataSize := FScreenW * BytesPerPixel * RowsPerBulk;
    FBulkData := GetMem(FBulkDataSize);
    FBulkBuffSize := FBulkDataSize;
    FBulkBuff := GetMem(FBulkBuffSize);
    ClearBulk;
    Check(FUsb.SetBulkSize(FBulkDataSize), TIMEOUT_ERROR);
  end;

  X := 0;
  Y := 0;
  while not Done do
  begin
    Check(not FAbort, USER_ABORT);
    if FSingleDotRead then
    begin
      if not ReadDot then
      begin
        Check(FUsb.ReadScreenDot(X, Y), TIMEOUT_ERROR);
        ReadDot := True;
        Continue;
      end;
      Check(FUsb.IsScreenDotReadingDone(ReadDotDone), TIMEOUT_ERROR);
      if not ReadDotDone then
        Continue;
      ReadDot := False;
      Check(FUsb.GetScreenDot(Clr), TIMEOUT_ERROR);
      Index := ClrSize*(X + Y*ScreenW);
      Move(Clr, FData[Index], ClrSize);
      Inc(X);
      if X = ScreenW then
      begin
        X := 0;
        Inc(Y);
      end;
    end
    else
    begin
      if not ReadDot then
      begin
        Check(FUsb.StartScreenDots(Y), TIMEOUT_ERROR);
        ReadDot := True;
        Continue;
      end;
      Sleep(1);
      Check(FUsb.IsScreenDotsReadingDone(ReadDotDone), TIMEOUT_ERROR);
      if not ReadDotDone then
      begin
        Continue;
      end;
      ReadDot := False;
      Check(GetBulk(FBulkDataSize), TIMEOUT_ERROR);

      N := ScreenW*RowsPerBulk - 1;
      for I := 0 to N do
      begin
        J := I*BytesPerPixel;
        Clr :=
         Byte(FBulkData[J]) or
         Byte(FBulkData[J + 1])*$100 or
         Byte(FBulkData[J + 2])*$10000;
        Index := ClrSize*(Y*ScreenW + I);
        Move(Clr, FData[Index], ClrSize);
      end;

      Inc(Y, RowsPerBulk);
    end;
    Done := Y = ScreenH;
    FProgress := Round(100.0*(X + Y*ScreenW)/(ScreenW*ScreenH));
  end;

  Result := Done;
end;

function TReadThread.GetBulk(Size: Integer): Boolean;
var
  I,J,N: Integer;
  CurrDataSize: Integer;
  ReadBytes: Integer;
begin
  Result := False;

  if Size < 1 then
    Exit;
  if FBulkDataSize < Size then
    Exit;
  if FBulkBuffSize < Size then
    Exit;

  CurrDataSize := Size;
  I := 0;

  Check(FUsb.StartBulk, TIMEOUT_ERROR);
  while I < Size do
  begin
    Check(not FAbort, USER_ABORT);
    N := Size - I;
    if N > CurrDataSize then
      N := CurrDataSize;

    ReadBytes := FUsb.Read(FBulkBuff, N, READ_TIMEOUT);
    if ReadBytes < 0 then
    begin
      Check(FUsb.Error = USB_TIMEOUT_ERROR, FUsb.Error);
      Continue;
    end;

    for J := 0 to ReadBytes - 1 do
    begin
      if J = N then
        Break;
      FBulkData[I] := FBulkBuff[J];
      Inc(I);
    end;
  end;

  Result := True;
end;

function TReadThread.ClearBulk: Boolean;
const
  KeyStr: PChar = '#CLEAR-BULK';
  KeyStrN = 100;
var
  KeyStrLen: Integer;
  BuffSize: Integer;
  I: Integer;
  Index: Integer = 0;
  Ch: Char;
  FoundCnt: Integer = 0;
  LastIndex: Integer = -1;
  Done: Boolean = False;
begin
  KeyStrLen := StrLen(KeyStr);
  BuffSize := KeyStrLen*KeyStrN;
  Check(FUsb.SetBulkSize(BuffSize), TIMEOUT_ERROR);
  Check(FUsb.ClearBulk, TIMEOUT_ERROR);
  while not Done do
  begin
    Sleep(1);
    Check(FUsb.IsCmdDone(Done), TIMEOUT_ERROR);
  end;

  Check(FUsb.StartBulk, TIMEOUT_ERROR);
  Check(GetBulk(BuffSize), TIMEOUT_ERROR);
  for I := 0 to BuffSize - 1 do
  begin
    Ch := FBulkData[I];
    if Ch = KeyStr[Index] then
    begin
      Inc(Index);
      if Index = KeyStrLen then
      begin
        LastIndex := I;
        Index := 0;
        Inc(FoundCnt);
      end;
    end
    else
    begin
      Index := 0;
      FoundCnt := 0;
    end;
  end;

  if (FoundCnt = 0) or (FoundCnt = KeyStrN) then
  begin
    Result := FoundCnt = KeyStrN;
    Exit;
  end;

  BuffSize := KeyStrLen*(KeyStrN - FoundCnt) - (BuffSize - (1 + LastIndex));
  Check(FUsb.SetBulkSize(BuffSize), TIMEOUT_ERROR);
  Check(GetBulk(BuffSize), TIMEOUT_ERROR);
  Result := True;
end;

{ TReadForm }
procedure TReadForm.FormCreate(Sender: TObject);
begin
  FOldBytesCount := 0;
  DoubleBuffered := True;
  LanguageChanged;
  FUsb := TUsb.Create;
  FThread := TReadThread.Create;
  FThread.Usb := FUsb;
  FUsb.Open;
end;

procedure TReadForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not FThread.Finished then
  begin
    FThread.Abort := True;
    FThread.WaitFor;
  end;
  CanClose := True;
end;

procedure TReadForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TReadForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  X, Y: Integer;
  W, H: Integer;
  Clr: Uint32 = 0;
  Index: Integer;
  Data: PChar;
begin
  if JobDone then
  begin
    if not Assigned(DevScreenCachedBmp) then
    begin
      DevScreenCachedBmp := Graphics.TBitmap.Create;
      DevScreenCachedBmp.PixelFormat := TPixelFormat.pf32bit;
    end;
    W := FThread.ScreenW;
    H := FThread.ScreenH;
    Data := FThread.Data;
    DevScreenCachedBmp.SetSize(W, H);
    DevScreenCachedBmp.BeginUpdate(True);
    Index := 0;
    for Y := 0 to H - 1 do
    begin
      for X := 0 to W - 1 do
      begin
         Move(Data[Index], Clr, ClrSize);
         DevScreenCachedBmp.Canvas.Pixels[X, Y] := Clr;
         Inc(Index, ClrSize);
      end;
    end;
    DevScreenCachedBmp.EndUpdate(False);
  end;
end;

procedure TReadForm.FormDestroy(Sender: TObject);
begin
  FThread.Free;
  FUsb.Free;
end;

procedure TReadForm.FormShow(Sender: TObject);
begin
  ReadLbl.AutoSize := False;
  ReadLbl.Left := 8;
  ReadLbl.Width := Width - 2*ReadLbl.Left;
  FThread.Start;
end;

procedure TReadForm.ProgressTimerTimer(Sender: TObject);
var
  Tmp: Int64;
begin
  Tmp := FThread.Progress;
  if ReadProgressBar.Position <> Tmp then
    ReadProgressBar.Position := Integer(Tmp);

  Tmp := FUsb.ReadBytesCount;
  if Tmp <> FOldBytesCount then
  begin
    FOldBytesCount := Tmp;
    SetBytesCountLblCaption;
  end;

  if FThread.Finished then
  begin
    ProgressTimer.Enabled := False;
    Close;
  end;
end;

procedure TReadForm.LanguageChanged;
begin
  CancelBtn.Caption := Gti(1001);
  Caption := Gti(2100);
  FReadCaptionStr := Gti(2112) + '  ';
  FReadUnitsStr := Gti(2111);
  SetBytesCountLblCaption;
end;

procedure TReadForm.SetBytesCountLblCaption;
begin
  ReadLbl.Caption := FReadCaptionStr + IntToStr(FOldBytesCount) + FReadUnitsStr;
end;

function TReadForm.IsOpened: Boolean;
begin
  Result := FUsb.Opened;
end;

function TReadForm.IsJobDone: Boolean;
begin
  Result := FThread.JobDone;
end;

function TReadForm.GetError: Integer;
begin
  Result := FThread.Error;
end;

class function TReadForm.Execute: Boolean;
var
  Frm: TReadForm;
  Err: Integer;
  MsgId: Integer = -1;
  TitleId: Integer = -1;
  Flags: Integer = MB_OK;
begin
  Frm := TReadForm.Create(Application);

  if not Frm.IsOpened then
  begin
    Frm.Free;
    MessageBoxW(MainForm.Handle, PWideChar(WideString(Gti(2102))),
     PWideChar(WideString(Gti(2101))), MB_OK or MB_ICONERROR);
    Result := False;
  end
  else
  begin
    Frm.ShowModal;
    Result := Frm.JobDone;
    Err := Frm.Error;
    Frm.Free;
    if not Result then
    begin
      case Err of
        0,USER_ABORT: ;
        WRONG_MODEL:
        begin
          Flags := Flags or MB_ICONERROR;
          MsgId   := 2110;
          TitleId := 2109;
        end;
        WRONG_VERSION,WRONG_DEVICE:
        begin
          Flags := Flags or MB_ICONERROR;
          MsgId   := 2108;
          TitleId := 2107;
        end;
        else
        begin
          Flags := Flags or MB_ICONERROR;
          MsgId   := 2104;
          TitleId := 2103;
        end;
      end;
      if MsgId > 0 then
        MessageBoxW(MainForm.Handle, PWideChar(WideString(Gti(MsgId))),
         PWideChar(WideString(Gti(TitleId))), Flags);
    end;
  end;
end;

end.

