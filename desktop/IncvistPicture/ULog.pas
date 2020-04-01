unit ULog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, lazutf8classes;

type
  TLog = class
  private
  protected
    function GetOpened: Boolean; virtual; abstract;
  public
    procedure Write(Value: String); virtual; abstract;
    procedure Write(Value: Boolean);
    procedure Write(Value: Integer);
    procedure Write(Value: Real);
    procedure Write(Value: Real; Precision, Digits: Integer);
    procedure Nl;
    property Opened: Boolean read GetOpened;
  end;

  TLogFile = class(TLog)
  private
    FFileName: String;
    FStream: TFileStreamUTF8;
    FOpened: Boolean;
  protected
    function GetOpened: Boolean; override;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;
    procedure Open(OpenExisting: Boolean);
    procedure Close;
    procedure Write(Value: String); override;
    procedure Flush;

    property FileName: String read FFileName;
  end;

  TLogConsole = class(TLog)
  private
    FOpened: Boolean;
    FAllocated: Boolean;
    FStream: THandleStream;
  protected
    function GetOpened: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure Write(Value: String); override;
    procedure Flush;
  end;

implementation

{ TLog }
procedure TLog.Write(Value: Boolean);
begin
  if Opened then
    Write(BoolToStr(Value, True));
end;

procedure TLog.Write(Value: Integer);
begin
  if Opened then
    Write(IntToStr(Value));
end;

procedure TLog.Write(Value: Real);
begin
  if Opened then
    Write(FloatToStr(Value));
end;

procedure TLog.Write(Value: Real; Precision, Digits: Integer);
begin
  if Opened then
    Write(FloatToStrF(Value, ffFixed, Precision, Digits));
end;

procedure TLog.Nl;
var
  Txt: String;
begin
  if Opened then
  begin
    case DefaultTextLineBreakStyle of
      tlbsLF: Txt := #10;
      tlbsCR: Txt := #13;
      else    Txt := #13#10;
    end;
    Write(Txt);
  end;
end;

{ TLogFile }
constructor TLogFile.Create(FileName: String);
begin
  inherited Create;
  FFileName := FileName;
  FOpened := False;
  FStream := nil;
end;

destructor TLogFile.Destroy;
begin
  Close;
  inherited;
end;

function TLogFile.GetOpened: Boolean;
begin
  Result := FOpened;
end;

procedure TLogFile.Open(OpenExisting: Boolean);
var
  Mode: Word;
begin
  Close;
  try
    if OpenExisting
      then Mode := fmOpenWrite
      else Mode := fmCreate;
    FStream := TFileStreamUTF8.Create(FFileName, Mode);
    FStream.Seek(0, soEnd);
    FOpened := True;
  except
    if Assigned(FStream) then
    begin
      FStream.Free;
      FStream := nil;
    end;
  end;
end;

procedure TLogFile.Close;
begin
  if FOpened then
  begin
    FOpened := False;
    FStream.Free;
    FStream := nil;
  end;
end;

procedure TLogFile.Write(Value: String);
var
  Len: Integer;
begin
  if FOpened then
  begin
    try
      Len := Length(Value);
      if Len > 0 then
        FStream.Write(PChar(Value)[0], Len);
    except
    end;
  end;
end;

procedure TLogFile.Flush;
begin
  if FOpened then
    FlushFileBuffers(FStream.Handle);
end;

{ TLogConsole }
constructor TLogConsole.Create;
begin
  inherited;
  FOpened := False;
  FAllocated := False;
  FStream := nil;
end;

destructor TLogConsole.Destroy;
begin
  Close;
  inherited;
end;

function TLogConsole.GetOpened: Boolean;
begin
  Result := FOpened;
end;

procedure TLogConsole.Open;
var
  Handle: THandle;
begin
  Close;
  FAllocated := AllocConsole;
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  FStream := THandleStream.Create(Handle);
  FOpened := True;
end;

procedure TLogConsole.Close;
begin
  if FOpened then
  begin
    FOpened := False;
    if FAllocated then
    begin
      FAllocated := False;
      FreeConsole;
    end;
    FStream.Free;
    FStream := nil;
  end;
end;

procedure TLogConsole.Write(Value: String);
var
  Len: Integer;
begin
  if FOpened then
  begin
    try
      Len := Length(Value);
      if Len > 0 then
        FStream.Write(PChar(Value)[0], Len);
    except
    end;
  end;
end;

procedure TLogConsole.Flush;
begin
  if FOpened then
    FlushFileBuffers(FStream.Handle);
end;

end.

