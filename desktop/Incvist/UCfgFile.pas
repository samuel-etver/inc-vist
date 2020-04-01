unit UCfgFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTypes, LazUtf8, LazUTF8Classes;

type
  { TCfgFileItem }
  TCfgFileItem = class(TObject)
  private
    FKey: UTF8String;
    FValue: UTF8String;
  public
    constructor Create;
    constructor Create(AKey: UTF8String; AValue: UTF8String);
    property Key: UTF8String read FKey write FKey;
    property Value: UTF8String read FValue write FValue;
  end;

 { TCfgFile }
 TCfgFile = class(TObject)
  private
    FList: TList;
    FCountMax: Integer;
  protected
    function GetCount: Integer;
    procedure SetCountMax(NewValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(FileName: UTF8String): Boolean;
    function Load(Stream: TStream): Boolean;
    function Save(FileName: UTF8String): Boolean;
    function Save(Stream: TStream): Boolean;
    procedure Clear;
    function IndexOf(Key: UTF8String): Integer;
    procedure Write(Key: UTF8String; Val: UTF8String);
    procedure Write(Key: UTF8String; Val: Boolean);
    procedure Write(Key: UTF8String; Val: Integer);
    procedure Write(Key: UTF8String; Val: Float64);
    function Read(Key: UTF8String; DefVal: UTF8String): UTF8String;
    function Read(Key: UTF8String; DefVal: Boolean): Boolean;
    function Read(Key: UTF8String; DefVal: Integer): Integer;
    function Read(Key: UTF8String; DefVal: Float64): Float64;
    procedure Delete(Key: UTF8String);
    procedure Delete(Index: Integer);
    property CountMax: Integer read FCountMax write SetCountMax;
    property Count: Integer read GetCount;
  end;

implementation

{ TCfgFileItem }
constructor TCfgFileItem.Create;
begin
  FKey := '';
  FValue := '';
end;

constructor TCfgFileItem.Create(AKey: UTF8String; AValue: UTF8String);
begin
  FKey := AKey;
  FValue := AValue;
end;

{ TCfgFile }
constructor TCfgFile.Create;
begin
  FList := TList.Create;
  inherited;
end;

destructor TCfgFile.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TCfgFile.Load(FileName: UTF8String): Boolean;
const
  StreamSizeMax = $10000;
var
  FileStream: TFileStreamUTF8 = nil;
  MemoryStream: TMemoryStream;
  StreamSize: Int64;
begin
  MemoryStream := TMemoryStream.Create;
  try
    FileStream := TFileStreamUTF8.Create(FileName, fmOpenRead);
    StreamSize := FileStream.Size;
    if StreamSize > StreamSizeMax then
      StreamSize := StreamSizeMax;
    MemoryStream.CopyFrom(FileStream, StreamSize);
    MemoryStream.Position := 0;
    Result := Load(MemoryStream);
  except
    Result := False;
  end;
  if Assigned(FileStream) then
    FileStream.Free;
  MemoryStream.Free;
end;

function TCfgFile.Load(Stream: TStream): Boolean;
const
  UTF8SizeMax = 6;
var
  MemoryStream: TMemoryStream;
  N: Int64;
  Mem: PChar;
  MemSize: Int64;
  I: Int64;
  J,JN: Integer;
  Buff: array[0..UTF8SizeMax] of Char;
  Line: UTF8String;
  LineStream: TMemoryStream;
  Key: UTF8String;
  Val: UTF8String;
  Eol: Boolean;
  Pos: PtrInt;
begin
  LineStream := TMemoryStream.Create;

  if Stream is TMemoryStream then
  begin
    MemoryStream := Stream as TMemoryStream;
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    N := Stream.Size - Stream.Position;
    if N > 0 then
      MemoryStream.CopyFrom(Stream, N);
    MemoryStream.Position := 0;
  end;

  Mem := MemoryStream.Memory + MemoryStream.Position;
  MemSize := MemoryStream.Size - MemoryStream.Position;

  I := 0;
  Line := '';
  while I < MemSize do
  begin
    // Copy for safe operation
    N := MemSize - I;
    if N > UTF8SizeMax then
      N := UTF8SizeMax;
    JN := Integer(N);
    J := 0;
    while J < JN do
    begin
      Buff[J] := Mem[I + J];
      Inc(J);
    end;
    while J < UTF8SizeMax do
    begin
      Buff[J] := #0;
      Inc(J);
    end;

    N := UTF8CharacterLength(Buff);
    if N = 0 then
      Break;
    Inc(I, N);
    Eol := False;
    if N = 1 then
    begin
      if Buff[0] in [#$0d, #$0a] then
        Eol := True;
    end;
    if not Eol then
      LineStream.Write(Buff, N);
    if I >= MemSize then
      Eol := True;
    if Eol then
    begin
      N := LineStream.Position;
      LineStream.Position := 0;
      if N = 0 then
        Continue;
      SetString(Line, LineStream.Memory, N);
      Pos := UTF8Pos('=', Line);
      if Pos < 1 then
      begin
        Key := UTF8Trim(Line);
        Val := '';
      end
      else
      begin
        Key := UTF8Trim(UTF8Copy(Line, 1, Pos - 1));
        Val := UTF8Trim(UTF8Copy(Line, Pos + 1, UTF8Length(Line) - Pos));
      end;
      Write(Key, Val);
    end;
  end;

  if Assigned(MemoryStream) and (MemoryStream <> Stream) then
    MemoryStream.Free;
  LineStream.Free;
  Result := True;
end;

function TCfgFile.Save(FileName: UTF8String): Boolean;
var
  FileStream: TFileStreamUTF8 = nil;
  MemoryStream: TMemoryStream;
begin
  Result := False;
  MemoryStream := TMemoryStream.Create;
  try
    if Save(MemoryStream) then
    begin
      MemoryStream.Position := 0;
      FileStream := TFileStreamUTF8.Create(FileName, fmCreate);
      FileStream.CopyFrom(MemoryStream, MemoryStream.Size);
      Result := True;
    end;
  except
  end;

  if Assigned(FileStream) then
    FileStream.Free;
  MemoryStream.Free;
end;

function TCfgFile.Save(Stream: TStream): Boolean;
var
  I,N: Integer;
  Str: UTF8String;
  Item: TCfgFileItem;
  Nl: UTF8String;
begin
  case DefaultTextLineBreakStyle of
    tlbsLF: Nl := #10;
    tlbsCR: Nl := #13;
    else    Nl := #13#10;
  end;

  N := Count - 1;
  for I := 0 to N do
  begin
    Item := TCfgFileItem(FList.Items[I]);
    Str := Item.Key + '=' + Item.Value + Nl;
    Stream.Write(PString(Str)[0], Length(Str));
  end;
  Result := True;
end;

procedure TCfgFile.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TCfgFileItem(FList.Items[I]).Free;
  FList.Clear;
end;

function TCfgFile.IndexOf(Key: UTF8String): Integer;
var
  I,N: Integer;
begin
  Result := -1;
  N := Count - 1;
  for I := 0 to N do
  begin
    if UTF8CompareText(TCfgFileItem(FList.Items[I]).Key, Key) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TCfgFile.Write(Key: UTF8String; Val: UTF8String);
var
  Index: Integer;
  Item: TCfgFileItem;
begin
  Index := IndexOf(Key);
  if Index >= 0 then
  begin
    Item := TCfgFileItem(FList.Items[Index]);
    Item.Value := Val;
  end
  else
  begin
    Item := TCfgFileItem.Create(Key, Val);
    FList.Add(Item);
  end;
end;

procedure TCfgFile.Write(Key: UTF8String; Val: Boolean);
begin
  Write(Key, BoolToStr(Val, True));
end;

procedure TCfgFile.Write(Key: UTF8String; Val: Integer);
begin
  Write(Key, IntToStr(Val));
end;

procedure TCfgFile.Write(Key: UTF8String; Val: Float64);
begin
  Write(Key, FloatToStr(Val));
end;

function TCfgFile.Read(Key: UTF8String; DefVal: UTF8String): UTF8String;
var
  Index: Integer;
begin
  Index := IndexOf(Key);
  if Index < 0
    then Result := DefVal
    else Result := TCfgFileItem(FList.Items[Index]).Value;
end;

function TCfgFile.Read(Key: UTF8String; DefVal: Boolean): Boolean;
begin
  try
    Result := StrToBool(Read(Key, ''));
  except
    Result := DefVal;
  end;
end;

function TCfgFile.Read(Key: UTF8String; DefVal: Integer): Integer;
var
  Tmp: Float64;
begin
  try
    Tmp := StrToFloat(Read(Key, ''));
    if Tmp > MaxInt
      then Result := DefVal
      else if Tmp < -MaxInt
        then Result := DefVal
        else Result := Trunc(Tmp);
  except
    Result := DefVal;
  end;
end;

function TCfgfile.Read(Key: UTF8String; DefVal: Float64): Float64;
begin
  try
    Result := StrToFloat(Read(Key, ''));
  except
    Result := DefVal;
  end;
end;

procedure TCfgFile.Delete(Key: UTF8String);
var
  Done: Boolean = False;
  Index: Integer;
begin
  while not Done do
  begin
    Index := IndexOf(Key);
    if Index >= 0
     then Delete(Index)
     else Done := True;
  end;
end;

procedure TCfgFile.Delete(Index: Integer);
begin
  TCfgFileItem(FList.Items[Index]).Free;
  FList.Delete(Index);
end;

function TCfgFile.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TCfgFile.SetCountMax(NewValue: Integer);
var
  I,N: Integer;
begin
  if NewValue < FCountMax then
  begin
    if NewValue < 0
      then N := 0
      else N := NewValue;
    for I := Count - 1 downto N do
    begin
      TCfgFileItem(FList.Items[I]).Free;
      FList.Delete(I);
    end;
  end;

  FCountMax := NewValue;
end;

end.

