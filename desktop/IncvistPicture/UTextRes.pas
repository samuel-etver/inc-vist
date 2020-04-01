unit UTextRes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, lazutf8classes;

type
  TTextResItem = class
  private
    FKey: String;
    FValue: String;
  public
    constructor Create(Key: String; Value: String);

    property Key: String read FKey write FKey;
    property Value: String read FValue write FValue;
  end;


  TTextRes = class
  private
    FList: TList;
    FSorted: Boolean;
    function GetCount: Integer;
    function GetValue(const Key: String): String;
    function IndexOf(const Key: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromFile(FileName: UTF8String);
    procedure LoadFromStream(Stream: TStream);
    procedure Add(Key: String; Val: String);
    procedure Sort;
    property Count: Integer read GetCount;
    property Sorted: Boolean read FSorted;
    property Values[const Key: String]: String read GetValue;
  end;

implementation

constructor TTextResItem.Create(Key: String; Value: String);
begin
  FKey := Key;
  FValue := Value;
end;

constructor TTextRes.Create;
begin
  FList := TList.Create;
  FSorted := False;
end;

destructor TTextRes.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TTextRes.Clear;
var
  I,N: Integer;
begin
  N := FList.Count - 1;
  for I := 0 to N do
    TTextResItem(FList[I]).Free;
  FList.Clear;
end;

procedure TTextRes.LoadFromFile(FileName: UTF8String);
var
  Stream: TFileStreamUTF8;
begin
  Stream := nil;
  try
    Stream := TFileStreamUTF8.Create(FileName, fmOpenRead);
    LoadFromStream(Stream);
  except
  end;

  if Assigned(Stream) then Stream.Free;
end;

procedure TTextRes.LoadFromStream(Stream: TStream);
var
  CachedCh: Cardinal;
  Space: Cardinal;
  LeftSep: Cardinal;
  RightSep: Cardinal;
  Tab: Cardinal;
  Cr: Cardinal;
  Lf: Cardinal;
  Slash: Cardinal;
  Ch: Cardinal;
  Eof: Boolean;
  Err: Boolean;
  Key: String;
  Val: String;

  function ReadChar: Cardinal; forward;
  procedure ResetChar; forward;

  function ToUni(S: String): Cardinal;
  var
    ChLen: Integer;
  begin
    Result := UTF8CharacterToUnicode(PChar(S), ChLen);
  end;

  function ToUtf8(U: Cardinal): String;
  begin
    Result := UnicodeToUtf8(U);
  end;

  procedure SkipSpaces;
  begin
    if Err then
      Exit;

    while not Eof do
    begin
      if ReadChar in [Space, Tab, Cr, Lf]
        then ResetChar
        else Break;
    end;
  end;

  function ReadKey: String;
  label
    OnError;
  begin
    Result := '';
    if Eof or Err then
      Exit;
    if ReadChar <> LeftSep then
      goto OnError;
    ResetChar;

    while True do
    begin
      Ch := ReadChar;
      if Eof then
        goto OnError;
      ResetChar;
      if Ch = RightSep then
        Exit;
      Result := Result + ToUtf8(Ch);
    end;

    OnError:
    begin
      Err := True;
    end;
  end;

  function ReadVal: String;
  label
    OnError;
  begin
    Result := '';
    if Eof or Err then
      Exit;
    if ReadChar <> LeftSep then
      goto OnError;
    ResetChar;

    while True do
    begin
      Ch := ReadChar;
      if Eof then
        goto OnError;
      ResetChar;
      if Ch = RightSep then
        Exit;
      if Ch = Slash then
      begin
        Ch := ReadChar;
        if Eof then
          goto OnError;
        ResetChar;
      end;
      Result := Result + ToUtf8(Ch);
    end;

    OnError:
    begin
      Err := True;
    end;
  end;

  function ReadChar: Cardinal;
  begin
    if Eof then
    begin
      Result := 0;
      Exit;
    end;

    if CachedCh = 0 then
    begin
      if Stream.Read(CachedCh, 2) <> 2 then
      begin
        CachedCh := 0;
        Eof := True;
      end;
    end;
    Result := CachedCh;
  end;

  procedure ResetChar;
  begin
    CachedCh := 0;
  end;

begin
  Space := ToUni(' ');
  LeftSep := ToUni('<');
  RightSep := ToUni('>');
  Tab := ToUni(Chr(9));
  Cr := ToUni(Chr(13));
  Lf := ToUni(Chr(10));
  Slash := ToUni('\');
  Err := False;
  Eof := False;

  ResetChar;
  if ReadChar = $FEFF then
    ResetChar;
  Key := '';
  Val := '';
  while True do
  begin
    SkipSpaces;
    Key := ReadKey;
    SkipSpaces;
    Val := ReadVal;
    if Eof then Exit;
    if Err then Exit;
    Add(Key, Val);
  end;

  Sort;
end;

procedure TTextRes.Add(Key: String; Val: String);
var
  I: Integer;
begin
  FSorted := False;
  I := IndexOf(Key);
  if I < 0
    then FList.Add(TTextResItem.Create(Key, Val))
    else TTextResItem(FList[I]).Value := Val;
end;

function TTextRes.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TTextRes.Sort;
var
  I: Integer;
  J: Integer;
  N: Integer;
begin
  N := Count - 1;
  for I := 0 to N - 1 do
  begin
    for J := I + 1 to N do
    begin
      if CompareStr(TTextResItem(FList[I]).Key,
         TTextResItem(FList[J]).Key) > 0 then
        FList.Exchange(I, J);
    end;
  end;
  FSorted := True;
end;

function TTextRes.IndexOf(const Key: String): Integer;
var
  I: Integer;
  N: Integer;
  Lo: Integer;
  Hi: Integer;
begin
  Result := -1;

  if not FSorted then
  begin
    N := Count - 1;
    for I := 0 to N do
    begin
      if TTextResItem(FList[I]).Key = Key then
      begin
        Result := I;
        Break;
      end;
    end;
  end
  else
  begin
    Lo := 0;
    Hi := Count - 1;
    while Lo <= Hi do
    begin
      I := (Lo + Hi) div 2;
      N := CompareStr(TTextResItem(FList[I]).Key, Key);
      if N < 0
        then Hi := I - 1
        else if N > 0
          then Lo := I + 1
          else
          begin
            Result := I;
            Break;
          end;
    end;
  end;
end;

function TTextRes.GetValue(const Key: String): String;
var
  I: Integer;
begin
  I := IndexOf(Key);
  if I < 0
    then Result := ''
    else Result := TTextResItem(FList[I]).Value;
end;

end.

