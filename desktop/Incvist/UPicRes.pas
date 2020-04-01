unit UPicRes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, lazutf8classes;

type
  TPicResItem = class
  private
    FKey: String;
    FPic: TPicture;
  public
    constructor Create(Key: String; Pic: TPicture);
    property Key: String read FKey write FKey;
    property Pic: TPicture read FPic write FPic;
  end;

  TPicRes = class
  private
    FList: TList;
    FSorted: Boolean;
    function GetCount: Integer;
    function GetPic(Key: String): TPicture;
    function IndexOf(Key: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Sort;
    procedure Add(Key: String; FileName: UTF8String);
    procedure Add(Key: String; Stream: TStream);

    property Count: Integer read GetCount;
    property Pics[Index: String]: TPicture read GetPic;
    property Sorted: Boolean read FSorted;
  end;

implementation

constructor TPicResItem.Create(Key: String; Pic: TPicture);
begin
  FKey := Key;
  FPic := Pic;
end;

constructor TPicRes.Create;
begin
  FList := TList.Create;
  FSorted := False;
end;

destructor TPicRes.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TPicRes.Clear;
var
  I: Integer;
  Item: TPicResItem;
begin
  for I := Count - 1 downto 0 do
  begin
    Item := TPicResItem(FList[I]);
    if Assigned(Item.Pic) then
      Item.Pic.Free;
    Item.Free;
  end;
  FList.Clear;
end;

function TPicRes.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TPicRes.Add(Key: String; FileName: UTF8String);
var
  Stream: TFileStreamUTF8;
begin
  try
    Stream := TFileStreamUTF8.Create(FileName, fmOpenRead);
    Stream.Position := 0;
    Add(Key, Stream);
  except
  end;
end;

procedure TPicRes.Add(Key: String; Stream: TStream);
var
  I: Integer;
  Pic: TPicture;
begin
  FSorted := False;
  Pic := TPicture.Create;
  try
    Pic.LoadFromStream(Stream);
    I := IndexOf(Key);
    if I < 0
      then FList.Add(TPicResItem.Create(Key, Pic))
      else TPicResItem(FList[I]).Pic := Pic;
  except
    Pic.Free;
  end;
end;

function TPicRes.GetPic(Key: String): TPicture;
var
  I: Integer;
begin
  I := IndexOf(Key);
  if I < 0
    then Result := nil
    else Result := TPicResItem(FList[I]).Pic;
end;

procedure TPicRes.Sort;
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
      if CompareStr(TPicResItem(FList[I]).Key,
         TPicResItem(FList[J]).Key) > 0 then
        FList.Exchange(I, J);
    end;
  end;
  FSorted := True;
end;

function TPicRes.IndexOf(Key: String): Integer;
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
      if TPicResItem(FList[I]).Key = Key then
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
      N := CompareStr(TPicResItem(FList[I]).Key, Key);
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

end.

