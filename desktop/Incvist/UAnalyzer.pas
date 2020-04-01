unit UAnalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTypes;

type
  TMemDumpOutOfRangeExeption = class(Exception)
  private
    FOffs: Integer;
    FSize: Integer;
  public
    constructor Create;
    constructor Create(AOffs: Integer; ASize: Integer);
    property Offs: Integer read FOffs write FOffs;
    property Size: Integer read FSize write FSize;
  end;

  TAnalyzer = class(TObject)
  private
  protected
    function GetMemSize: Integer; virtual; abstract;
    function GetBulkSize: Integer; virtual; abstract;
    function AnalyzeImpl: Boolean; virtual; abstract;
    class procedure CheckAccess(Offs: Integer; Size: Integer);
    class function GetPtr(Offs: Integer): Pointer;
    class function GetU8(Offs: Integer): Byte;
    class function GetI8(Offs: Integer): Shortint;
    class function GetU16(Offs: Integer): Word;
    class function GetI16(Offs: Integer): Int16;
    class function GetU32(Offs: Integer): Dword;
    class function GetI32(Offs: Integer): Int32;
    class function GetF32(Offs: Integer): Float32;
    class function GetF64(Offs: Integer): Float64;
    class function GetDate(Offs: Integer): TDate;
    class function GetTime(Offs: Integer): TTime;
  public
    constructor Create;
    function Analyze: Boolean;
    property MemSize: Integer read GetMemSize;
    property BulkSize: Integer read GetBulkSize;
  end;

implementation

uses
  UGlobal;

var
  Century: Word;

{TMemDumpOutOfRangeExeption}
constructor TMemDumpOutOfRangeExeption.Create;
begin
  Create(0, 0);
end;

constructor TMemDumpOutOfRangeExeption.Create(AOffs: Integer; ASize: Integer);
begin
  inherited Create('Access to memory dump is out of range');
  FOffs := AOffs;
  FSize := ASize;
end;

{ TAnalyzer }
constructor TAnalyzer.Create;
begin
  inherited;
end;

function TAnalyzer.Analyze: Boolean;
begin
  Result := False;

  try
    Result := AnalyzeImpl;
  except
  end;
end;

class procedure TAnalyzer.CheckAccess(Offs: Integer; Size: Integer);
var
  F: Boolean;
begin
  F := False;
  if Assigned(MemDump) then
    if (Offs >= 0) and (Offs < MemDumpSize) then
      if (Offs + Size >= 0) and (Offs + Size <= MemDumpSize) then
        F := True;
  if not F then
    raise TMemDumpOutOfRangeExeption.Create(Offs, Size);
end;

class function TAnalyzer.GetPtr(Offs: Integer): Pointer;
begin
  Result := MemDump + Offs;
end;

class function TAnalyzer.GetU8(Offs: Integer): Byte;
begin
  CheckAccess(Offs, 1);
  Result := PByte(GetPtr(Offs))^;
end;

class function TAnalyzer.GetI8(Offs: Integer): Shortint;
begin
  CheckAccess(Offs, 1);
  Result := PShortint(GetPtr(Offs))^;
end;

class function TAnalyzer.GetU16(Offs: Integer): Word;
begin
  CheckAccess(Offs, 2);
  Result := PWord(GetPtr(Offs))^;
end;

class function TAnalyzer.GetI16(Offs: Integer): Int16;
begin
  CheckAccess(Offs, 2);
  Result := PInt16(GetPtr(Offs))^;
end;

class function TAnalyzer.GetU32(Offs: Integer): Dword;
begin
  CheckAccess(Offs, 4);
  Result := PDword(GetPtr(Offs))^;
end;

class function TAnalyzer.GetI32(Offs: Integer): Int32;
begin
  CheckAccess(Offs, 4);
  Result := PInt32(GetPtr(Offs))^;
end;

class function TAnalyzer.GetF32(Offs: Integer): Float32;
begin
  CheckAccess(Offs, 4);
  Result := PFloat32(GetPtr(Offs))^;
end;

class function TAnalyzer.GetF64(Offs: Integer): Float64;
begin
  CheckAccess(Offs, 8);
  Result := PFloat64(GetPtr(Offs))^;
end;

class function TAnalyzer.GetDate(Offs: Integer): TDate;
var
  D,M,Y: Word;
begin
  D := GetU8(Offs);
  M := GetU8(Offs + 1);
  Y := GetU8(Offs + 2);
  Result := EncodeDate(Y + Century, M, D);
end;

class function TAnalyzer.GetTime(Offs: Integer): TTime;
var
  H,M,S: Word;
begin
  H := GetU8(Offs);
  M := GetU8(Offs + 1);
  S := GetU8(Offs + 2);
  Result := EncodeTime(H, M, S, 0);
end;

procedure Init;
var
  D,M,Y: Word;
begin
  DecodeDate(Date, Y, M, D);
  Century := (Y div 100)*100;
end;

initialization

  Init;

end.

