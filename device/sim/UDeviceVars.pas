unit UDeviceVars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTypes;

const
  VAR_TYPE_BOOL     = 0;
  VAR_TYPE_INT8     = 1;
  VAR_TYPE_BYTE     = 2;
  VAR_TYPE_INT16    = 3;
  VAR_TYPE_WORD     = 4;
  VAR_TYPE_INT32    = 5;
  VAR_TYPE_DWORD    = 6;
  VAR_TYPE_FLOAT32  = 7;
  VAR_TYPE_FLOAT64  = 8;
  VAR_TYPE_STR      = 9;
  VAR_TYPE_EMPTY    = $FFFF;

type
  TDeviceVarException = class(Exception)
  end;

  TDeviceVar = class
  private
    FVarName: String;
    FVarType: Integer;
    FReadOnly: Boolean;
    FBoolValue: Boolean;
    FByteValue: Byte;
    FWordValue: Word;
    FDwordValue: Dword;
    FInt8Value: Int8;
    FInt16Value: Int16;
    FInt32Value: Int32;
    FFloat32Value: Single;
    FFloat64Value: Double;
    FStrValue: String;
    function GetEmpty: Boolean;
    procedure SetEmpty;
    function GetBoolValue: Boolean;
    procedure SetBoolValue(NewValue: Boolean);
    function GetByteValue: Byte;
    procedure SetByteValue(NewValue: Byte);
    function GetInt8Value: Int8;
    procedure SetInt8Value(NewValue: Int8);
    function GetWordValue: Word;
    procedure SetWordValue(NewValue: Word);
    function GetInt16Value: Int16;
    procedure SetInt16Value(NewValue: Int16);
    function GetDwordValue: Dword;
    procedure SetDwordValue(NewValue: Dword);
    function GetInt32Value: Int32;
    procedure SetInt32Value(NewValue: Int32);
    function GetFloat32Value: Float32;
    procedure SetFloat32Value(NewValue: Float32);
    function GetFloat64Value: Float64;
    procedure SetFloat64Value(NewValue: Float64);
    function GetStrValue: String;
    procedure SetStrValue(NewValue: String);
    procedure CheckType(TypeToCheck: Integer);
  public
    constructor Create(NewVarName: String = '';
     NewVarType: Integer = VAR_TYPE_EMPTY; NewReadOnly: Boolean = False);
    function Read(Buff: PChar): Boolean;
    function Write(Buff: PChar): Integer;
    function ToStr: String;
    property VarName: String read FVarName write FVarName;
    property VarType: Integer read FVarType write FVarType;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property IsEmpty: Boolean read GetEmpty;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property ByteValue: Byte read GetByteValue write SetByteValue;
    property Int8Value: Int8 read GetInt8Value write SetInt8Value;
    property WordValue: Word read GetWordValue write SetWordValue;
    property Int16Value: Int16 read GetInt16Value write SetInt16Value;
    property DwordValue: Dword read GetDwordValue write SetDwordValue;
    property Int32Value: Int32 read GetInt32Value write SetInt32Value;
    property Float32Value: Float32 read GetFloat32Value write SetFloat32Value;
    property Float64Value: Float64 read GetFloat64Value write SetFloat64Value;
    property StrValue: String read GetStrValue write SetStrValue;
  end;

  TDeviceVarList = class
  private
    FList: TList;
    function GetDeviceVar(aName: String): TDeviceVar;
    function GetItem(aIndex: Integer): TDeviceVar;
    function GetCount: Integer;
    function Find(aName: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aVar: TDeviceVar);
    procedure Clear;
    procedure Sort;
    property DeviceVar[aName: String]: TDeviceVar read GetDeviceVar;
    property Items[aIndex: Integer]: TDeviceVar read GetItem;
    property Count: Integer read GetCount;
  end;

implementation

type
  TPtrValue = record
    case Byte of
    0:  (PVoidValue: Pointer);
    1:  (PBoolValue: PBoolean);
    2:  (PCharValue: PChar);
    3:  (PByteValue: PByte);
    4:  (PInt8Value: PInt8);
    5:  (PWordValue: PWord);
    6:  (PInt16Value: PInt16);
    7:  (PDwordValue: PDword);
    8:  (PInt32Value: PInt32);
    9:  (PFloat32Value: PSingle);
    10: (PFloat64Value: PDouble);
  end;

constructor TDeviceVar.Create(NewVarName: String; NewVarType: Integer;
 NewReadOnly: Boolean);
begin
  FVarName  := NewVarName;
  FVarType  := NewVarType;
  FReadOnly := NewReadOnly;
  FBoolValue := True;
  FByteValue := 0;
  FInt8Value := 0;
  FWordValue := 0;
  FInt16Value := 0;
  FDwordValue := 0;
  FInt32Value := 0;
  FFloat32Value := 0;
  FFloat64Value := 0;
  FStrValue := '';
end;

function TDeviceVar.ToStr: String;
begin
  case VarType of
    VAR_TYPE_BOOL:
      if BoolValue
        then Result := 'True'
        else Result := 'False';
    VAR_TYPE_BYTE:    Result := IntToStr(ByteValue);
    VAR_TYPE_INT8:    Result := IntToStr(Int8Value);
    VAR_TYPE_WORD:    Result := IntToStr(WordValue);
    VAR_TYPE_INT16:   Result := IntToStr(Int16Value);
    VAR_TYPE_DWORD:   Result := IntToStr(DwordValue);
    VAR_TYPE_INT32:   Result := IntToStr(Int32Value);
    VAR_TYPE_FLOAT32: Result := FloatToStr(Float32Value);
    else Result := '';
  end;
end;

function TDeviceVar.GetEmpty: Boolean;
begin
  Result := FVarType = VAR_TYPE_EMPTY;
end;

procedure TDeviceVar.SetEmpty;
begin
  FVarType := VAR_TYPE_EMPTY;
end;

function TDeviceVar.GetBoolValue: Boolean;
begin
  CheckType(VAR_TYPE_BOOL);
  Result := FBoolValue;
end;

procedure TDeviceVar.SetBoolValue(NewValue: Boolean);
begin
  FBoolValue := NewValue;
  FVarType   := VAR_TYPE_BOOL;
end;

function TDeviceVar.GetByteValue: Byte;
begin
  CheckType(VAR_TYPE_BYTE);
  Result := FByteValue;
end;

procedure TDeviceVar.SetByteValue(NewValue: Byte);
begin
  FByteValue := NewValue;
  FVarType   := VAR_TYPE_BYTE;
end;

function TDeviceVar.GetWordValue: Word;
begin
  CheckType(VAR_TYPE_WORD);
  Result := FWordValue;
end;

procedure TDeviceVar.SetWordValue(NewValue: Word);
begin
  FWordValue := NewValue;
  FVarType   := VAR_TYPE_WORD;
end;

function TDeviceVar.GetDwordValue: Dword;
begin
  CheckType(VAR_TYPE_DWORD);
  Result := FDwordValue;
end;

procedure TDeviceVar.SetDwordValue(NewValue: Dword);
begin
  FDwordValue := NewValue;
  FVarType    := VAR_TYPE_DWORD;
end;

function TDeviceVar.GetInt8Value: Int8;
begin
  CheckType(VAR_TYPE_INT8);
  Result := FInt8Value;
end;

procedure TDeviceVar.SetInt8Value(NewValue: Int8);
begin
  FInt8Value := NewValue;
  FVarType   := VAR_TYPE_INT8;
end;

function TDeviceVar.GetInt16Value: Int16;
begin
  CheckType(VAR_TYPE_INT16);
  Result := FInt16Value;
end;

procedure TDeviceVar.SetInt16Value(NewValue: Int16);
begin
  FInt16Value := NewValue;
  FVarType    := VAR_TYPE_INT16;
end;

function TDeviceVar.GetInt32Value: Int32;
begin
  CheckType(VAR_TYPE_INT32);
  Result := FInt32Value;
end;

procedure TDeviceVar.SetInt32Value(NewValue: Int32);
begin
  FInt32Value := NewValue;
  FVarType    := VAR_TYPE_INT32;
end;

function TDeviceVar.GetFloat32Value: Float32;
begin
  CheckType(VAR_TYPE_FLOAT32);
  Result := FFloat32Value;
end;

procedure TDeviceVar.SetFloat32Value(NewValue: Single);
begin
  FFloat32Value := NewValue;
  FVarType      := VAR_TYPE_FLOAT32;
end;

function TDeviceVar.GetFloat64Value: Float64;
begin
  CheckType(VAR_TYPE_FLOAT64);
  Result := FFloat64Value;
end;

procedure TDeviceVar.SetFloat64Value(NewValue: Double);
begin
  FFloat64Value := NewValue;
  FVarType      := VAR_TYPE_FLOAT64;
end;

function TDeviceVar.GetStrValue: String;
begin
  CheckType(VAR_TYPE_STR);
  Result := FStrValue;
end;

procedure TDeviceVar.SetStrValue(NewValue: String);
begin
  FStrValue := NewValue;
  FVarType  := VAR_TYPE_STR;
end;

procedure TDeviceVar.CheckType(TypeToCheck: Integer);
begin
  if TypeToCheck <> FVarType then
    raise TDeviceVarException.Create(FVarName + ' type error');
end;

function TDeviceVar.Read(Buff: PChar): Boolean;
var
  Pv:  TPtrValue;
  Tp:  Word;
  I:   Integer;
  N:   Integer;
  Txt: String;
begin
  if not Assigned(Buff) then
  begin
    SetEmpty;
    Result := False;
    Exit;
  end;

  Result := True;
  Pv.PVoidValue := Buff;
  Tp := Pv.PWordValue^;

  Pv.PVoidValue := Buff + 2;
  case Tp of
    VAR_TYPE_BOOL:    BoolValue  := Pv.PBoolValue^;
    VAR_TYPE_INT8:    Int8Value  := Pv.PInt8Value^;
    VAR_TYPE_BYTE:    ByteValue  := Pv.PByteValue^;
    VAR_TYPE_INT16:   Int16Value := Pv.PInt16Value^;
    VAR_TYPE_WORD:    WordValue  := Pv.PWordValue^;
    VAR_TYPE_INT32:   Int32Value := Pv.PInt32Value^;
    VAR_TYPE_DWORD:   DwordValue := Pv.PDwordValue^;
    VAR_TYPE_FLOAT32: Float32Value := Pv.PFloat32Value^;
    VAR_TYPE_FLOAT64: Float64Value := Pv.PFloat64Value^;
    VAR_TYPE_STR:
    begin
      N := Pv.PByteValue^;
      Txt := '';
      for I := 1 to N do
        Txt := Txt + Pv.PCharValue[I];
      StrValue := Txt;
    end;
    else
    begin
      SetEmpty;
      Result := False;
    end;
  end;
end;

function TDeviceVar.Write(Buff: PChar): Integer;
var
  Pt: PWord;
  Pv: TPtrValue;
  I:  Integer;
  N:  Integer;
begin
  Pt  := Pointer(Buff);
  Pt^ := FVarType;
  Result := 2;

  Pv.PVoidValue := Buff + 2;

  case FVarType of
    VAR_TYPE_BOOL:
    begin
      Pv.PBoolValue^ := FBoolValue; Inc(Result, 1);
    end;

    VAR_TYPE_INT8:
    begin
      Pv.PInt8Value^ := FInt8Value; Inc(Result);
    end;

    VAR_TYPE_BYTE:
    begin
      Pv.PByteValue^ := FByteValue; Inc(Result);
    end;

    VAR_TYPE_INT16:
    begin
      Pv.PInt16Value^ := FInt16Value; Inc(Result, 2);
    end;

    VAR_TYPE_WORD:
    begin
      Pv.PWordValue^ := FWordValue; Inc(Result, 2);
    end;

    VAR_TYPE_INT32:
    begin
      Pv.PInt32Value^ := FInt32Value; Inc(Result, 4);
    end;

    VAR_TYPE_DWORD:
    begin
      Pv.PDwordValue^ := FDwordValue; Inc(Result, 4);
    end;

    VAR_TYPE_FLOAT32:
    begin
      Pv.PFloat32Value^ := FFloat32Value; Inc(Result, 4);
    end;

    VAR_TYPE_FLOAT64:
    begin
      Pv.PFloat64Value^ := FFloat64Value; Inc(Result, 8);
    end;

    VAR_TYPE_STR:
    begin
      Pv.PByteValue^ := Length(FStrValue);
      N := Pv.PByteValue^;
      Inc(Result);
      for I := 1 to N do
        Pv.PCharValue[I] := FStrValue[I];
      Inc(Result, N);
    end;
  end;
end;

{ TDeviceVarList }
constructor TDeviceVarList.Create;
begin
  FList := TList.Create;
end;

destructor TDeviceVarList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TDeviceVarList.GetDeviceVar(aName: String): TDeviceVar;
var
  I: Integer;
begin
  Result := nil;
  I := Find(aName);
  if I >= 0 then
    Result := TDeviceVar(FList.Items[I]);
end;

function TDeviceVarList.GetItem(aIndex: Integer): TDeviceVar;
begin
  Result := TDeviceVar(FList.Items[aIndex]);
end;

function TDeviceVarList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDeviceVarList.Find(aName: String): Integer;
var
  I, N: Integer;
begin
  Result := -1;
  N := Count - 1;
  for I := 0 to N do begin
    if TDeviceVar(FList.Items[I]).VarName = aName then begin
      Result := I; Break;
    end;
  end;
end;

procedure TDeviceVarList.Add(aVar: TDeviceVar);
begin
  if Find(aVar.VarName) < 0 then FList.Add(aVar);
end;

procedure TDeviceVarList.Clear;
var
  I: Integer;
begin
  I := Count - 1;
  while I >= 0 do begin
    TDeviceVar(FList.Items[I]).Free;
    Dec(I);
  end;
  FList.Clear;
end;

procedure TDeviceVarList.Sort;
var
  I, J, NI, NJ: Integer;
  ItemI, ItemJ: TDeviceVar;
begin
  NI := Count - 2;
  NJ := Count - 1;
  for I := 0 to NI do
    for J := I + 1 to NJ do begin
      ItemI := Items[I];
      ItemJ := Items[J];
      if ItemI.VarName > ItemJ.VarName then
        FList.Exchange(I, J);
    end;
end;

end.

