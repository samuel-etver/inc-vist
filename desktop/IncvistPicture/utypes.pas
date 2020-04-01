unit UTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Float32 = Single;
  PFloat32 = ^Float32;
  Float64 = Double;
  PFloat64 = ^Float64;

  TUn = packed record
  case Integer of
    0:  (C8:   Char);
    1:  (I8:   Shortint);
    2:  (U8:   Byte);
    3:  (I16:  Int16);
    4:  (U16:  Word);
    5:  (I32:  Int32);
    6:  (U32:  Dword);
    7:  (I64:  Int64);
    8:  (U64:  Qword);
    9:  (F32:  Float32);
    10: (F64:  Float64);
    11: (CA8:  array[0..7] of Char);
    12: (IA8:  array[0..7] of Shortint);
    13: (UA8:  array[0..7] of Byte);
    14: (IA16: array[0..3] of Int16);
    15: (UA16: array[0..3] of Word);
    16: (IA32: array[0..1] of Int32);
    17: (UA32: array[0..1] of Dword);
  end;

  TWBool = class
  private
    FValue: Boolean;
  public
    constructor Create;
    constructor Create(AValue: Boolean);
    class function ToStr(AValue: TWBool): String;
    function ToStr: String;
    class function Parse(Txt: String): Boolean;
    class function Parse(Txt: String; Def: Boolean): Boolean;
    property Value: Boolean read FValue write FValue;
  end;

  TWInt = class
  private
    FValue: Integer;
  public
    constructor Create;
    constructor Create(AValue: Integer);
    class function ToStr(AValue: TWInt): String;
    class function ToStr(AValue: TWInt; var FS: TFormatSettings): String;
    function ToStr: String;
    function ToStr(var FS: TFormatSettings): String;
    class function Parse(Txt: UTF8String): TWInt;
    class function Parse(Txt: UTF8String; Def: Integer): TWInt;
    class function Parse(Txt: UTF8String; var FS: TFormatSettings): TWInt;
    class function Parse(Txt: UTF8String; Def: Integer;
     var FS: TFormatSettings): TWInt;
    property Value: Integer read FValue write FValue;
  end;

  TWFloat32 = class
  private
    FValue: Float32;
  public
    constructor Create;
    constructor Create(AValue: Float32);
    class function ToStr(AValue: TWFloat32): String;
    class function ToStr(AValue: TWFloat32; var FS: TFormatSettings): String;
    class function ToStr(AValue: TWFloat32; Precision, Digits: Integer): String;
    class function ToStr(AValue: TWFloat32; Precision, Digits: Integer;
     var FS: TFormatSettings): String;
    class function ToStr(AValue: TWFloat32; Format: TFloatFormat;
     Precision, Digits: Integer): String;
    class function ToStr(AValue: TWFloat32; Format: TFloatFormat;
     Precision, Digits: Integer; var FS: TFormatSettings): String;
    function ToStr: String;
    function ToStr(var FS: TFormatSettings): String;
    function ToStr(Precision, Digits: Integer): String;
    function ToStr(Precision, Digits: Integer; var FS: TFormatSettings): String;
    function ToStr(Format: TFloatFormat; Precision, Digits: Integer): String;
    function ToStr(Format: TFloatFormat; Precision, Digits: Integer;
     var FS: TFormatSettings): String;
    class function Parse(Txt: UTF8String): TWFloat32;
    class function Parse(Txt: UTF8String; Def: Float32): TWFloat32;
    class function Parse(Txt: UTF8String; var FS: TFormatSettings): TWFloat32;
    class function Parse(Txt: UTF8String; Def: Float32; var FS: TFormatSettings): TWFloat32;
    property Value: Float32 read FValue write FValue;
  end;

  TWDate = class
  private
    FValue: TDate;
  public
    constructor Create;
    constructor Create(AValue: TDate);
    class function ToStr(AValue: TWDate): String;
    class function ToStr(AValue: TWDate; var FS: TFormatSettings): String;
    class function Parse(Txt: UTF8String): TWDate;
    class function Parse(Txt: UTF8String; var FS: TFormatSettings): TWDate;
    function ToStr: String;
    function ToStr(var FS: TFormatSettings): String;
    property Value: TDate read FValue write FValue;
  end;

  TWTime = class
  private
    FValue: TTime;
  public
    constructor Create;
    constructor Create(AValue: TTime);
    class function ToStr(AValue: TWTime): String;
    class function ToStr(AValue: TWTime; var FS: TFormatSettings): String;
    function ToStr: String;
    function ToStr(var FS: TFormatSettings): String;
    class function Parse(Txt: UTF8String): TWTime;
    class function Parse(Txt: UTF8String; var FS: TFormatSettings): TWTime;
    property Value: TTime read FValue write FValue;
  end;

  TWDateTime = class
  private
    FValue: TDateTime;
  public
    constructor Create;
    constructor Create(AValue: TDateTime);
    property Value: TDateTime read FValue write FValue;
  end;

  TWString = class
  private
    FValue: UTF8String;
  public
    constructor Create;
    constructor Create(AValue: UTF8String);
    property Value: UTF8String read FValue write FValue;
  end;

  function ToStr(AValue: TWBool): String;
  function ToStr(AValue: TWInt): String;
  function ToStr(AValue: TWint; var FS: TFormatSettings): String;
  function ToStr(AValue: TWFloat32): String;
  function ToStr(AValue: TWFloat32; var FS: TFormatSettings): String;
  function ToStr(AValue: TWFloat32; Precision, Digits: Integer): String;
  function ToStr(AValue: TWFloat32; Precision, Digits: Integer;
   var FS: TFormatSettings): String;
  function ToStr(AValue: TWFloat32; Format: TFloatFormat;
   Precision, Digits: Integer): String;
  function ToStr(AValue: TWFloat32; Format: TFloatFormat;
   Precision, Digits: Integer; var FS: TFormatSettings): String;
  function ToStr(AValue: TWDate): String;
  function ToStr(AValue: TWDate; var FS: TFormatSettings): String;
  function ToStr(AValue: TWTime): String;
  function ToStr(AValue: TWTime; var FS: TFormatSettings): String;

implementation

{ TWBool }
constructor TWBool.Create;
begin
  inherited;
  FValue := False;
end;

constructor TWBool.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

class function TWBool.ToStr(AValue: TWBool): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr;
end;

function TWBool.ToStr: String;
begin
  if FValue
    then Result := 'True'
    else Result := 'False';
end;

class function TWBool.Parse(Txt: String): Boolean;
begin
  Txt := UpperCase(Trim(Txt));
  if Txt = 'TRUE' then
    Result := True
  else if Txt = 'FALSE' then
    Result := False
  else raise EConvertError.Create('''' + Txt + ''' is not valid Boolean value');
end;

class function TWBool.Parse(Txt: String; Def: Boolean): Boolean;
begin
  try
    Result := Parse(Txt);
  except
    Result := Def;
  end;
end;

{ TWInt }
constructor TWInt.Create;
begin
  inherited;
  FValue := 0;
end;

constructor TWInt.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

class function TWInt.ToStr(AValue: TWInt): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr;
end;

class function TWInt.ToStr(AValue: TWInt; var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(FS);
end;

function TWInt.ToStr: String;
begin
  Result := IntToStr(FValue);
end;

function TWInt.ToStr(var FS: TFormatSettings): String;
begin
  Result := FloatToStrF(FValue, ffFixed, 15, 0, FS);
end;

class function TWInt.Parse(Txt: UTF8String): TWInt;
begin
  Result := TWInt.Create(StrToInt(Txt));
end;

class function TWInt.Parse(Txt: UTF8String; Def: Integer): TWInt;
begin
  Result := TWInt.Create(StrToIntDef(Txt, Def));
end;

class function TWInt.Parse(Txt: UTF8String; var FS: TFormatSettings): TWInt;
begin
  Result := TWint.Create(Trunc(StrToFloat(Txt, FS)));
end;

class function TWInt.Parse(Txt: UTF8String; Def: Integer;
 var FS: TFormatSettings): TWInt;
begin
  try
    Result := Parse(Txt, FS);
  except
    Result := TWint.Create(Def);
  end;
end;

{ TWFloat32 }
constructor TWFloat32.Create;
begin
  inherited;
  FValue := 0.0;
end;

constructor TWFloat32.Create(AValue: Float32);
begin
  inherited Create;
  FValue := AValue;
end;

class function TWFloat32.ToStr(AValue: TWFloat32): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr;
end;

class function TWFloat32.ToStr(AValue: TWFloat32; var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(FS);
end;

class function TWFloat32.ToStr(AValue: TWFloat32;
 Precision, Digits: Integer): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(Precision, Digits);
end;

class function TWFloat32.ToStr(AValue: TWFloat32; Precision, Digits: Integer;
 var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(Precision, Digits, FS);
end;

class function TWFloat32.ToStr(AValue: TWFloat32; Format: TFloatFormat;
 Precision, Digits: Integer): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(Format, Precision, Digits);
end;

class function TWFloat32.ToStr(AValue: TWFloat32; Format: TFloatFormat;
 Precision, Digits: Integer; var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := AValue.ToStr(Format, Precision, Digits, FS);
end;

function TWFloat32.ToStr: String;
begin
  Result := FloatToStr(FValue);
end;

function TWFloat32.ToStr(var FS: TFormatSettings): String;
begin
  Result := FloatToStr(FValue, FS);
end;

function TWFloat32.ToStr(Precision, Digits: Integer): String;
begin
  Result := ToStr(ffFixed, Precision, Digits);
end;

function TWFloat32.ToStr(Precision, Digits: Integer; var FS: TFormatSettings): String;
begin
  Result := ToStr(ffFixed, Precision, Digits, FS);
end;

function TWFloat32.ToStr(Format: TFloatFormat; Precision, Digits: Integer): String;
begin
  Result := FloatToStrF(FValue, Format, Precision, Digits);
end;

function TWFloat32.ToStr(Format: TFloatFormat; Precision, Digits: Integer;
 var FS: TFormatSettings): String;
begin
  Result := FloatToStrF(FValue, Format, Precision, Digits, FS);
end;

class function TWFloat32.Parse(Txt: UTF8String): TWFloat32;
begin
  Result := TWFloat32.Create(StrToFloat(Txt));
end;

class function TWFloat32.Parse(Txt: UTF8String; Def: Float32): TWFloat32;
begin
  try
    Result := Parse(Txt);
  except
    Result := TWFloat32.Create(Def);
  end;
end;

class function TWFloat32.Parse(Txt: UTF8String; var FS: TFormatSettings): TWFloat32;
begin
  Result := TWFloat32.Create(StrToFloat(Txt, FS));
end;

class function TWFloat32.Parse(Txt: UTF8String; Def: Float32;
 var FS: TFormatSettings): TWFloat32;
begin
  try
    Result := Parse(Txt, FS);
  except
    Result := TWFloat32.Create(Def);
  end;
end;

{ TWDate }
constructor TWDate.Create;
begin
  inherited;
  FValue := Date;
end;

constructor TWDate.Create(AValue: TDate);
begin
  inherited Create;
  FValue := AValue;
end;

class function TWDate.ToStr(AValue: TWDate): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := DateToStr(AValue.Value);
end;

class function TWDate.ToStr(AValue: TWDate; var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := DateToStr(AValue.Value, FS);
end;

function TWDate.ToStr: String;
begin
  Result := TWDate.ToStr(Self);
end;

function TWDate.ToStr(var FS: TFormatSettings): String;
begin
  Result := TWDate.ToStr(Self, FS);
end;

class function TWDate.Parse(Txt: UTF8String): TWDate;
begin
  Result := TWDate.Create(StrToDate(Txt));
end;

class function TWDate.Parse(Txt: UTF8String; var FS: TFormatSettings): TWDate;
begin
  Result := TWDate.Create(StrToDate(Txt, FS));
end;

{ TWTime }
constructor TWTime.Create;
begin
  inherited;
  FValue := Time;
end;

constructor TWTime.Create(AValue: TTime);
begin
  inherited Create;
  FValue := AValue;
end;

class function TWTime.ToStr(AValue: TWTime): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := TimeToStr(AValue.Value);
end;

class function TWTime.ToStr(AValue: TWTime; var FS: TFormatSettings): String;
begin
  if not Assigned(AValue)
    then Result := ''
    else Result := TimeToStr(AValue.Value, FS);
end;

function TWTime.ToStr: String;
begin
  Result := ToStr(Self);
end;

function TWTime.ToStr(var FS: TFormatSettings): String;
begin
  Result := ToStr(Self, FS);
end;

class function TWTime.Parse(Txt: UTF8String): TWTime;
begin
  Result := TWTime.Create(StrToTime(Txt));
end;

class function TWTime.Parse(Txt: UTF8String; var FS: TFormatSettings): TWTime;
begin
  Result := TWTime.Create(StrToTime(Txt, FS));
end;

{ TWDateTime }
constructor TWDateTime.Create;
begin
  inherited;
  FValue := Now;
end;

constructor TWDateTime.Create(AValue: TDateTime);
begin
  inherited Create;
  FValue := AValue;
end;

{ TWString }
constructor TWString.Create;
begin
  inherited;
  FValue := '';
end;

constructor TWString.Create(AValue: UTF8String);
begin
  inherited Create;
  FValue := AValue;
end;

function ToStr(AValue: TWBool): String;
begin
  Result := TWBool.ToStr(AValue);
end;

function ToStr(AValue: TWInt): String;
begin
  Result := TWInt.ToStr(AValue);
end;

function ToStr(AValue: TWInt; var FS: TFormatSettings): String;
begin
  Result := TWInt.ToStr(AValue, FS);
end;

function ToStr(AValue: TWFloat32): String;
begin
  Result := TWFloat32.ToStr(AValue);
end;

function ToStr(AValue: TWFloat32; var FS: TFormatSettings): String;
begin
  Result := TWFloat32.ToStr(AValue, FS);
end;

function ToStr(AValue: TWFloat32; Precision, Digits: Integer): String;
begin
  Result := TWFloat32.ToStr(AValue, Precision, Digits);
end;

function ToStr(AValue: TWFloat32; Precision, Digits: Integer;
 var FS: TFormatSettings): String;
begin
  Result := TWFloat32.ToStr(AValue, Precision, Digits, FS);
end;

function ToStr(AValue: TWFloat32; Format: TFloatFormat;
 Precision, Digits: Integer): String;
begin
  Result := TWFloat32.ToStr(AValue, Format, Precision, Digits);
end;

function ToStr(AValue: TWFloat32; Format: TFloatFormat;
 Precision, Digits: Integer; var FS: TFormatSettings): String;
begin
  Result := TWFloat32.ToStr(AValue, Format, Precision, Digits, FS);
end;

function ToStr(AValue: TWDate): String;
begin
  Result := TWDate.ToStr(AValue);
end;

function ToStr(AValue: TWDate; var FS: TFormatSettings): String;
begin
  Result := TWDate.ToStr(AValue, FS);
end;

function ToStr(AValue: TWTime): String;
begin
  Result := TWTime.ToStr(AValue);
end;

function ToStr(AValue: TWTime; var FS: TFormatSettings): String;
begin
  Result := TWTime.ToStr(AValue, FS);
end;

end.

