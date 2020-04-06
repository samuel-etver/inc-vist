unit UGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDevice, UDeviceThread, Windows, IniFiles, UTypes;

const
  //Debug = True;
  DeviceName = 'incvist';
  DeviceLcdW = 240;
  DeviceLcdH = 320;
  DeviceButtonsCount = 12;
  DeviceButtonNames: array[0 .. DeviceButtonsCount - 1] of String = (
    'f1',      'f2',    'f3',
    'measure', 'up',    'plus',
    'left',    'menu',  'right',
    'power',   'down',  'minus');
  MeasureUmin = 0.0;
  MeasureUmax = 3.0;

var
  ApplicationPath: String;
  Device: TDevice;
  DeviceThread: TDeviceThread;
  DeviceKeys: Word;
  ArchiveFileName: String;
  ArchiveSize: Integer;
  MemFillFormShown: Boolean;
  MemFillFormTop: Integer;
  MemFillFormLeft: Integer;
  MemFillFormHex: Boolean;
  MemFillFormValue: Byte;
  MemFillFormValueType: Integer;
  MemFillFormFromAddress: Dword;
  MemFillFormToAddress: Dword;
  MemJumpFormShown: Boolean;
  MemJumpFormLeft: Integer;
  MemJumpFormTop: Integer;
  MemJumpFormHex: Boolean;
  MemJumpFormAddress: Dword;
  MemSearchFormShown: Boolean;
  MemSearchFormOk: Boolean;
  MemSearchFormLeft: Integer;
  MemSearchFormTop: Integer;
  MemSearchFormHex: Boolean;
  MemSearchFormValue: Byte;
  MemSearchFormValueType: Integer;
  MemSearchFormFromAddress: Dword;
  MemSearchFormToAddress: Dword;
  ArchiveMemFormSearchIndex: Int64;
  SupplySourceVoltage: Float32;
  MeasureT: Float32;
  MeasureNoise: Float32;
  MeasureUavr:  Float32;
  MeasureUamp:  Float32;
  MeasureUpeak: Float32;
  DebugBool: Boolean;
  DebugInt32: Int32;
  DebugFloat: Float32;

function StrToAddress(ATxt: String; AHex: Boolean): Int64;
function AddressToStr(AAddress: Int64; AHex: Boolean): String;
procedure Load;
procedure Save;
procedure ReadVars;
procedure WriteVars;

implementation

uses Forms;

procedure DoInit;
const
  BuffSize = 4096;
var
  Buff: PChar;
begin
  ApplicationPath := '';
  Buff := AllocMem(BuffSize);
  if Buff <> nil then begin
    if GetModuleFileName(GetModuleHandle(nil), Buff, BuffSize) <> 0 then begin
      ApplicationPath := ExtractFilePath(Buff);
      if ApplicationPath[Length(ApplicationPath)] <> '\' then
        ApplicationPath := ApplicationPath + '\';
    end;
    FreeMem(Buff);
  end;
  ArchiveFileName := DeviceName + '.archive';

  Device := TDevice.Create;
  ArchiveSize := Device.FlashSize;
  DeviceThread := TDeviceThread.Create;
  DeviceKeys := 0;
  MemFillFormShown := False;
  MemJumpFormShown := False;
  MemSearchFormShown := False;
  MemSearchFormOk := False;
  ArchiveMemFormSearchIndex := -1;
  DebugBool := False;
  DebugInt32 := 0;
  DebugFloat := 0.0;
  MeasureUavr  := MeasureUmin;
  MeasureUamp  := MeasureUmin;
  MeasureUpeak := MeasureUmin;
end;

procedure DoFinal;
begin
  DeviceThread.Free;
  Device.Free;
end;

function StrToAddress(ATxt: String; AHex: Boolean): Int64;
begin
  ATxt := Trim(ATxt);
  if AHex
    then Result := StrToInt('$' + ATxt)
    else Result := StrToInt(ATxt);
end;

function AddressToStr(AAddress: Int64; AHex: Boolean): String;
var
  Txt: String;
  I, N: Integer;
begin
  if AHex
    then Txt := IntToHex(AAddress, 16)
    else Txt := IntToStr(AAddress);
  N := Length(Txt) - 1;
  I := 1;
  while I <= N do begin
    if Txt[I] <> '0' then Break;
    Inc(I);
  end;

  if I = N + 1
    then Result := Txt[N + 1]
    else Result := Copy(Txt, I, N + 2 - I);
end;

procedure Load;
var
  IniFile:     TIniFile;
  SectionName: String;

procedure BeginSection(NewSectionName: String);
begin
  SectionName := NewSectionName;
end;

procedure EndSection;
begin
end;

function ReadStr(Key: String; DefVal: String = ''): String;
begin
  Result := IniFile.ReadString(SectionName, Key, DefVal);
end;

function ReadBool(Key: String; DefVal: Boolean): Boolean;
var
  Txt: String;
begin
  Txt := ReadStr(Key);
  if AnsiCompareText(Txt, 'true') = 0
    then Result := True
    else if AnsiCompareText(Txt, 'false') = 0
      then Result := False
      else Result := DefVal;
end;

function ReadInt(Key: String; DefVal: Integer): Integer;
begin
  Result := StrToIntDef(ReadStr(Key), DefVal);
end;

function ReadFloat32(Key: String; DefVal: Float32): Float32;
begin
  try
    Result := Float32(StrToFloat(ReadStr(Key)));
  except
    Result := DefVal;
  end;
end;

begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  BeginSection('Measure');
  SupplySourceVoltage := ReadFloat32('SupplySourceVoltage', 3.3);
  MeasureT := ReadFloat32('MeasureT', 0.0);
  if MeasureT < 0.001 then
    MeasureT := 0.001
  else if MeasureT > 1.000 then
    MeasureT := 1.000;
  MeasureNoise := ReadFloat32('MeasureNoise', 0.0);
  MeasureUavr := ReadFloat32('MeasureUavr', MeasureUavr);
  if MeasureUavr < MeasureUmin then
    MeasureUavr := MeasureUmin
  else if MeasureUavr > MeasureUmax then
    MeasureUavr := MeasureUmax;
  MeasureUamp := ReadFloat32('MeasureUamp', MeasureUamp);
  if MeasureUamp < MeasureUmin then
    MeasureUamp := MeasureUmin
  else if MeasureUamp > MeasureUmax then
    MeasureUamp := MeasureUmax;
  MeasureUpeak := ReadFloat32('MeasureUpeak', MeasureUpeak);
  if MeasureUpeak < MeasureUmin then
    MeasureUpeak := MeasureUmin
  else if MeasureUpeak > MeasureUmax then
    MeasureUPeak := MeasureUmax;
  EndSection;
  IniFile.Free;
end;

procedure Save;
var
  IniFile:     TIniFile;
  SectionName: String;

procedure BeginSection(NewSectionName: String);
begin
  SectionName := NewSectionName;
end;

procedure EndSection;
begin
end;

procedure WriteStr(Key: String; Value: String);
begin
  IniFile.WriteString(SectionName, Key, Value);
end;

procedure WriteBool(Key: String; Value: Boolean);
var
  Txt: String;
begin
  if Value
    then Txt := 'True'
    else Txt := 'False';
  WriteStr(Key, Txt);
end;

procedure WriteInt(Key: String; Value: Integer);
begin
  WriteStr(Key, IntToStr(Value));
end;

procedure WriteFloat32(Key: String; Value: Float32);
begin
  WriteStr(Key, FloatToStr(Value));
end;

begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  BeginSection('Measure');
  WriteFloat32('SupplySourceVoltage', SupplySourceVoltage);
  WriteFloat32('MeasureT', MeasureT);
  WriteFloat32('MeasureNoise', MeasureNoise);
  WriteFloat32('MeasureUavr', MeasureUavr);
  WriteFloat32('MeasureUamp', MeasureUamp);
  WriteFloat32('MeasureUpeak', MeasureUpeak);
  EndSection;
  IniFile.Free;
end;

procedure ReadVars;
begin
end;

procedure WriteVars;
begin
end;

initialization
  DoInit;

finalization
  DoFinal;

end.

