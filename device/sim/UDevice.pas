unit UDevice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Graphics, UDeviceVars;

type
  TDevice = class
  private
    FKeys: Word;
    FVarList: TDeviceVarList;

    function GetPixel(X, Y: Integer): TColor;
    function IsDeviceOn: Boolean;
    function GetVar(DeviceVar: TDeviceVar): Boolean;
    function SetVar(DeviceVar: TDeviceVar): Boolean;
    procedure CreateVars;
    procedure FreeVars;
    function GetDeviceVar(VarName: String): TDeviceVar;

    function GetFlashByte(aIndex: Integer): Byte;
    procedure SetFlashByte(aIndex: Integer; aValue: Byte);
    function GetFlashSize: Integer;
  protected
    procedure ReadVars;
    procedure WriteVars;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;

    property Pixels[X, Y: Integer]: TColor read GetPixel;
    property Keys: Word write FKeys;
    property DeviceOn: Boolean read IsDeviceOn;
    property Vars[VarName: String]: TDeviceVar read GetDeviceVar;
    property VarList: TDeviceVarList read FVarList;

    property FlashBytes[Index: Integer]: Byte read GetFlashByte write SetFlashByte;
    property FlashSize: Integer read GetFlashSize;
  end;

  procedure LoadDeviceLib;
  function IsDeviceLibLoaded: Boolean;
  procedure FreeDeviceLib;

implementation

uses
  UGlobal;

type
  Int32 = LongInt;
  TDll_LcdGetPixel = function(X, Y: Int32): Dword; cdecl;
  TDll_Execute = procedure; cdecl;
  TDll_KeybrdSetKeys = procedure(Keys: Word); cdecl;
  TDll_SystemIsOn = function: Word; cdecl;
  TDll_GetVar = function(VarName: PChar): Pointer; cdecl;
  TDll_SetVar = function(VarName, VarValue: PChar): Boolean; cdecl;
  TDll_GetFlashByte = function(aIndex: Dword): Byte; cdecl;
  TDll_SetFlashByte = procedure(aIndex: Dword; aData: Byte); cdecl;
  TDll_GetFlashSize = function: Dword; cdecl;

var
  DeviceLibHandle: THandle;
  Dll_LcdGetPixel: TDll_LcdGetPixel;
  Dll_Execute: TDll_Execute;
  Dll_KeybrdSetKeys: TDll_KeybrdSetKeys;
  Dll_SystemIsOn: TDll_SystemIsOn;
  Dll_GetVar: TDll_GetVar;
  Dll_SetVar: TDll_SetVar;
  Dll_GetFlashByte: TDll_GetFlashByte;
  Dll_SetFlashByte: TDll_SetFlashByte;
  Dll_GetFlashSize: TDll_GetFlashSize;

procedure CreateArchiveMem; forward;
procedure FreeArchiveMem; forward;

constructor TDevice.Create;
begin
  FKeys := 0;
  if not IsDeviceLibLoaded then
    LoadDeviceLib;
  CreateVars;
end;

destructor TDevice.Destroy;
begin
  FreeVars;
  inherited;
  FreeDeviceLib;
end;

procedure TDevice.CreateVars;

procedure Add(VarName: String; VarType: Integer; ReadOnly: Boolean = False);
begin
  FVarList.Add(TDeviceVar.Create(VarName, VarType, ReadOnly));
end;

begin
  FVarList := TDeviceVarList.Create;
  Add('SupplySourceVoltage', VAR_TYPE_FLOAT32);
  Add('DebugBool', VAR_TYPE_BOOL, True);
  Add('DebugInt32', VAR_TYPE_INT32, True);
  Add('DebugFloat', VAR_TYPE_FLOAT32, True);
  Add('MenuLevel', VAR_TYPE_BYTE, True);
  Add('MeasureT_pd', VAR_TYPE_FLOAT32);
  Add('MeasureNoise_pd', VAR_TYPE_FLOAT32);
  Add('MeasureUavr', VAR_TYPE_FLOAT32);
  Add('MeasureUamp', VAR_TYPE_FLOAT32);
  Add('MeasureUpeak', VAR_TYPE_FLOAT32);
  FVarList.Sort;
end;

procedure TDevice.FreeVars;
begin
  FVarList.Free;
end;

function TDevice.GetDeviceVar(VarName: String): TDeviceVar;
begin
  Result := FVarList.DeviceVar[VarName];
end;

procedure TDevice.Execute;
var
  F: Boolean;
begin
  if not IsDeviceLibLoaded then Exit;

  F := IsDeviceOn;

  if F then
    WriteVars;
  Dll_KeybrdSetKeys(FKeys);
  Dll_Execute();
  if F then
    ReadVars;
end;

function TDevice.GetPixel(X, Y: Integer): TColor;
var
  R: Integer;
  G: Integer;
  B: Integer;
  V: Integer;
begin
  Result := clBlack;
  if IsDeviceLibLoaded then
  begin
    V := Dll_LcdGetPixel(X, Y);
    B := (V and $001F);
    G := (V and $07E0) shr 5;
    R := (V and $F800) shr 11;
    Result := Rgb((R*256) div $20,
                  (G*256) div $40,
                  (B*256) div $20);
  end;
end;

function TDevice.IsDeviceOn: Boolean;
begin
  Result := False;
  if IsDeviceLibLoaded then
    Result := Dll_SystemIsOn() <> 0;
end;

function TDevice.GetVar(DeviceVar: TDeviceVar): Boolean;
begin
  Result := False;
  if IsDeviceLibLoaded then
    Result := DeviceVar.Read(Dll_GetVar(PChar(DeviceVar.VarName)));
end;

function TDevice.SetVar(DeviceVar: TDeviceVar): Boolean;
var
  Buff: array[0..255] of Char;
begin
  Result := False;
  if IsDeviceLibLoaded then
  begin
    DeviceVar.Write(Buff);
    Result := Dll_SetVar(PChar(DeviceVar.VarName), Buff);
  end;
end;

function TDevice.GetFlashByte(aIndex: Integer): Byte;
begin
  Result := 0;
  if IsDeviceLibLoaded then
    Result := Dll_GetFlashByte(aIndex);
end;

procedure TDevice.SetFlashByte(aIndex: Integer; aValue: Byte);
begin
  if IsDeviceLibLoaded then
    Dll_SetFlashByte(aIndex, aValue);
end;

function TDevice.GetFlashSize: Integer;
begin
  Result := 0;
  if IsDeviceLibLoaded then
    Result := Dll_GetFlashSize();
end;

procedure DoInit;
begin
  DeviceLibHandle := 0;
end;

procedure DoFinal;
begin
end;

procedure LoadDeviceLib;
const
  BuffSize = 1024;
var
  Buff: array[0..BuffSize - 1] of Char;
  N: Dword;
  FilePath: String;

function GetProc(FuncName: PChar): TFarProc;
begin
  Result := GetProcAddress(DeviceLibHandle, FuncName);
end;

begin
  N := GetModuleFileName(GetModuleHandle(nil), Buff, BuffSize);
  if (N = 0) or (N = BuffSize) then Exit;
  FilePath := ExtractFilePath(Buff);
  if Length(FilePath) = 0 then Exit;
  if FilePath[Length(FilePath)] <> '\' then
    FilePath := FilePath + '\';
  DeviceLibHandle := LoadLibrary(PChar(FilePath + DeviceName + '.dll'));
  if DeviceLibHandle = 0 then Exit;
  Dll_LcdGetPixel   := TDll_LcdGetPixel(GetProc('Dll_LcdGetPixel'));
  Dll_Execute       := TDll_Execute(GetProc('Dll_Execute'));
  Dll_KeybrdSetKeys := TDll_KeybrdSetKeys(GetProc('Dll_KeybrdSetKeys'));
  Dll_SystemIsOn    := TDll_SystemIsOn(GetProc('Dll_SystemIsOn'));
  Dll_GetVar        := TDll_GetVar(GetProc('Dll_GetVar'));
  Dll_SetVar        := TDll_SetVar(GetProc('Dll_SetVar'));
  Dll_GetFlashByte  := TDll_GetFlashByte(GetProc('Dll_GetFlashByte'));
  Dll_SetFlashByte  := TDll_SetFlashByte(GetProc('Dll_SetFlashByte'));
  Dll_GetFlashSize  := TDll_GetFlashSize(GetProc('Dll_GetFlashSize'));
  if not (
     Assigned(Dll_LcdGetPixel) and
     Assigned(Dll_Execute) and
     Assigned(Dll_KeybrdSetKeys) and
     Assigned(Dll_SystemIsOn) and
     Assigned(Dll_GetVar) and
     Assigned(Dll_SetVar) and
     Assigned(Dll_GetFlashByte) and
     Assigned(Dll_SetFlashByte) and
     Assigned(Dll_GetFlashSize)) then
  begin
    FreeLibrary(DeviceLibHandle);
    DeviceLibHandle := 0;
    Exit;
  end;
  CreateArchiveMem;
end;

function IsDeviceLibLoaded: Boolean;
begin
  Result := DeviceLibHandle <> 0;
end;

procedure FreeDeviceLib;
begin
  if IsDeviceLibLoaded then
  begin
    FreeArchiveMem;
    FreeLibrary(DeviceLibHandle);
    DeviceLibHandle := 0;
  end;
end;

procedure CreateArchiveMem;
var
  Stream: TMemoryStream;
  I, N: Integer;
  F: Boolean;
  Buff: PByte;
begin
  N := Device.FlashSize;
  for I := 0 to N - 1 do
    Device.FlashBytes[I] := $FF;

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(ApplicationPath + ArchiveFileName);
    F := True;
  except
    F := False;
  end;

  if F then begin
    if Stream.Size < N then N := Stream.Size;

    Buff := Stream.Memory;
    for I := 0 to N - 1 do begin
      Device.FlashBytes[I] := Buff^;
      Inc(Buff);
    end;
  end;

  Stream.Free;
end;

procedure FreeArchiveMem;
var
  Stream: TMemoryStream;
  I, N: Integer;
  B: Byte;
begin
  Stream := TMemoryStream.Create;
  N := Device.FlashSize - 1;
  for I := 0 to N do begin
    B := Device.FlashBytes[I];
    Stream.Write(B, 1);
  end;

  try
    Stream.SaveToFile(ApplicationPath + ArchiveFileName);
  except
  end;

  Stream.Free;
end;

procedure TDevice.ReadVars;
var
  I:  Integer;
  N:  Integer;
  Dv: TDeviceVar;
begin
  with FVarList do
  begin
    N := Count - 1;
    for I := 0 to N do
    begin
      Dv := Items[I];
      if not GetVar(Dv) then
        raise TDeviceVarException.Create(Dv.VarName + ' does not exist');
    end;
  end;

  Dv := FVarList.DeviceVar['SupplySourceVoltage'];
  if Assigned(Dv) then SupplySourceVoltage := Dv.Float32Value;
  Dv := FVarList.DeviceVar['DebugBool'];
  if Assigned(Dv) then DebugBool := Dv.BoolValue;
  Dv := FVarList.DeviceVar['DebugInt32'];
  if Assigned(Dv) then DebugInt32 := Dv.Int32Value;
  Dv := FVarList.DeviceVar['DebugFloat'];
  if Assigned(Dv) then DebugFloat := Dv.Float32Value;
end;

procedure TDevice.WriteVars;
var
  I: Integer;
  N: Integer;
  Dv: TDeviceVar;
begin
  with FVarList do
  begin
    Dv := DeviceVar['SupplySourceVoltage'];
    if Assigned(Dv) then Dv.Float32Value := SupplySourceVoltage;
    Dv := DeviceVar['MeasureT_pd'];
    if Assigned(Dv) then Dv.Float32Value := MeasureT;
    Dv := DeviceVar['MeasureNoise_pd'];
    if Assigned(Dv) then Dv.Float32Value := MeasureNoise;
    Dv := DeviceVar['MeasureUavr'];
    if Assigned(Dv) then Dv.Float32Value := MeasureUavr;
    Dv := DeviceVar['MeasureUamp'];
    if Assigned(Dv) then Dv.Float32Value := MeasureUamp;
    Dv := DeviceVar['MeasureUpeak'];
    if Assigned(Dv) then Dv.Float32Value := MeasureUpeak;

    N := Count - 1;
    for I := 0 to N do
    begin
      Dv := Items[I];
      if Dv.ReadOnly then
        Continue;
      SetVar(Dv);
    end;
  end;
end;

initialization
  DoInit;

finalization
  DoFinal;

end.

