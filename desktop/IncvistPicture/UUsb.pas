unit UUsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, ULibUsb, UGlobal;

const
  USB_NO_ERROR                = 0;
  USB_TIMEOUT_ERROR           = 1;
  USB_UNKNOWN_ERROR           = 2;
  USB_EVENT_CREATE_ERROR      = 3;
  USB_NOT_OPENED_ERROR        = 4;

  IOCTL_TIMEOUT               = 1000;
  READ_TIMEOUT                = 500;
  WRITE_TIMEOUT               = 500;

  VENDOR_GET_PRODUCT_SPECIFICATION      = 1;
  VENDOR_SET_BULK_SIZE                  = 2;
  VENDOR_START_BULK                     = 3;
  VENDOR_GET_FLASH_CHANGED              = 10;
  VENDOR_CLEAR_FLASH_CHANGED            = 11;
  VENDOR_READ_FLASH                     = 12;
  VENDOR_IS_CMD_DONE                    = 13;
  VENDOR_RECOVER                        = 50;
  VENDOR_GET_SCREEN_WIDTH               = 100;
  VENDOR_GET_SCREEN_HEIGHT              = 101;
  VENDOR_READ_SCREEN_DOT                = 102;
  VENDOR_GET_SCREEN_DOT                 = 104;
  VENDOR_START_SCREEN_DOTS              = 110;
  VENDOR_CLEAR_BULK                     = 120;

  PipeIds: array[0..1] of Byte = (
    $81, $02
  );


type
  TUsb = class
  private
    FHandle: TUsbDevHandle;
    FReadBytesCount: Int64;
    FWriteBytesCount: Int64;
    FError: Integer;
    FBuff: PChar;
    FBuffSize: Integer;
    function IsLoaded: Boolean;
    function IsOpened: Boolean;
    procedure CopyBuffTo(Dst: PChar; Index: Integer; N: Integer);
    function GetBuffU8(Index: Integer): Uint8;
    function GetBuffI8(Index: Integer): Int8;
    function GetBuffU16(Index: Integer): UInt16;
    function GetBuffI16(Index: Integer): Int16;
    function GetBuffU32(Index: Integer): Uint32;
    function GetBuffI32(Index: Integer): Int32;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: Boolean;
    procedure Close;
    function ReadEp(Ep: Byte; Buff: PChar; N: Integer;
     Timeout: Integer): Integer;
    function WriteEp(Ep: Byte; Buff: PChar; N: Integer;
     Timeout: Integer): Integer;
    function Read(Buff: PChar; N: Integer; Timeout: Integer): Integer;
    function Write(Buff: PChar; N: Integer; Timeout: Integer): Integer;
    function ReadCmdData(Buff: PChar; N: Integer; Timeout: Integer): Integer;
    function WriteCmdData(Buff: PChar; N: Integer; Timeout: Integer): Integer;
    function ExecuteCmd(Request: Integer; Value: Integer; Index: Integer;
     Timeout: Integer): Integer;
    function ExecuteCmdGetData(Request: Integer; Value: Integer;
      Index: Integer; Buff: PChar; BuffSize: Integer; Timeout: Integer): Integer;
    function ExecuteCmdSetData(Request: Integer; Value: Integer;
      Index: Integer; Buff: PChar; BuffSize: Integer; Timeout: Integer): Integer;
    function GetProductSpecification(Buff: PWord): Boolean;
    function ClearFlashChanged: Boolean;
    function GetFlashChanged(var Changed: Boolean): Boolean;
    function ReadFlash(Addr: Dword): Boolean;
    function IsFlashReadingDone(var Done: Boolean): Boolean;
    function Recover: Boolean;
    function StartBulk: Boolean;
    function SetBulkSize(Size: Integer): Boolean;
    function GetScreenWidth(var Width: Integer): Boolean;
    function GetScreenHeight(var Height: Integer): Boolean;
    function ReadScreenDot(X, Y: Integer): Boolean;
    function IsScreenDotReadingDone(var Done: Boolean): Boolean;
    function GetScreenDot(var Clr: Uint32): Boolean;
    function StartScreenDots(Y: Integer): Boolean;
    function IsScreenDotsReadingDone(var Done: Boolean): Boolean;
    function IsCmdDone(var Done: Boolean): Boolean;
    function ClearBulk: Boolean;

    property Loaded: Boolean read IsLoaded;
    property Opened: Boolean read IsOpened;
    property Handle: TUsbDevHandle read FHandle;
    property ReadBytesCount: Int64 read FReadBytesCount write FReadBytesCount;
    property WriteBytesCount: Int64 read FWriteBytesCount write FWriteBytesCount;
    property Error: Integer read FError write FError;
  end;

implementation

var
  LibHandle: THandle;

{ TUsb }
constructor TUsb.Create;
begin
  FHandle := 0;
  FReadBytesCount := 0;
  FWriteBytesCount := 0;
  FError := USB_NO_ERROR;
  FBuffSize := 256;
  GetMem(FBuff, FBuffSize);
  ULibUsb.Load;
end;

destructor TUsb.Destroy;
begin
  Close;
  FreeMem(FBuff);
  ULibUsb.Unload;
  inherited;
end;

function TUsb.IsLoaded: Boolean;
begin
  Result := ULibUsb.UsbIsLoaded;
end;

function TUsb.IsOpened: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TUsb.CopyBuffTo(Dst: PChar; Index: Integer; N: Integer);
var
  I: Integer;
begin
  for I := 0 to N - 1 do
    Dst[I] := FBuff[Index + I];
end;

function TUsb.GetBuffU8(Index: Integer): Uint8;
begin
  Result := Byte(FBuff[Index]);
end;

function TUsb.GetBuffI8(Index: Integer): Int8;
begin
  Result := Int8(GetBuffU8(Index));
end;

function TUsb.GetBuffU16(Index: Integer): Uint16;
begin
  CopyBuffTo(Pointer(@Result), Index, 2);
end;

function TUsb.GetbuffI16(Index: Integer): Int16;
begin
  CopyBuffTo(Pointer(@Result), Index, 2);
end;

function TUsb.GetBuffU32(Index: Integer): Uint32;
begin
  CopyBuffTo(Pointer(@Result), Index, 4);
end;

function TUsb.GetBuffI32(Index: Integer): Int32;
begin
  CopyBuffTo(Pointer(@Result), Index, 4);
end;

function TUsb.Open: Boolean;
var
  PBus: PUsbBus;
  PDev: PUsbDevice;
  H: TUsbDevHandle = 0;
label
  OnError;
begin
  Result := False;
  FReadBytesCount := 0;
  FWriteBytesCount := 0;

  if not Loaded then Exit;

  Close();

  UsbFindBusses;
  UsbFindDevices;

  PBus := ULibUsb.UsbGetBusses();

  while Assigned(PBus) do
  begin
    PDev := PBus^.devices;
    while Assigned(PDev) do
    begin
      if (PDev^.descriptor.idVendor = $0471) and
         (PDev^.descriptor.idProduct = $0002) then
      begin
        H := ULibUsb.UsbOpen(PDev);
        if H = 0 then
          goto OnError;
        if ULibUsb.UsbClaimInterface(H, 0) < 0 then
          goto OnError;
        FHandle := H;
        Result := True;
        Exit;
      end;
      PDev := PDev^.next;
    end;
    PBus := PBus^.next;
  end;

  OnError:
  begin
    if H <> 0 then
      ULibUsb.UsbClose(H);
    FHandle := 0;
  end;
end;

procedure TUsb.Close;
begin
  if not Loaded then Exit;
  if not Opened then Exit;

  ULibUsb.UsbClose(FHandle);
  FHandle := 0;
end;

function TUsb.ReadEp(Ep: Byte; Buff: PChar; N: Integer;
 Timeout: Integer): Integer;
begin
  FError := USB_NO_ERROR;
  Result := ULibUsb.UsbBulkRead(FHandle, Ep, Buff, N, Timeout);
  if Result < 0
    then FError := USB_TIMEOUT_ERROR
    else Inc(FReadBytesCount, Result);
end;

function TUsb.WriteEp(Ep: Byte; Buff: PChar; N: Integer;
 Timeout: Integer): Integer;
begin
  FError := USB_NO_ERROR;
  Result := ULibUsb.UsbBulkWrite(FHandle, Ep, Buff, N, Timeout);
  if Result < 0
    then FError := USB_TIMEOUT_ERROR
    else Inc(FWriteBytesCount, Result);
end;

function TUsb.Read(Buff: PChar; N: Integer; Timeout: Integer): Integer;
begin
  if not Opened then
  begin
    FError := USB_NOT_OPENED_ERROR;
    Result := -1;
  end
  else
  begin
    Result := ReadEp(PipeIds[0], Buff, N, Timeout);
  end;
end;

function TUsb.Write(Buff: PChar; N: Integer; Timeout: Integer): Integer;
begin
  if not Opened then
  begin
    FError := USB_NOT_OPENED_ERROR;
    Result := -1;
  end
  else
  begin
    Result := WriteEp(PipeIds[1], Buff, N, Timeout);
  end;
end;

function TUsb.ReadCmdData(Buff: PChar; N: Integer; Timeout: Integer): Integer;
begin
  if not Opened then
  begin
    FError := USB_NOT_OPENED_ERROR;
    Result := -1;
  end
  else
  begin
    Result := ReadEp(PipeIds[0], Buff, N, Timeout);
  end;
end;

function TUsb.WriteCmdData(Buff: PChar; N: Integer; Timeout: Integer): Integer;
begin
  if not Opened then
  begin
    FError := USB_NOT_OPENED_ERROR;
    Result := -1;
  end
  else
  begin
    Result := WriteEp(PipeIds[1], Buff, N, Timeout);
  end;
end;

function TUsb.ExecuteCmd(Request: Integer; Value: Integer; Index: Integer;
 Timeout: Integer): Integer;
begin
  FError := USB_NO_ERROR;
  Result := ULibUsb.UsbControlMsg(FHandle, $40, Request, Value,
   Index, nil, 0, Timeout);
  if Result < 0 then
    FError := USB_TIMEOUT_ERROR;
end;

function TUsb.ExecuteCmdGetData(Request: Integer; Value: Integer;
  Index: Integer; Buff: PChar; BuffSize: Integer; Timeout: Integer): Integer;
begin
  FError := USB_NO_ERROR;

  Result := ULibUsb.UsbControlMsg(FHandle,
   $40 or USB_ENDPOINT_IN, request, value, index,
   Buff, BuffSize, Timeout);

  if Result < 0
    then FError := USB_TIMEOUT_ERROR
    else Inc(FReadBytesCount, Result);
end;

function TUsb.ExecuteCmdSetData(Request: Integer; Value: Integer;
  Index: Integer; Buff: PChar; BuffSize: Integer; Timeout: Integer): Integer;
begin
  FError := USB_NO_ERROR;

  Result := ULibUsb.UsbControlMsg(FHandle,
   $40 or USB_ENDPOINT_OUT, Request, Value, Index,
   Buff, BuffSize, Timeout);

  if Result < 0
    then FError := USB_TIMEOUT_ERROR
    else Inc(FWriteBytesCount);
end;

function TUsb.GetProductSpecification(Buff: PWord): Boolean;
var
  N: Integer;
begin
  N := ExecuteCmdGetData(VENDOR_GET_PRODUCT_SPECIFICATION, 0, 0,
   PChar(Buff), 6, IOCTL_TIMEOUT);
  Result := N = 6;
end;

function TUsb.ClearFlashChanged: Boolean;
begin
  Result := ExecuteCmd(VENDOR_CLEAR_FLASH_CHANGED, 0, 0, IOCTL_TIMEOUT) >= 0;
end;

function TUsb.GetFlashChanged(var Changed: Boolean): Boolean;
begin
  Result := ExecuteCmdGetData(VENDOR_GET_FLASH_CHANGED, 0, 0,
   FBuff, 1, IOCTL_TIMEOUT) = 1;
  if Result then
    Changed := GetBuffU8(0) <> 0;
end;

function TUsb.ReadFlash(Addr: Dword): Boolean;
begin
  Result := ExecuteCmd(VENDOR_READ_FLASH, Addr and $FFFF,
   Integer(Dword(Addr) div $10000), IOCTL_TIMEOUT) = 0;
end;

function TUsb.IsFlashReadingDone(var Done: Boolean): Boolean;
begin
  Result := IsCmdDone(Done);
end;

function TUsb.Recover: Boolean;
const
  RecoverStr = '#RECOVER-SQ';
var
  Attempts: Integer;
  Index: Integer = 0;
begin
  Result := False;

  for Attempts := 0 to 255 do
  begin
    if ExecuteCmdGetData(VENDOR_RECOVER, 0, Index,
       FBuff, 1, IOCTL_TIMEOUT) <> 1 then
      Exit;

    if RecoverStr[Index + 1] = FBuff[0]
      then Inc(Index)
      else Index := 0;

    if Index = Length(RecoverStr) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TUsb.StartBulk: Boolean;
begin
  Result := ExecuteCmd(VENDOR_START_BULK, 0, 0, IOCTL_TIMEOUT) = 0;
end;

function TUsb.SetBulkSize(Size: Integer): Boolean;
begin
  Result := ExecuteCmd(VENDOR_SET_BULK_SIZE, Size and $FFFF, 0,
   IOCTL_TIMEOUT) >= 0;
end;

function TUsb.GetScreenWidth(var Width: Integer): Boolean;
begin
  Result := ExecuteCmdGetData(VENDOR_GET_SCREEN_WIDTH, 0, 0, FBuff, 2,
   IOCTL_TIMEOUT) = 2;
  if Result then
    Width := GetBuffI16(0);
end;

function TUsb.GetScreenHeight(var Height: Integer): Boolean;
begin
  Result := ExecuteCmdGetData(VENDOR_GET_SCREEN_HEIGHT, 0, 0, FBuff, 2,
    IOCTL_TIMEOUT) = 2;
  if Result then
    Height := GetBuffI16(0);
end;

function TUsb.ReadScreenDot(X, Y: Integer): Boolean;
begin
  Result := ExecuteCmd(VENDOR_READ_SCREEN_DOT, x, y, IOCTL_TIMEOUT) >= 0;
end;

function TUsb.IsScreenDotReadingDone(var Done: Boolean): Boolean;
begin
  Result := IsCmdDone(Done);
end;

function TUsb.GetScreenDot(var Clr: Uint32): Boolean;
begin
  Result := ExecuteCmdGetData(VENDOR_GET_SCREEN_DOT, 0, 0, FBuff, 4,
   IOCTL_TIMEOUT) = 4;
  if Result then
    Clr := GetBuffU32(0);
end;

function TUsb.StartScreenDots(Y: Integer): Boolean;
begin
  Result := ExecuteCmd(VENDOR_START_SCREEN_DOTS, y, 0, IOCTL_TIMEOUT) >= 0;
end;

function TUsb.IsScreenDotsReadingDone(var Done: Boolean): Boolean;
begin
  Result := IsCmdDone(Done);
end;

function TUsb.IsCmdDone(var Done: Boolean): Boolean;
begin
  Result := ExecuteCmdGetData(VENDOR_IS_CMD_DONE, 0, 0, FBuff, 1,
   IOCTL_TIMEOUT) = 1;
  if Result then
    Done := GetBuffU8(0) <> 0;
end;

function TUsb.ClearBulk: Boolean;
begin
  Result := ExecuteCmd(VENDOR_CLEAR_BULK, 0, 0, IOCTL_TIMEOUT) >= 0;
end;

procedure Init;
begin
  LibHandle := 0;
end;

procedure Fin;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
end;

initialization

  Init;

finalization

  Fin;

end.

