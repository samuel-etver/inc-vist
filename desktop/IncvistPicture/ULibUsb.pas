unit ULibUsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  {
    PATH_MAX from limits.h can't be used on Windows if the dll and
    import libraries are build/used by different compilers
  }
  LIBUSB_PATH_MAX = 512;

  {
    USB spec information

    This is all stuff grabbed from various USB specs and is pretty much
    not subject to change
  }

  {

    Device and/or Interface Class codes
  }
  USB_CLASS_PER_INTERFACE = 0;	// for DeviceClass
  USB_CLASS_AUDIO         = 1;
  USB_CLASS_COMM	  = 2;
  USB_CLASS_HID		  = 3;
  USB_CLASS_PRINTER	  = 7;
  USB_CLASS_MASS_STORAGE  = 8;
  USB_CLASS_HUB		  = 9;
  USB_CLASS_DATA	  = 10;
  USB_CLASS_VENDOR_SPEC	  = $ff;

  {
    Descriptor types
  }
  USB_DT_DEVICE		  = $01;
  USB_DT_CONFIG		  = $02;
  USB_DT_STRING		  = $03;
  USB_DT_INTERFACE	  = $04;
  USB_DT_ENDPOINT	  = $05;

  USB_DT_HID		  = $21;
  USB_DT_REPORT		  = $22;
  USB_DT_PHYSICAL	  = $23;
  USB_DT_HUB		  = $29;

  {
    Descriptor sizes per descriptor type
  }
  USB_DT_DEVICE_SIZE	     = 18;
  USB_DT_CONFIG_SIZE	     = 9;
  USB_DT_INTERFACE_SIZE	     = 9;
  USB_DT_ENDPOINT_SIZE	     = 7;
  USB_DT_ENDPOINT_AUDIO_SIZE = 9;  // Audio extension
  USB_DT_HUB_NONVAR_SIZE     = 7;

  USB_MAXENDPOINTS	     = 32;

  USB_ENDPOINT_ADDRESS_MASK  = $0f;    // in bEndpointAddress
  USB_ENDPOINT_DIR_MASK	     = $80;

  USB_ENDPOINT_TYPE_MASK	= $03; // in bmAttributes
  USB_ENDPOINT_TYPE_CONTROL	= 0;
  USB_ENDPOINT_TYPE_ISOCHRONOUS	= 1;
  USB_ENDPOINT_TYPE_BULK	= 2;
  USB_ENDPOINT_TYPE_INTERRUPT	= 3;

  USB_MAXINTERFACES	        = 32;

  USB_MAXALTSETTING	        = 128; // Hard limit

  USB_MAXCONFIG		        = 8;

  {
    Standard requests
  }
  USB_REQ_GET_STATUS		= $00;
  USB_REQ_CLEAR_FEATURE	        = $01;
  //  = $02 is reserved
  USB_REQ_SET_FEATURE		= $03;
  //  = $04 is reserved
  USB_REQ_SET_ADDRESS		= $05;
  USB_REQ_GET_DESCRIPTOR	= $06;
  USB_REQ_SET_DESCRIPTOR	= $07;
  USB_REQ_GET_CONFIGURATION	= $08;
  USB_REQ_SET_CONFIGURATION	= $09;
  USB_REQ_GET_INTERFACE		= $0A;
  USB_REQ_SET_INTERFACE		= $0B;
  USB_REQ_SYNCH_FRAME		= $0C;

  USB_TYPE_STANDARD		= ($00 << 5);
  USB_TYPE_CLASS		= ($01 << 5);
  USB_TYPE_VENDOR		= ($02 << 5);
  USB_TYPE_RESERVED		= ($03 << 5);

  USB_RECIP_DEVICE		= $00;
  USB_RECIP_INTERFACE	        = $01;
  USB_RECIP_ENDPOINT	        = $02;
  USB_RECIP_OTHER		= $03;

  {
    Various libusb API related stuff
  }
  USB_ENDPOINT_IN		= $80;
  USB_ENDPOINT_OUT		= $00;

  {
    Error codes
  }
  USB_ERROR_BEGIN		= 500000;

  {
    Device reset types for usb_reset_ex.
    http://msdn.microsoft.com/en-us/library/ff537269%28VS.85%29.aspx
    http://msdn.microsoft.com/en-us/library/ff537243%28v=vs.85%29.aspx
  }
  USB_RESET_TYPE_RESET_PORT     = (1 << 0);
  USB_RESET_TYPE_CYCLE_PORT     = (1 << 1);
  USB_RESET_TYPE_FULL_RESET     = (USB_RESET_TYPE_CYCLE_PORT or
                                   USB_RESET_TYPE_RESET_PORT);

  LIBUSB_HAS_INSTALL_SERVICE_NP       = 1;
  LIBUSB_HAS_UNINSTALL_SERVICE_NP     = 1;
  LIBUSB_HAS_INSTALL_DRIVER_NP        = 1;
  LIBUSB_HAS_TOUCH_INF_FILE_NP        = 1;
  LIBUSB_HAS_INSTALL_NEEDS_RESTART_NP = 1;
  LIBUSB_HAS_INSTALL_NP               = 1;

type
  {
    All standard descriptors have these 2 fields in common
  }
  PUsbDescriptorHeader = ^TUsbDescriptorHeader;
  TUsbDescriptorHeader = packed record
    bLength:         Byte;
    bDescriptorType: Byte;
  end;

  {
    String descriptor
  }
  PUsbStringDescriptor = ^TUsbStringDescriptor;
  TUsbStringDescriptor = packed record
    bLength:         Byte;
    bDescriptorType: Byte;
    wData:           array[0..0] of WideChar;
  end;

  {
    HID descriptor
  }
  PUsbHidDescriptor = ^TUsbHidDescriptor;
  TUsbHidDescriptor = packed record
     bLength:         Byte;
     bDescriptorType: Byte;
     bcdHID:          Word;
     bCountryCode:    Byte;
     bNumDescriptors: Byte;
  end;

  PUsbEndpointDescriptor = ^TUsbEndpointDescriptor;
  TUsbEndpointDescriptor = packed record
      bLength:          Byte;
      bDescriptorType:  Byte;
      bEndpointAddress: Byte;
      bmAttributes:     Byte;
      wMaxPacketSize:   Word;
      bInterval:        Byte;
      bRefresh:         Byte;
      bSynchAddress:    Word;
      extra:            PChar;	// Extra descriptors
      extralen:         Int32;
  end;

  {
    Interface descriptor
  }
  PUsbInterfaceDescriptor = ^TUsbInterfaceDescriptor;
  TUsbInterfaceDescriptor = packed record
    bLength:            Byte;
    bDescriptorType:    Byte;
    bInterfaceNumber:   Byte;
    bAlternateSetting:  Byte;
    bNumEndpoints:      Byte;
    bInterfaceClass:    Byte;
    bInterfaceSubClass: Byte;
    bInterfaceProtocol: Byte;
    iInterface:         Byte;
    endpoint:           PUsbEndpointDescriptor;
    extra:              PChar;	// Extra descriptors
    extralen:           Int32;
  end;

  PUsbInterface = ^TUsbInterface;
  TUsbInterface = packed record
    altsetting:         PUsbInterfaceDescriptor;
    num_altsetting:     Int32;
  end;

  PUsbConfigDescriptor = ^TUsbConfigDescriptor;
  TUsbConfigDescriptor = packed record
    bLength:             Byte;
    bDescriptorType:     Byte;
    wTotalLength:        Word;
    bNumInterfaces:      Byte;
    bConfigurationValue: Byte;
    iConfiguration:      Byte;
    bmAttributes:        Byte;
    MaxPower:            Byte;
    intf:                PUsbInterface;
    extra:               PChar;	// Extra descriptors
    extralen:            Int32;
  end;

  {
    Device descriptor
  }
  PUsbDeviceDescriptor = ^TUsbDeviceDescriptor;
  TUsbDeviceDescriptor = packed record
     bLength:            Byte;
     bDescriptorType:    Byte;
     bcdUSB:             Word;
     bDeviceClass:       Byte;
     bDeviceSubClass:    Byte;
     bDeviceProtocol:    Byte;
     bMaxPacketSize0:    Byte;
     idVendor:           Word;
     idProduct:          Word;
     bcdDevice:          Word;
     iManufacturer:      Byte;
     iProduct:           Byte;
     iSerialNumber:      Byte;
     bNumConfigurations: Byte;
  end;

  TUsbCtrlSetup = packed record
    bRequestType:        Byte;
    bRequest:            Byte;
    wValue:              Word;
    wIndex:              Word;
    wLength:             Word;
  end;

  {
    Data types
    usb_device
    usb_bus;
  }
  PUsbBus = ^TUsbBus;
  PUsbDevice = ^TUsbDevice;
  PPUsbDevice = ^PUsbDevice;
  TUsbDevice = packed record
    next: PUsbDevice;
    prev: PUsbDevice;
    filename: array[0..LIBUSB_PATH_MAX-1] of Char;
    bus: PUsbBus;
    descriptor: TUsbDeviceDescriptor;
    config: PUsbConfigDescriptor;
    dev: Pointer;		// Darwin support
    devnum: Byte;
    num_children: Byte;
    children: PPUsbDevice;
  end;

  TUsbBus = packed record
    next: PUsbBus;
    prev: PUsbBus;
    dirname: array[0..LIBUSB_PATH_MAX-1] of Char;
    devices: PUsbDevice;
    location: Dword;
    root_dev: PUsbDevice;
  end;

  {
    Version information, Windows specific
  }
  PUsbVersion = ^TUsbVersion;
  TUsbVersion = packed record
      dll: packed record
        major: Int32;
        minor: Int32;
        micro: Int32;
        nano:  Int32;
      end;
      driver: packed record
        major: Int32;
        minor: Int32;
        micro: Int32;
        nano:  Int32;
      end;
  end;

  TUsbDevHandle = THandle;

  TUsbOpenProc = function(Dev: PUsbDevice): TUsbDevHandle; cdecl;
  TUsbCloseProc = function(Handle: TUsbDevHandle): Int32; cdecl;
  TUsbGetStringProc = function(Handle: TUsbDevHandle; Index: Int32;
   LangId: Int32; Buf: Pointer; BufLen: Dword): Int32; cdecl;
  TUsbGetStringSimpleProc = function(Handle: TUsbDevHandle; Index: Int32;
   Buf: Pointer; BufLen: Dword): Int32; cdecl;
  TUsbGetDescriptorByEndpointProc = function(Handle: TUsbDevHandle; Ep: Int32;
   AType: Byte; Index: Byte; Buf: Pointer; Size: Int32): Int32; cdecl;
  TUsbGetDescriptorProc = function(Handle: TUsbDevHandle; AType: Byte;
   Index: Byte; Buf: Pointer; Size: Int32): Int32; cdecl;
  TUsbBulkWriteProc = function(Handle: TUsbDevHandle; Ep: Int32;
   Bytes: Pointer; Size: Int32; Timeout: Int32): Int32;
  TUsbBulkReadProc = function(Handle: TUsbDevHandle; Ep: Int32; Bytes: Pointer;
   Size: Int32; Timeout: Int32): Int32; cdecl;
  TUsbInterruptWriteProc = function(Handle: TUsbDevHandle; Ep: Int32;
    Bytes: Pointer; Size: Int32; Timeout: Int32): Int32;
  TUsbInterruptReadProc = function(Handle: TUsbDevHandle; Ep: Int32;
   Bytes: Pointer; Size: Int32; Timeout: Int32): Int32; cdecl;
  TUsbControlMsgProc = function(Handle: TUsbDevHandle; RequestType: Int32;
   Request: Int32; Value: Int32; Index: Int32; Bytes: Pointer; Size: Int32;
   Timeout: Int32): Int32; cdecl;
   TUsbSetConfigurationProc = function(Handle: TUsbDevHandle;
    Configuration: Int32): Int32; cdecl;
   TUsbClaimInterfaceProc = function(Handle: TUsbDevHandle;
    Intf: Int32): Int32; cdecl;
   TUsbReleaseInterfaceProc = function(Handle: TUsbDevHandle;
    Intf: Int32): Int32; cdecl;
   TUsbSetAltInterfaceProc = function(Handle: TUsbDevHandle;
    Alternate: Int32): Int32; cdecl;
   TUsbResetEpProc = function(Handle: TUsbDevHandle; Ep: Dword): Int32; cdecl;
   TUsbClearHaltProc = function(Handle: TUsbDevHandle; Ep: Dword): Int32; cdecl;
   TUsbResetProc = function(Handle: TUsbDevHandle): Int32; cdecl;
   TUsbResetExProc = function(Handle: TUsbDevHandle;
    ResetType: DWord): Int32; cdecl;
   TUsbStrErrorProc = function: PChar; cdecl;
   TUsbInitProc = procedure; cdecl;
   TUsbSetDebugProc = procedure(Level: Int32); cdecl;
   TUsbFindBussesProc = function: Int32; cdecl;
   TUsbFindDevicesProc = function: Int32; cdecl;
   TUsbDeviceProc = function(Handle: TUsbDevHandle): PUsbDevice; cdecl;
   TUsbGetBussesProc = function: PUsbBus; cdecl;
   TUsbGetVersionProc = function: PUsbVersion;
   TUsbIsochronousSetupAsyncProc = function(Handle: TUsbDevHandle;
    Context: Pointer; Ep: Byte; PktSize: Int32): Int32; cdecl;
   TUsbBulkSetupAsyncProc = function(Handle: TUsbDevHandle; Context: Pointer;
    Ep: Byte): Int32; cdecl;
   TUsbInterruptSetupAsyncProc = function(Handle: TUsbDevHandle;
    Context: Pointer; Ep: Byte): Int32; cdecl;
   TUsbSubmitAsyncProc = function(Context: Pointer; Bytes: Pointer;
    Size: Int32): Int32; cdecl;

   {
     Windows specific functions
   }

var
  UsbOpen: TUsbOpenProc;
  UsbClose: TUsbCloseProc;
  UsbGetString: TUsbGetStringProc;
  UsbGetStringSimple: TUsbGetStringSimpleProc;
  UsbGetDescriptorByEndpoint: TUsbGetDescriptorByEndpointProc;
  UsbGetDescriptor: TUsbGetDescriptorProc;
  UsbBulkWrite: TUsbBulkWriteProc;
  UsbBulkRead: TUsbBulkReadProc;
  UsbInterruptWrite: TUsbInterruptWriteProc;
  UsbInterruptRead: TUsbInterruptReadProc;
  UsbControlMsg: TUsbControlMsgProc;
  UsbSetConfiguration: TUsbSetConfigurationProc;
  UsbClaimInterface: TUsbClaimInterfaceProc;
  UsbReleaseInterface: TUsbReleaseInterfaceProc;
  UsbSetAltInterface: TUsbSetAltInterfaceProc;
  UsbResetEp: TUsbResetEpProc;
  UsbClearHalt: TUsbClearHaltProc;
  UsbReset: TUsbResetProc;
  UsbResetEx: TUsbResetExProc;
  UsbStrError: TUsbStrErrorProc;
  UsbInit: TUsbInitProc;
  UsbSetDebug: TUsbSetDebugProc;
  UsbFindBusses: TUsbFindBussesProc;
  UsbFindDevices: TUsbFindDevicesProc;
  UsbDevice: TUsbDeviceProc;
  UsbGetBusses: TUsbGetBussesProc;
  UsbGetVersion: TUsbGetVersionProc;
  UsbIsochronousSetupAsync: TUsbIsochronousSetupAsyncProc;
  UsbBulkSetupAsync: TUsbBulkSetupAsyncProc;
  UsbInterruptSetupAsync: TUsbInterruptSetupAsyncProc;
  UsbSubmitAsync: TUsbSubmitAsyncProc;

function UsbIsLoaded: Boolean;
procedure Load;
procedure Unload;

implementation

uses UGlobal;

var
  LibHandle: THandle;
  LoadCount: Integer;

function GetAddresses: Boolean; forward;

procedure Init;
begin
  LibHandle := 0;
  LoadCount := 0;
end;

procedure Fin;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
end;

procedure Load;
begin
  if LoadCount > 0 then
  begin
    Inc(LoadCount);
  end
  else
  begin
    LibHandle := LoadLibraryW('libusb0.dll');
    if LibHandle <> 0 then
      LoadCount := 1;
    if GetAddresses
      then UsbInit
      else Unload;
  end
end;

function GetAddresses: Boolean;

function Ga(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(LibHandle, ProcName);
end;

begin
  UsbOpen :=
   TUsbOpenProc(Ga('usb_open'));
  UsbClose :=
   TUsbCloseProc(Ga('usb_close'));
  UsbGetString :=
   TUsbGetStringProc(Ga('usb_get_string'));
  UsbGetStringSimple :=
   TUsbGetStringSimpleProc(Ga('usb_get_string_simple'));
  UsbGetDescriptorByEndpoint :=
   TUsbGetDescriptorByEndpointProc(Ga('usb_get_descriptor_by_endpoint'));
  UsbGetDescriptor :=
   TUsbGetDescriptorProc(Ga('usb_get_descriptor'));
  UsbBulkWrite :=
   TUsbBulkWriteProc(Ga('usb_bulk_write'));
  UsbBulkRead :=
   TUsbBulkReadProc(Ga('usb_bulk_read'));
  UsbInterruptWrite :=
   TUsbInterruptWriteProc(Ga('usb_interrupt_write'));
  UsbInterruptRead :=
   TUsbInterruptReadProc(Ga('usb_interrupt_read'));
  UsbControlMsg :=
   TUsbControlMsgProc(Ga('usb_control_msg'));
  UsbSetConfiguration :=
   TUsbSetConfigurationProc(Ga('usb_set_configuration'));
  UsbClaimInterface :=
   TUsbClaimInterfaceProc(Ga('usb_claim_interface'));
  UsbReleaseInterface :=
   TUsbReleaseInterfaceProc(Ga('usb_release_interface'));
  UsbSetAltInterface :=
   TUsbSetAltInterfaceProc(Ga('usb_set_altinterface'));
  UsbResetEp :=
   TUsbResetEpProc(Ga('usb_resetep'));
  UsbClearHalt :=
   TUsbClearHaltProc(Ga('usb_clear_halt'));
  UsbReset :=
   TUsbResetProc(Ga('usb_reset'));
  UsbResetEx :=
   TUsbResetExProc(Ga('usb_reset_ex'));
  UsbStrError :=
   TUsbStrErrorProc(Ga('usb_strerror'));
  UsbInit :=
   TUsbInitProc(Ga('usb_init'));
  UsbSetDebug :=
   TUsbSetDebugProc(Ga('usb_set_debug'));
  UsbFindBusses :=
   TUsbFindBussesProc(Ga('usb_find_busses'));
  UsbFindDevices :=
   TUsbFindDevicesProc(Ga('usb_find_devices'));
  UsbDevice :=
   TUsbDeviceProc(Ga('usb_device'));
  UsbGetBusses :=
   TUsbGetBussesProc(Ga('usb_get_busses'));
  UsbGetVersion :=
   TUsbGetVersionProc(Ga('usb_get_version'));
  UsbIsochronousSetupAsync :=
   TUsbIsochronousSetupAsyncProc(Ga('usb_isochronous_setup_async'));
  UsbBulkSetupAsync :=
   TUsbBulkSetupAsyncProc(Ga('usb_bulk_setup_async'));
  UsbInterruptSetupAsync :=
   TUsbInterruptSetupAsyncProc(Ga('usb_interrupt_setup_async'));
  UsbSubmitAsync :=
   TUsbSubmitAsyncProc(Ga('usb_submit_async'));

  Result :=
   Assigned(UsbOpen) and
   Assigned(UsbClose) and
   Assigned(UsbGetString) and
   Assigned(UsbGetStringSimple) and
   Assigned(UsbGetDescriptorByEndpoint) and
   Assigned(UsbGetDescriptor) and
   Assigned(UsbBulkWrite) and
   Assigned(UsbBulkRead) and
   Assigned(UsbInterruptRead) and
   Assigned(UsbControlMsg) and
   Assigned(UsbSetConfiguration) and
   Assigned(UsbClaimInterface) and
   Assigned(UsbReleaseInterface) and
   Assigned(UsbSetAltInterface) and
   Assigned(UsbResetEp) and
   Assigned(UsbClearHalt) and
   Assigned(UsbReset) and
   Assigned(UsbResetEx) and
   Assigned(UsbStrError) and
   Assigned(UsbInit) and
   Assigned(UsbSetDebug) and
   Assigned(UsbFindBusses) and
   Assigned(UsbFindDevices) and
   Assigned(UsbDevice) and
   Assigned(UsbGetBusses) and
   Assigned(UsbGetVersion) and
   Assigned(UsbIsochronousSetupAsync) and
   Assigned(UsbBulkSetupAsync) and
   Assigned(UsbInterruptSetupAsync) and
   Assigned(UsbSubmitAsync);
end;

procedure Unload;
begin
  if LoadCount > 0 then
  begin
    Dec(LoadCount);
    if LoadCount = 0 then
    begin
      FreeLibrary(LibHandle);
      LibHandle := 0;
    end;
  end;
end;

function UsbIsLoaded: Boolean;
begin
  Result := LoadCount > 0;
end;

initialization

  Init;

finalization

  Fin;

end.

