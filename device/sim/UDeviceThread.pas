unit UDeviceThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Windows;


type
  TDeviceThread = class
  private
    FKeys: Word;
    FLcdBitmap: Graphics.TBitmap;
    FLcdDataTicks: Dword;
    FLcdUpdated: Boolean;
  protected
    procedure GetLcdData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
    procedure Draw(Canvas: TCanvas);
    property Keys: Word read FKeys write FKeys;
    property LcdUpdated: Boolean read FLcdUpdated write FLcdUpdated;
  end;

implementation

uses
  UGlobal;

constructor TDeviceThread.Create;
begin
  FKeys := 0;
  FLcdBitmap        := Graphics.TBitmap.Create;
  FLcdBitmap.Width  := DeviceLcdW;
  FLcdBitmap.Height := DeviceLcdH;
  FLcdDataTicks     := GetTickCount;
  FLcdUpdated       := False;
end;

destructor TDeviceThread.Destroy;
begin
  FLcdBitmap.Free;
  inherited;
end;

procedure TDeviceThread.Execute;
begin
  Device.Keys := DeviceKeys;
  Device.Execute;
  if GetTickCount - FLcdDataTicks >= 50 then
  begin
    GetLcdData;
    FLcdUpdated   := True;
    FLcdDataTicks := GetTickCount;
  end;
end;

procedure TDeviceThread.Draw(Canvas: TCanvas);
begin
  Canvas.Draw(0, 0, FLcdBitmap);
end;

procedure TDeviceThread.GetLcdData;
var
  X: Integer;
  Y: Integer;
  W: Integer;
  H: Integer;
begin
  W := FLcdBitmap.Width;
  H := FLcdBitmap.Height;

  for X := 0 to W - 1 do
    for Y := 0 to H - 1 do
      FLcdBitmap.Canvas.Pixels[X, Y] := Device.Pixels[X, Y];
end;

end.

