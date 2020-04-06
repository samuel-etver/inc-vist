unit udevicevarsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, Buttons, UDeviceVars, UGlobal;

type

  { TDeviceVarsForm }

  TDeviceVarsForm = class(TForm)
    UpdateTimer: TTimer;
    VarsDrawGrid: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure VarsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure VarsDrawGridEditingDone(Sender: TObject);
    procedure VarsDrawGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure VarsDrawGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    { private declarations }
    FGridEditText: String;
    FGridEditRow: Integer;
    FGridEditCol: Integer;
  public
    { public declarations }
  end;

var
  DeviceVarsForm: TDeviceVarsForm;

implementation

{$R *.lfm}

{ TDeviceVarsForm }

procedure TDeviceVarsForm.VarsDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Txt: String;
  Vars: TDeviceVarList;
  DeviceVar: TDeviceVar;
begin
  if aRow + aCol = 0 then Exit;

  Vars := Device.VarList;

  with VarsDrawGrid.Canvas do begin
    if aCol = 0 then begin
      Txt := IntToStr(aRow);
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2,
       (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
      Exit;
    end;

    if aRow = 0 then begin
      case aCol of
        1: Txt := 'Var';
        2: Txt := 'Type';
        3: Txt := 'Value';
        else Txt := '';
      end;
      Font.Style := Font.Style + [fsBold];
      TextRect(aRect, aRect.Left + 4, (aRect.Top + aRect.Bottom -
       TextHeight('Wg')) div 2, Txt);
      Font.Style := Font.Style - [fsBold];
      Exit;
    end;

    if aRow > Vars.Count then Exit;

    if aCol = 1 then begin
      Txt := Vars.Items[aRow - 1].VarName;
      TextRect(aRect, aRect.Left + 4, (aRect.Top + aRect.Bottom -
       TextHeight('Wg')) div 2, Txt);
      Exit;
    end;

    DeviceVar := Vars.Items[aRow - 1];

    if aCol = 2 then begin
      case DeviceVar.VarType of
        VAR_TYPE_BOOL:    Txt := 'BOOL';
        VAR_TYPE_BYTE:    Txt := 'BYTE';
        VAR_TYPE_INT8:    Txt := 'INT8';
        VAR_TYPE_WORD:    Txt := 'WORD';
        VAR_TYPE_INT16:   Txt := 'INT16';
        VAR_TYPE_DWORD:   Txt := 'DWORD';
        VAR_TYPE_INT32:   Txt := 'INT32';
        VAR_TYPE_FLOAT32: Txt := 'FLOAT32';
        else Txt := IntToStr(DeviceVar.VarType);
      end;
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2,
       (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
      Exit;
    end;

    if aCol = 3 then begin
      Txt := DeviceVar.ToStr;
      TextRect(aRect, aRect.Right - TextWidth(Txt) - 4,
       (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
      Exit;
    end;
  end;
end;

procedure TDeviceVarsForm.VarsDrawGridEditingDone(Sender: TObject);
var
  Txt: String;
  IVal: Integer;
  FVal: Double;
  Vars: TDeviceVarList;
begin
  if FGridEditCol <> 3 then Exit;
  Vars := Device.VarList;
  if FGridEditRow > Vars.Count then Exit;
  Txt := LowerCase(Trim(FGridEditText));
  try
    with Vars.Items[FGridEditRow - 1] do begin
      case VarType of
        VAR_TYPE_BOOL:
          if (Txt = 'true') or (Txt = 't') then BoolValue := True
          else if (Txt = 'false') or (Txt = 'f') then BoolValue := False;
        VAR_TYPE_BYTE: begin
          IVal := StrToInt(Txt);
          if (IVal >= 0) and (IVal < $100) then
            ByteValue := Byte(IVal);
        end;
        VAR_TYPE_INT8: begin
          IVal := StrToInt(Txt);
          if (IVal >= -128) and (IVal <= 127) then
            Int8Value := Int8(IVal);
        end;
        VAR_TYPE_WORD: begin
          IVal := StrToInt(Txt);
          if (IVal >= 0) and (IVal <= $10000) then
            Int16Value := Word(IVal);
        end;
        VAR_TYPE_INT16: begin
          IVal := StrToInt(Txt);
          if (IVal >= -32768) and (IVal <= 32767) then
            Int32Value := Int32(IVal);
        end;
        VAR_TYPE_DWORD: begin
          FVal := StrToFloat(Txt);
          if (FVal >= 0) and (FVal < $100000000) then
            DwordValue := Dword(Trunc(FVal));
        end;
        VAR_TYPE_INT32: begin
          Int32Value := StrToInt(Txt);
        end;
        VAR_TYPE_FLOAT32: begin
          Float32Value := StrToFloat(Txt);
        end;
      end;
    end;
  except
  end;
end;

procedure TDeviceVarsForm.VarsDrawGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
var
  Vars: TDeviceVarList;
begin
  Vars := Device.VarList;
  Value := '';
  if (ACol = 3) and (ARow <= Vars.Count) then
    Value := Vars.Items[ARow - 1].ToStr;
end;

procedure TDeviceVarsForm.VarsDrawGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  FGridEditText := Value;
  FGridEditRow := aRow;
  FGridEditCol := aCol;
end;

procedure TDeviceVarsForm.FormCreate(Sender: TObject);
begin
  with VarsDrawGrid do begin
    RowCount := Device.VarList.Count + 1;
    ColWidths[1] := 140;
    ColWidths[2] := 64;
    ColWidths[3] := 80;
  end;
end;

procedure TDeviceVarsForm.UpdateBtnClick(Sender: TObject);
begin
  VarsDrawGrid.Invalidate;
end;

procedure TDeviceVarsForm.UpdateTimerTimer(Sender: TObject);
begin
  VarsDrawGrid.Invalidate;
end;

end.

