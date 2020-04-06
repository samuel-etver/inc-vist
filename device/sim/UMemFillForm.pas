unit UMemFillForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UGlobal, strutils, windows;

type

  TFillValueType = (fvtDec, fvtHex, fvtSymbol);

  { TMemFillForm }

  TMemFillForm = class(TForm)
    Label4: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    HexCheckBox: TCheckBox;
    SizeEdit: TEdit;
    SizeBtn: TSpeedButton;
    ValueTypeComboBox: TComboBox;
    ValueEdit: TEdit;
    FromEdit: TEdit;
    ToEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    FromBeginningBtn: TSpeedButton;
    ToEndBtn: TSpeedButton;
    procedure SizeBtnClick(Sender: TObject);
    procedure SizeEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FromBeginningBtnClick(Sender: TObject);
    procedure FromEditChange(Sender: TObject);
    procedure HexCheckBoxChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ToEndBtnClick(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueTypeComboBoxChange(Sender: TObject);
  private
    { private declarations }
    FFromAddress: Dword;
    FToAddress: Dword;
    FMemSize: Uint64;
    FValue: Integer;
    FValueType: Integer;
    FHex: Boolean;
    FFromEditChangeEvent: TNotifyEvent;
    FToEditChangeEvent: TNotifyEvent;
    FSizeEditChangeEvent: TNotifyEvent;
    FHexCheckBoxChangeEvent: TNotifyEvent;
    function StrToValue(Txt: String): Integer;
    procedure SaveChangeHandlers;
    procedure RestoreChangeHandlers;
    function CheckAddressStr(AddressStr: String; AsHex: Boolean): Boolean;
    function AddressToStr(Address: Integer; AsHex: Boolean): String;
    function StrToAddress(AddressStr: String; AsHex: Boolean): Integer;
    function CheckSizeStr(SizeStr: String; AsHex: Boolean): Boolean;
    function SizeToStr(Size: Integer; AsHex: Boolean): String;
    function StrToSize(SizeStr: String; AsHex: Boolean): Integer;
    procedure SetEditState(Edit: TEdit; State: Boolean);
    procedure FromEditChangeImpl;
  public
    { public declarations }
    property FromAddress: Dword read FFromAddress write FFromAddress;
    property ToAddress: Dword read FToAddress write FToAddress;
    property MemSize: Uint64 read FMemSize write FMemSize;
    property Value: Integer read FValue write FValue;
    property ValueType: Integer read FValueType write FValueType;
    property Hex: Boolean read FHex write FHex;
  end;

var
  MemFillForm: TMemFillForm;

implementation

{$R *.lfm}

{ TMemFillForm }

procedure TMemFillForm.FromBeginningBtnClick(Sender: TObject);
begin
  FromEdit.Text := '0';
end;

procedure TMemFillForm.FromEditChange(Sender: TObject);
begin
  SaveChangeHandlers;
  FromEditChangeImpl;
  RestoreChangeHandlers;
end;

procedure TMemFillForm.FromEditChangeImpl;
var
  FromStr: String;
  ToStr: String;
  FromOk: Boolean;
  ToOk: Boolean;
  SizeOk: Boolean;
  FromValue: Integer = 0;
  ToValue: Integer = 0;
  SizeValue: Integer;

begin
  FromStr := Trim(FromEdit.Text);
  ToStr   := Trim(ToEdit.Text);
  FromOk := CheckAddressStr(FromStr, FHex);
  ToOk   := CheckAddressStr(ToStr,   FHex);

  if FromOk then
    FromValue := StrToAddress(FromStr, FHex);
  if ToOk then
    ToValue   := StrToAddress(ToStr, FHex);

  if FromOk and ToOk then begin
    SizeValue := ToValue - FromValue;
    SizeEdit.Text := SizeToStr(SizeValue, FHex);
    SizeOk := (SizeValue >= 0) and (SizeValue <= FMemSize);
    SetEditState(SizeEdit, SizeOk);
  end;

  if FromOk then
    FromOk := (FromValue >= 0) and (FromValue <= FMemSize);
  if ToOk then
    ToOk := (ToValue >= 0) and (ToValue <= FMemSize);

  SetEditState(FromEdit, FromOk);
  SetEditState(ToEdit, ToOk);
end;

procedure TMemFillForm.HexCheckBoxChange(Sender: TObject);
var
  Txt: String;

begin
  SaveChangeHandlers;

  FHex := (Sender as TCheckBox).Checked;

  Txt := Trim(FromEdit.Text);
  if CheckAddressStr(Txt, not FHex) then
    FromEdit.Text := AddressToStr(StrToAddress(Txt, not FHex), FHex);
  Txt := Trim(ToEdit.Text);
  if CheckAddressStr(Txt, not FHex) then
    ToEdit.Text := AddressToStr(StrToAddress(Txt, not FHex), FHex);
  Txt := Trim(SizeEdit.Text);
  if CheckAddressStr(Txt, not FHex) then
    SizeEdit.Text := SizeToStr(StrToSize(Txt, not FHex), FHex);

  RestoreChangeHandlers;
end;

procedure TMemFillForm.OkBtnClick(Sender: TObject);
var
  FromStr: String;
  ToStr: String;
begin
  try
    FromStr := Trim(FromEdit.Text);
    ToStr := Trim(ToEdit.Text);
    FFromAddress := StrToAddress(FromStr, FHex);
    FToAddress := StrToAddress(ToStr, FHex);
    FValue := StrToValue(ValueEdit.Text);
    FValueType := ValueTypeComboBox.ItemIndex;
    ModalResult := mrOk;
  except
    MessageBox(Handle, 'Check entered values!', 'Error', MB_OK or MB_ICONSTOP);
  end;
end;

procedure TMemFillForm.ToEndBtnClick(Sender: TObject);
begin
  ToEdit.Text := AddressToStr(FMemSize, FHex);
end;

procedure TMemFillForm.ValueEditChange(Sender: TObject);
var
  State: Boolean = False;
begin
  try
    StrToValue(ValueEdit.Text);
    State := True;
  except
  end;
  SetEditState(ValueEdit, State);
end;

procedure TMemFillForm.ValueTypeComboBoxChange(Sender: TObject);
begin
  ValueEditChange(ValueEdit);
end;

procedure TMemFillForm.FormCreate(Sender: TObject);
begin
  FFromAddress := 0;
  FToAddress := 0;
  FMemSize := 0;
  FValue := 0;
  FValueType := 0;
  FHex := True;
end;

procedure TMemFillForm.FormShow(Sender: TObject);
begin
  SaveChangeHandlers;
  HexCheckBox.Checked := FHex;
  if (FValueType >= 0) and (FValueType < ValueTypeComboBox.Items.Count)
    then ValueTypeComboBox.ItemIndex := FValueType;
  case ValueTypeComboBox.ItemIndex of
    0: ValueEdit.Text := IntToStr(FValue);
    1: ValueEdit.Text := IntToHex(FValue, 2);
    2: ValueEdit.Text := Chr(FValue);
  end;
  FromEdit.Text := AddressToStr(FFromAddress, FHex);
  ToEdit.Text := AddressToStr(FToAddress, FHex);
  SizeEdit.Text := AddressToStr(FToAddress - FFromAddress, FHex);
  FromEditChangeImpl;
  RestoreChangeHandlers;
end;

procedure TMemFillForm.SizeBtnClick(Sender: TObject);
begin
  FromEdit.Text := '0';
  ToEdit.Text := AddressToStr(FMemSize, Hex);
end;

procedure TMemFillForm.SizeEditChange(Sender: TObject);
var
  FromStr: String;
  SizeStr: String;
  FromOk: Boolean;
  ToOk: Boolean;
  SizeOk: Boolean;
  FromValue: Integer = 0;
  ToValue: Integer = 0;
  SizeValue: Integer;

begin
  SaveChangeHandlers;

  FromStr := Trim(FromEdit.Text);
  SizeStr := Trim(SizeEdit.Text);
  FromOk := CheckAddressStr(FromStr, FHex);
  SizeOk := CheckSizeStr(SizeStr, FHex);

  if FromOk then
    FromValue := StrToAddress(FromStr, FHex);
  if SizeOk then
    SizeValue := StrToAddress(SizeStr, FHex);

  if FromOk and SizeOk then begin
    ToValue := FromValue + SizeValue;
    ToEdit.Text := AddressToStr(ToValue, FHex);
    ToOk := (ToValue >= 0) and (ToValue <= FMemSize);
    SetEditState(ToEdit, ToOk);
  end;

  if FromOk then
    FromOk := (FromValue >= 0) and (FromValue <= FMemSize);
  if SizeOk then
    SizeOk := (SizeValue >= 0) and (SizeValue <= FMemSize);

  SetEditState(FromEdit, FromOk);
  SetEditState(SizeEdit, SizeOk);

  RestoreChangeHandlers;
end;

function TMemFillForm.StrToValue(Txt: String): Integer;
begin
  case ValueTypeComboBox.ItemIndex of
    0: Result := StrToInt(Txt);
    1: Result := StrToInt('$' + Trim(Txt));
    2: begin
      if Length(Txt) = 0
        then Txt := ' '
        else if Length(Txt) <> 1 then begin
          Txt := Trim(Txt);
          if Length(Txt) <> 1 then
            raise Exception.Create('It''s String, not symbol');
        end;
      Result := Ord(Txt[1]);
    end;
  end;
  if (Result < 0) or (Result > 255) then
    raise Exception.Create('Range error');
end;

procedure TMemFillForm.SaveChangeHandlers;
begin
  FFromEditChangeEvent := FromEdit.OnChange;
  FToEditChangeEvent := ToEdit.OnChange;
  FSizeEditChangeEvent := SizeEdit.OnChange;
  FHexCheckBoxChangeEvent := HexCheckBox.OnChange;

  FromEdit.OnChange := nil;
  ToEdit.OnChange := nil;
  SizeEdit.OnChange := nil;
  HexCheckBox.OnChange := nil;
end;

procedure TMemFillForm.RestoreChangeHandlers;
begin
  FromEdit.OnChange := FFromEditChangeEvent;
  ToEdit.OnChange := FToEditChangeEvent;
  SizeEdit.OnChange := FSizeEditChangeEvent;
  HexCheckBox.OnChange := FHexCheckBoxChangeEvent;
end;

function TMemFillForm.CheckAddressStr(AddressStr: String; AsHex: Boolean): Boolean;
begin
  Result := False;
  try
    if AsHex
      then StrToInt('$' + AddressStr)
      else StrToInt(AddressStr);
    Result := True;
  except
  end;
end;

function TMemFillForm.AddressToStr(Address: Integer; AsHex: Boolean): String;
begin
  if AsHex then begin
    Result := TrimLeftSet(HexStr(Address, 16), ['0']);
    if Length(Result) = 0 then
      Result := '0';
  end
  else begin
    Result := IntToStr(Address);
  end;
end;

function TMemFillForm.StrToAddress(AddressStr: String; AsHex: Boolean): Integer;
begin
  if AsHex
    then Result := StrToInt('$' + AddressStr)
    else Result := StrToInt(AddressStr);
end;

function TMemFillForm.CheckSizeStr(SizeStr: String; AsHex: Boolean): Boolean;
begin
  Result := CheckAddressStr(SizeStr, AsHex);
end;

function TMemFillForm.SizeToStr(Size: Integer; AsHex: Boolean): String;
begin
  Result := AddressToStr(Size, AsHex);
end;

function TMemFillForm.StrToSize(SizeStr: String; AsHex: Boolean): Integer;
begin
  Result := StrToAddress(SizeStr, AsHex);
end;

procedure TMemFillForm.SetEditState(Edit: TEdit; State: Boolean);
begin
  if State
    then Edit.Font.Color := clDefault
    else Edit.Font.Color := clRed;
end;

end.

