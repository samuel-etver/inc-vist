unit UMemJumpForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, UGlobal;

type

  { TMemJumpForm }

  TMemJumpForm = class(TForm)
    AddressEdit: TEdit;
    GroupBox1: TGroupBox;
    HexCheckBox: TCheckBox;
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure AddressEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HexCheckBoxChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    { private declarations }
    FAddress: Dword;
    FMemSize: Dword;
    FHex: Boolean;
    function StrToAddress(ATxt: String): Dword;
  public
    { public declarations }
    property Address: Dword read FAddress write FAddress;
    property MemSize: DWord read FMemSize write FMemSize;
    property Hex: Boolean read FHex write FHex;
  end;

var
  MemJumpForm: TMemJumpForm;

implementation

{$R *.lfm}

{ TMemJumpForm }

procedure TMemJumpForm.FormCreate(Sender: TObject);
begin
  FMemSize := 0;
  FAddress := 0;
  FHex := True;
end;

procedure TMemJumpForm.FormShow(Sender: TObject);
begin
  HexCheckBox.Checked := FHex;
  AddressEdit.Text := AddressToStr(FAddress, Hex);
end;

procedure TMemJumpForm.AddressEditChange(Sender: TObject);
begin
  try
    StrToAddress(AddressEdit.Text);
    AddressEdit.Font.Color := clDefault;
  except
    AddressEdit.Font.Color := clRed;
  end;
end;

procedure TMemJumpForm.HexCheckBoxChange(Sender: TObject);
begin
  Hex := HexCheckBox.Checked;
  AddressEditChange(AddressEdit);
end;

procedure TMemJumpForm.OkBtnClick(Sender: TObject);
begin
  try
    FAddress := StrToAddress(AddressEdit.Text);
    FHex := HexCheckBox.Checked;
    ModalResult := mrOk;
  except
  end;
end;

function TMemJumpForm.StrToAddress(ATxt: String): Dword;
var
  I: Int64;
begin
  I := UGlobal.StrToAddress(ATxt, Hex);
  if (I < 0) or (I >= FMemSize) then
    raise Exception.Create('Range error');
  Result := Dword(I);
end;

end.

