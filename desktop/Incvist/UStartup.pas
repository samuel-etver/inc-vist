unit UStartup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UGlobal;

type

  { TStartupForm }
  TChoice = (scNone, scOpenLast, scOpenExisting, scCreateNew);

  TStartupForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    DontShowCheckBox: TCheckBox;
    OpenExistingRadioBtn: TRadioButton;
    OpenLastRadioBtn: TRadioButton;
    CreateNewRadioBtn: TRadioButton;
    RadioGroup1: TRadioGroup;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure LanguageChanged;
    function GetChoice: TChoice;
  public
    { public declarations }
    procedure DisableOpenLast;
    property Choice: TChoice read GetChoice;
  end;

var
  StartupForm: TStartupForm;

implementation

{$R *.lfm}

{ TStartupForm }

procedure TStartupForm.FormCreate(Sender: TObject);
begin
  LanguageChanged;
end;

procedure TStartupForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  StartupDialogEnabled := not DontShowCheckBox.Checked;
  Save;
end;

procedure TStartupForm.LanguageChanged;
begin
  Caption := Gti(2200);
  OkBtn.Caption := Gti(1000);
  CancelBtn.Caption := Gti(1001);
  OpenExistingRadioBtn.Caption := Gti(2201);
  OpenLastRadioBtn.Caption := Gti(2202);
  CreateNewRadioBtn.Caption := Gti(2203);
  DontShowCheckBox.Caption := Gti(2204);
end;

function TStartupForm.GetChoice: TChoice;
begin
  if OpenExistingRadioBtn.Checked
    then Result := scOpenExisting
    else if OpenLastRadioBtn.Checked
      then Result := scOpenLast
      else if CreateNewRadioBtn.Checked
        then Result := scCreateNew
        else Result := scNone;
end;

procedure TStartupForm.DisableOpenLast;
begin
  OpenLastRadioBtn.Enabled := False;
end;

end.

