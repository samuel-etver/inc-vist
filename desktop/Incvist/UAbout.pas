unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UGlobal;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseBtn: TButton;
    ManufacturerLbl: TLabel;
    Panel1: TPanel;
    ProgramLbl: TLabel;
    VersionLbl: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure LanguageChanged;
  public
    { public declarations }
    class function Execute: Boolean;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  LanguageChanged;
end;

procedure TAboutForm.LanguageChanged;
begin
  Caption := Gti(1400);
  CloseBtn.Caption := Gti(1002);
  ManufacturerLbl.Caption := Gti(1401);
  ProgramLbl.Caption := Gti(1402);
  VersionLbl.Caption := Gti(1403);
end;

class function TAboutForm.Execute: Boolean;
begin
  AboutForm := TAboutForm.Create(Application);
  AboutForm.ShowModal;
  AboutForm.Free;
  Result := True;
end;

end.

