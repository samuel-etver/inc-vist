unit UInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UGlobal;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    CloseBtn: TButton;
    IncTitlePnl: TPanel;
    VistTitlePnl: TPanel;
    IncReadPnl: TPanel;
    IncAddedPnl: TPanel;
    IncMatchedPnl: TPanel;
    ReadTitlePnl: TPanel;
    AddedTitlePnl: TPanel;
    MatchedTitlePnl: TPanel;
    VistReadPnl: TPanel;
    VistAddedPnl: TPanel;
    VistMatchedPnl: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure LanguageChanged;
  public
    { public declarations }
    class function Execute: Boolean;
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  LanguageChanged;
  IncReadPnl.Caption := IntToStr(IncDataRead);
  IncMatchedPnl.Caption := IntToStr(IncDataMatched);
  IncAddedPnl.Caption := IntToStr(IncDataAdded);
  VistReadPnl.Caption := IntToStr(VistDataRead);
  VistMatchedPnl.Caption := IntToStr(VistDataMatched);
  VistAddedPnl.Caption := IntToStr(VistDataAdded);
end;

procedure TInfoForm.LanguageChanged;
const
  RightSpaces = '  ';
begin
  Caption := Gti(1800);
  CloseBtn.Caption := Gti(1002);
  ReadTitlePnl.Caption := Gti(1801) + RightSpaces;
  AddedTitlePnl.Caption := Gti(1802) + RightSpaces;
  MatchedTitlePnl.Caption := Gti(1803) + RightSpaces;
  IncTitlePnl.Caption := Gti(1804);
  VistTitlePnl.Caption := Gti(1805);
end;

class function TInfoForm.Execute: Boolean;
begin
  InfoForm := TInfoForm.Create(Application);
  InfoForm.showModal;
  InfoForm.Free;
  Result := True;
end;

end.

