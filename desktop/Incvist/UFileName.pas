unit UFileName;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TFileNameFrame }

  TFileNameFrame = class(TFrame)
    CaptionLbl: TLabel;
    FileNameLbl: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
    function GetCaption: String;
    procedure SetCaption(NewCaption: String);
    function GetFileName: String;
    procedure SetFileName(NewFileName: String);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    property Caption: String read GetCaption write SetCaption;
    property FileName: String read GetFileName write SetFileName;
  end;

implementation

{$R *.lfm}

constructor TFileNameFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileName := '';
end;

function TFileNameFrame.GetCaption: String;
begin
  Result := CaptionLbl.Caption;
end;

procedure TFileNameFrame.SetCaption(NewCaption: String);
begin
  CaptionLbl.Caption := NewCaption;
end;

function TFileNameFrame.GetFileName: String;
begin
  Result := FileNameLbl.Caption;
end;

procedure TFileNameFrame.SetFileName(NewFileName: String);
begin
  FileNameLbl.Caption := NewFileName;
end;

end.

