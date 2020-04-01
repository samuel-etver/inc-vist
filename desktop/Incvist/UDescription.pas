unit UDescription;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, UTypes;

type

  { TDescriptionFrame }

  TDescriptionFrame = class(TFrame)
    DescriptionMemo: TMemo;
  private
    { private declarations }
    FDataEdited: TNotifyEvent;
    procedure DescriptionMemoChange(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure LanguageChanged;
    procedure DataChanged;
    procedure GotoFirstRecord;
    procedure Clear;
    procedure CommitData;
    property DataEdited: TNotifyEvent read FDataEdited write FDataEdited;
  end;

implementation

uses
  UGlobal;

{$R *.lfm}

constructor TDescriptionFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataEdited := nil;
end;

procedure TDescriptionFrame.Loaded;
begin
  inherited;
  DescriptionMemo.OnChange := @DescriptionMemoChange;
end;

procedure TDescriptionFrame.LanguageChanged;
begin

end;

procedure TDescriptionFrame.DataChanged;
var
  Txt: TWString;
begin
  DescriptionMemo.OnChange := nil;
  Txt := DevQuery.Description.Text;
  if Assigned(Txt)
    then DescriptionMemo.Text := Txt.Value
    else DescriptionMemo.Clear;
  DescriptionMemo.OnChange := @DescriptionMemoChange;
end;

procedure TDescriptionFrame.GotoFirstRecord;
begin
  DescriptionMemo.SelStart  := 0;
  DescriptionMemo.SelLength := 0;
end;

procedure TDescriptionFrame.Clear;
begin
  DescriptionMemo.Clear;
end;

procedure TDescriptionFrame.DescriptionMemoChange(Sender: TObject);
begin
  if ProjOpened and (not ProjEdited) then
  begin
    ProjEdited := True;
    if Assigned(FDataEdited) then
      FDataEdited(Self);
  end;
end;

procedure TDescriptionFrame.CommitData;
begin
  with DevData.Description do
  begin
    if not Assigned(Text) then
      Text := TWString.Create;
    Text.Value := DescriptionMemo.Text;
    DevQuery.Description.Text := Text;
  end;
end;

end.

