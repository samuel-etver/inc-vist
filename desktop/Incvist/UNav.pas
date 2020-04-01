unit UNav;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Buttons;

type

  { TNavigatorFrame }

  TNavigatorFrame = class(TFrame)
    FirstBtn: TSpeedButton;
    PriorBtn: TSpeedButton;
    NextBtn: TSpeedButton;
    LastBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
  private
    { private declarations }
  public
    { public declarations }
    procedure EnableFirst(Value: Boolean);
    procedure EnableLast(Value: Boolean);
    procedure EnablePrior(Value: Boolean);
    procedure EnableNext(Value: Boolean);
    procedure EnableDelete(Value: Boolean);
    procedure EnableAll(Value: Boolean);
  end;

implementation

{$R *.lfm}

procedure TNavigatorFrame.EnableFirst(Value: Boolean);
begin
  FirstBtn.Enabled := Value;
end;

procedure TNavigatorFrame.EnableLast(Value: Boolean);
begin
  LastBtn.Enabled := Value;
end;

procedure TNavigatorFrame.EnablePrior(Value: Boolean);
begin
  PriorBtn.Enabled := Value;
end;

procedure TNavigatorFrame.EnableNext(Value: Boolean);
begin
  NextBtn.Enabled := Value;
end;

procedure TNavigatorFrame.EnableDelete(Value: Boolean);
begin
  DeleteBtn.Enabled := Value;
end;

procedure TNavigatorFrame.EnableAll(Value: Boolean);
begin
  EnableFirst(Value);
  EnableLast(Value);
  EnablePrior(Value);
  EnableNext(Value);
  EnableDelete(Value);
end;

end.

