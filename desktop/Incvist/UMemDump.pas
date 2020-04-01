unit UMemDump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, Grids, Menus, UGlobal;

type

  { TMemDumpForm }

  TMemDumpForm = class(TForm)
    MemDumpGrid: TDrawGrid;
    ClearMenuItem: TMenuItem;
    UpdateMenuItem: TMenuItem;
    SetMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ClearMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemDumpGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure SetMenuItemClick(Sender: TObject);
    procedure UpdateMenuItemClick(Sender: TObject);
  private
    { private declarations }
    FAddressTxt: String;
    function CalcRowCount: Integer;
    procedure SetMemDump(Value: Byte);
  public
    { public declarations }
    procedure LanguageChanged;
    procedure UpdateView;
  end;

var
  MemDumpForm: TMemDumpForm;

implementation

{$R *.lfm}

{ TMemDumpForm }

procedure TMemDumpForm.FormCreate(Sender: TObject);
begin
  FAddressTxt := '';
  MemDumpGrid.RowCount := CalcRowCount;
  MemDumpGrid.ColWidths[0] := 58;
  LanguageChanged;
end;

procedure TMemDumpForm.FormShow(Sender: TObject);
begin
  UpdateView;
end;

procedure TMemDumpForm.LanguageChanged;
begin
  Caption := Gti(2300);
  FAddressTxt := Gti(2301);
  ClearMenuItem.Caption := Gti(2302);
  SetMenuItem.Caption := Gti(2303);
  UpdateMenuItem.Caption := Gti(2304);
end;

procedure TMemDumpForm.ClearMenuItemClick(Sender: TObject);
begin
  SetMemDump($00);
end;

procedure TMemDumpForm.MemDumpGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Txt: String;
  Y: Integer;
  Index: Integer;
begin
  with (Sender as TDrawGrid).Canvas do
  begin
    Y := (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2;
    if ARow = 0 then
    begin
      if ACol = 0
        then Txt := FAddressTxt
        else Txt := IntToHex(ACol - 1, 1) + '/' + IntToHex(ACol - 1 + 8, 1);
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2, Y, Txt);
      Exit;
    end;

    if ACol = 0 then
    begin
      Txt := IntToHex((ARow - 1)*8, Length(IntToStr(MemDumpSize - 1)));
      TextRect(aRect, aRect.Right - 6 - TextWidth(Txt), Y, Txt);
      Exit;
    end;

    Index := (ARow - 1)*8 + ACol - 1;
    if Index < MemDumpSize then
    begin
      Txt := IntToHex(MemDump[index], 2);
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2, Y, Txt);
    end;
  end;
end;

procedure TMemDumpForm.SetMenuItemClick(Sender: TObject);
begin
  SetMemDump($FF);
end;

procedure TMemDumpForm.UpdateMenuItemClick(Sender: TObject);
begin
  MemDumpGrid.Invalidate;
end;

function TMemDumpForm.CalcRowCount: Integer;
begin
  Result := (MemDumpSize div 8) + 1;
end;

procedure TMemDumpForm.SetMemDump(Value: Byte);
var
  I: Integer;
begin
  for I := 0 to MemDumpSize - 1 do
    MemDump[I] := Value;
  MemDumpGrid.Invalidate;
end;

procedure TMemDumpForm.UpdateView;
var
  NewRowCount: Integer;
begin
  NewRowCount := CalcRowCount;
  if NewRowCount <> MemDumpGrid.RowCount
    then MemDumpGrid.RowCount := NewRowCount
    else MemDumpGrid.Invalidate;
end;

end.

