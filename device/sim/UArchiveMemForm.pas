unit UArchiveMemForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, Buttons, ExtCtrls, ActnList, UGlobal, UMemJumpForm, UMemSearchForm,
  UMemFillForm, Windows;

type

  { TArchiveMemForm }

  TArchiveMemForm = class(TForm)
    RefreshAction: TAction;
    JumpAction: TAction;
    FillAction: TAction;
    ImageList1: TImageList;
    JumpBtn1: TSpeedButton;
    SearchAgainAction: TAction;
    SearchAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    MemDrawGrid: TDrawGrid;
    Panel1: TPanel;
    HexBtn: TSpeedButton;
    DecBtn: TSpeedButton;
    JumpBtn: TSpeedButton;
    SearchBtn: TSpeedButton;
    SearchAgainBtn: TSpeedButton;
    FillBtn: TSpeedButton;
    StatusBar: TStatusBar;
    procedure FillActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HexBtnClick(Sender: TObject);
    procedure JumpActionExecute(Sender: TObject);
    procedure MemDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure MemDrawGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure MemDrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure MemDrawGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure RefreshActionExecute(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure SearchAgainActionExecute(Sender: TObject);
  private
    { private declarations }
    procedure MsgBoxNotFound;
  public
    { public declarations }
  end;

var
  ArchiveMemForm: TArchiveMemForm;

implementation

{$R *.lfm}

{ TArchiveMemForm }

procedure TArchiveMemForm.MemDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Txt: String;
  Index: Integer;
  Hex: Boolean;
begin
  Hex := HexBtn.Down;

  with MemDrawGrid.Canvas do begin
    if ARow + ACol = 0 then Exit;

    if (aRow = 0) and (aCol > 0) then begin
      Txt := IntToStr(aCol - 1) + '/' + IntToHex(aCol - 1 + 8, 1);
      Font.Style := Font.Style + [fsBold];
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2,
       (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
      Font.Style := Font.Style - [fsBold];
      Exit;
    end;

    if (aRow > 0) and (aCol = 0) then begin
      Txt := IntToHex((aRow - 1)*8, 8);
      Font.Style := Font.Style + [fsBold];
      TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2,
       (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
      Font.Style := Font.Style - [fsBold];
      Exit;
    end;

    Index := (aRow - 1)*8 + (aCol - 1);
    if Index >= ArchiveSize then Exit;
    if Hex
      then Txt := IntToHex(Device.FlashBytes[Index], 2)
      else Txt := IntToStr(Device.FlashBytes[Index]);
    TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2,
     (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2, Txt);
  end;
end;

procedure TArchiveMemForm.MemDrawGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
var
  I: Integer;
begin
  I := (aRow - 1)*8 + (aCol - 1);
  if HexBtn.Down
    then Value := IntToHex(Device.FlashBytes[I], 2)
    else Value := IntToStr(Device.FlashBytes[I]);
end;

procedure TArchiveMemForm.MemDrawGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  StatusBar.Panels[0].Text := 'Address: ' +
    AddressToStr(Dword((aRow - 1)*8 + (aCol - 1)), HexBtn.Down);
end;

procedure TArchiveMemForm.MemDrawGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  I: Integer;
begin
  try
    I := (ARow - 1)*8 + (ACol - 1);
    if HexBtn.Down
      then Device.FlashBytes[I] := StrToInt('$' + Trim(Value))
      else Device.FlashBytes[I] := StrToInt(Value);
  except
  end;
end;

procedure TArchiveMemForm.RefreshActionExecute(Sender: TObject);
begin
  MemDrawGrid.Invalidate;
end;

procedure TArchiveMemForm.SearchActionExecute(Sender: TObject);
var
  F: Boolean;
  I: Integer;
begin
  F := False;
  MemSearchForm := TMemSearchForm.Create(Self);
  if MemSearchFormShown then begin
    MemSearchForm.Left := MemSearchFormLeft;
    MemSearchForm.Top := MemSearchFormTop;
  end
  else begin
    MemSearchFormHex := True;
    MemSearchFormValue := 0;
    MemSearchFormValueType := 0;
    MemSearchFormFromAddress := 0;
    MemSearchFormToAddress := ArchiveSize;
    MemSearchForm.Left := (Screen.Width - MemSearchForm.Width) div 2;
    MemSearchForm.Top := (Screen.Height - MemSearchForm.Height) div 2;
  end;
  MemSearchForm.Hex := MemSearchFormHex;
  MemSearchForm.MemSize := ArchiveSize;
  MemSearchForm.Value := MemSearchFormValue;
  MemSearchForm.ValueType := MemSearchFormValueType;
  MemSearchForm.FromAddress := MemSearchFormFromAddress;
  MemSearchForm.ToAddress := MemSearchFormToAddress;

  if MemSearchForm.ShowModal = mrOk then begin
    MemSearchFormOk := True;
    MemSearchFormValue := MemSearchForm.Value;
    MemSearchFormHex := MemSearchForm.Hex;
    MemSearchFormValueType := MemSearchForm.ValueType;
    MemSearchFormFromAddress := MemSearchForm.FromAddress;
    MemSearchFormToAddress := MemSearchForm.ToAddress;
    F := True;
  end;

  MemSearchFormLeft := MemSearchForm.Left;
  MemSearchFormTop := MemSearchForm.Top;
  MemSearchFormShown := True;
  MemSearchForm.Free;

  if F then begin
    for I := MemSearchFormFromAddress to MemSearchFormToAddress - 1 do
      if Device.FlashBytes[I] = MemSearchFormValue then begin
        ArchiveMemFormSearchIndex := I;
        MemDrawGrid.Row := (I div 8) + 1;
        MemDrawGrid.Col := (I mod 8) + 1;
        Exit;
      end;
    MsgBoxNotFound;
  end;
end;

procedure TArchiveMemForm.MsgBoxNotFound;
begin
  MessageBox(Handle, 'Nothing found!',
   'Search', MB_OK or MB_ICONEXCLAMATION);
end;

procedure TArchiveMemForm.SearchAgainActionExecute(Sender: TObject);
var
  I: Dword;
  Index: Int64;
begin
  if not MemSearchFormOk then begin
    SearchActionExecute(nil); Exit;
  end;

  if (ArchiveMemFormSearchIndex < MemSearchFormFromAddress) or
     (ArchiveMemFormSearchIndex >= MemSearchFormToAddress)
    then Index := MemSearchFormFromAddress
    else Index := ArchiveMemFormSearchIndex + 1;
  for I := MemSearchFormFromAddress to MemSearchFormToAddress do begin
    if Device.FlashBytes[Index] = MemSearchFormValue then begin
      ArchiveMemFormSearchIndex := Index;
      MemDrawGrid.Row := (Index div 8) + 1;
      MemDrawGrid.Col := (Index mod 8) + 1;
      Exit;
    end;
    Inc(Index);
    if Index = MemSearchFormToAddress then Index := MemSearchFormFromAddress;
  end;

  ArchiveMemFormSearchIndex := -1;
  MsgBoxNotFound;
end;

procedure TArchiveMemForm.FormCreate(Sender: TObject);
begin
  MemDrawGrid.RowCount := 1 + (ArchiveSize div 8);
  StatusBar.Panels[StatusBar.Panels.Count - 1].Text := 'Memory size: ' +
   IntToStr(ArchiveSize) + ' bytes';

  HexBtn.Down := True
end;

procedure TArchiveMemForm.FillActionExecute(Sender: TObject);
var
  F: Boolean;
  I: Integer;
begin
  F := False;
  MemFillForm := TMemFillForm.Create(Application);

  if MemFillFormShown then begin
    MemFillForm.Left := MemFillFormLeft;
    MemFillForm.Top := MemFillFormTop;
  end
  else begin
    MemFillFormHex := True;
    MemFillFormValue := 0;
    MemFillFormValueType := 0;
    MemFillFormFromAddress := 0;
    MemFillFormToAddress := ArchiveSize;
    MemFillForm.Left := (Screen.Width  - MemFillForm.Width) div 2;
    MemFillForm.Top := (Screen.Height - MemFillForm.Height) div 2;
  end;
  MemFillForm.Hex := MemFillFormHex;
  MemFillForm.MemSize := ArchiveSize;
  MemFillForm.Value := MemFillFormValue;
  MemFillForm.ValueType := MemFillFormValueType;
  MemFillForm.FromAddress := MemFillFormFromAddress;
  MemFillForm.ToAddress := MemFillFormToAddress;

  if MemFillForm.ShowModal = mrOk then begin
    MemFillFormValue := MemFillForm.Value;
    MemFillFormHex := MemFillForm.Hex;
    MemFillFormValueType := MemFillForm.ValueType;
    MemFillFormFromAddress := MemFillForm.FromAddress;
    MemFillFormToAddress := MemFillForm.ToAddress;
    F := True;
  end;

  MemFillFormLeft := MemFillForm.Left;
  MemFillFormTop := MemFillForm.Top;
  MemFillFormShown := True;
  MemFillForm.Free;

  if F then begin
    for I := MemFillFormFromAddress to MemFillFormToAddress - 1 do
      Device.FlashBytes[I] := MemFillFormValue;
    MemDrawGrid.Invalidate;
  end;
end;

procedure TArchiveMemForm.HexBtnClick(Sender: TObject);
begin
  MemDrawGrid.Invalidate;
end;

procedure TArchiveMemForm.JumpActionExecute(Sender: TObject);
var
  F: Boolean;
begin
  F := False;

  MemJumpForm := TMemJumpForm.Create(Self);
  if MemJumpFormShown then begin
    MemJumpForm.Left := MemJumpFormLeft;
    MemJumpForm.Top := MemJumpFormTop;
  end
  else begin
    MemJumpForm.Left := (Screen.Width - MemJumpForm.Width) div 2;
    MemJumpForm.Top := (Screen.Height - MemJumpForm.Height) div 2;
    MemJumpFormHex := True;
    MemJumpFormAddress := 0;
  end;
  MemJumpForm.MemSize := ArchiveSize;
  MemJumpForm.Hex := MemJumpFormHex;
  MemJumpForm.Address := MemJumpFormAddress;

  if MemJumpForm.ShowModal = mrOk then begin
    F := True;
    MemJumpFormHex := MemJumpForm.Hex;
    MemJumpFormAddress := MemJumpForm.Address;
  end;

  MemJumpFormTop := MemJumpForm.Top;
  MemJumpFormLeft := MemJumpForm.Left;
  MemJumpFormShown := True;
  MemJumpForm.Free;

  if F then begin
    MemDrawGrid.Row := 1 + MemJumpFormAddress div 8;
    MemDrawGrid.Col := 1 + MemJumpFormAddress mod 8;
  end;
end;

end.

