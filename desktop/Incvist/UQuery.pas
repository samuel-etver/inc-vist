unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, EditBtn, CheckLst, UGlobal, UTypes, UDeviceData, lazutf8;

type

  { TQueryForm }

  TQueryForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CalendarCheckBox: TCheckBox;
    IncDiameterCheckBox: TCheckBox;
    IncDiameterFromEdit: TEdit;
    IncDiameterFromLbl: TLabel;
    IncDiameterFromUnitsLbl: TLabel;
    IncDiameterToEdit: TEdit;
    IncDiameterToLbl: TLabel;
    IncDiameterToUnitsLbl: TLabel;
    IncLenCheckBox: TCheckBox;
    IncLenFromEdit: TEdit;
    IncLenFromLbl: TLabel;
    IncLenFromUnitsLbl: TLabel;
    IncLenToEdit: TEdit;
    IncLenToLbl: TLabel;
    IncLenToUnitsLbl: TLabel;
    IncPanel: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    VistObjCheckBox: TCheckBox;
    VistObjListBox: TCheckListBox;
    VistPanel: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    DateFromEdit: TDateEdit;
    DateLbl: TLabel;
    TimeLbl: TLabel;
    DateToEdit: TDateEdit;
    DateFromLbl: TLabel;
    DateToLbl: TLabel;
    TimeFromLbl: TLabel;
    TimeToLbl: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    QueryPageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    IncTabSheet: TTabSheet;
    VistTabSheet: TTabSheet;
    TimeFromEdit: TTimeEdit;
    TimeToEdit: TTimeEdit;
    VistTypCheckBox: TCheckBox;
    VistTypListBox: TCheckListBox;
    procedure CalendarCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IncDiameterCheckBoxChange(Sender: TObject);
    procedure IncLenCheckBoxChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure VistObjCheckBoxChange(Sender: TObject);
    procedure VistTypCheckBoxChange(Sender: TObject);
  private
    { private declarations }
    FDevDataType: TDevDataType;
    FVistTypStrArr: array[0..VistTypCount-1] of UTF8String;
    FVistTypIndArr: array[0..VistTypCount-1] of Integer;
    FVistTypCheckedArr: array[0..VistTypCount-1] of Boolean;
    FVistObjStrArr: array[0..VistObjCount-1] of UTF8String;
    FVistObjIndArr: array[0..VistObjCount-1] of Integer;
    FVistObjCheckedArr: array[0..VistObjCount-1] of Boolean;
    procedure LanguageChanged;
    function GetInactiveColor: TColor;
    function GetEditColor(AnEnabled: Boolean): TColor;
    procedure SetDevDataType(NewValue: TDevDataType);
  public
    { public declarations }
    class function Execute(Ddt: TDevDataType): Boolean;
    property DevDataType: TDevDataType read FDevDataType write SetDevDataType;
  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.lfm}

{ TQueryForm }

procedure TQueryForm.FormCreate(Sender: TObject);
var
  Clr: TColor;
  I: Integer;
begin
  Clr := GetDefaultColor(dctBrush);
  IncPanel.Color := Clr;
  VistPanel.Color := Clr;

  FVistTypStrArr[0] := Gti(1174);
  FVistTypStrArr[1] := Gti(1175);
  FVistTypStrArr[2] := Gti(1189);
  for I := 0 to VistTypCount - 1 do
  begin
    FVistTypIndArr[I]     := I;
    FVistTypCheckedArr[I] := IsVistTypChecked(I + VistTypFirstId);
  end;
  FVistObjStrArr[0] := Gti(1176);
  FVistObjStrArr[1] := Gti(1177);
  FVistObjIndArr[0] := 0;
  FVistObjIndArr[1] := 1;
  FVistObjCheckedArr[0] := IsVistObjChecked(VistObjGeneralId);
  FVistObjCheckedArr[1] := IsVistObjChecked(VistObjShakerTableId);

  LanguageChanged;

  CalendarCheckBox.Checked := CalendarFilterEnabled;
  if Assigned(DateFrom) then
    DateFromEdit.Date := DateFrom.Value;
  if Assigned(DateTo) then
    DateToEdit.Date := DateTo.Value;
  if Assigned(TimeFrom) then
    TimeFromEdit.Time := TimeFrom.Value;
  if Assigned(TimeTo) then
    TimeToEdit.Time := TimeTo.Value;

  IncLenCheckBox.Checked := IncLenFilterEnabled;
  IncLenFromEdit.Text := TWFloat32.ToStr(IncLenFrom, 15, 0);
  IncLenToEdit.Text := TWFloat32.ToStr(IncLenTo, 15, 0);

  IncDiameterCheckBox.Checked := IncDiameterFilterEnabled;
  IncDiameterFromEdit.Text := TWFloat32.ToStr(IncDiameterFrom, 15, 0);
  IncDiameterToEdit.Text := TWFloat32.ToStr(IncDiameterTo, 15, 0);

  VistObjCheckBox.Checked := VistObjFilterEnabled;
  VistTypCheckBox.Checked := VistTypFilterEnabled;

  CalendarCheckBoxChange(nil);
  IncLenCheckBoxChange(nil);
  IncDiameterCheckBoxChange(nil);
  VistObjCheckBoxChange(nil);
  VistTypCheckBoxChange(nil);
end;

procedure TQueryForm.FormShow(Sender: TObject);
const
  Ofs = 6;
var
  X: Integer;
begin
  X := DateFromEdit.Left - Ofs;
  DateFromLbl.Left := X - DateFromLbl.Width;
  DateToLbl.Left   := X - DateToLbl.Width;
  X := TimeFromEdit.Left - Ofs;
  TimeFromLbl.Left := X - TimeFromLbl.Width;
  TimeToLbl.Left   := X - TimeToLbl.Width;
end;

procedure TQueryForm.SetDevDataType(NewValue: TDevDataType);
begin
  FDevDataType := NewValue;
  case FDevDataType of
    ddtInc:  QueryPageControl.PageIndex := 0;
    ddtVist: QueryPageControl.PageIndex := 1;
  end;
end;

procedure TQueryForm.IncDiameterCheckBoxChange(Sender: TObject);
var
  Checked: Boolean;
  Clr: TColor;
begin
  Checked := IncDiameterCheckBox.Checked;
  Clr := GetEditColor(Checked);
  IncDiameterFromEdit.Enabled := Checked;
  IncDiameterToEdit.Enabled := Checked;
  IncDiameterFromEdit.Color := Clr;
  IncDiameterToEdit.Color := Clr;
end;

procedure TQueryForm.IncLenCheckBoxChange(Sender: TObject);
var
  Checked: Boolean;
  Clr: TColor;
begin
  Checked := IncLenCheckBox.Checked;
  Clr := GetEditColor(Checked);
  IncLenFromEdit.Enabled := Checked;
  IncLenToEdit.Enabled := Checked;
  IncLenFromEdit.Color := Clr;
  IncLenToEdit.Color := Clr;
end;

function TQueryForm.GetInactiveColor: TColor;
begin
  Result := Color;
  if Result = clDefault then
    Result := GetDefaultColor(dctBrush);
  Result := ColorToRGB(Result);
end;

function TQueryForm.GetEditColor(AnEnabled: Boolean): TColor;
begin
  if AnEnabled
    then Result := clDefault
    else Result := GetInactiveColor;
end;

procedure TQueryForm.CalendarCheckBoxChange(Sender: TObject);
var
  Checked: Boolean;
  Clr: TColor;
begin
  Checked := CalendarCheckBox.Checked;

  DateFromEdit.Enabled := Checked;
  DateToEdit.Enabled := Checked;
  TimeFromEdit.Enabled := Checked;
  TimeToEdit.Enabled := Checked;

  Clr := GetEditColor(Checked);
  DateFromEdit.Color := Clr;
  DateToEdit.Color := Clr;
  TimeFromEdit.Color := Clr;
  TimeToEdit.Color := Clr;
end;

procedure TQueryForm.OkBtnClick(Sender: TObject);
var
  Txt: String;
  DtTm: TDateTime;
  A: array[0..1,0..1] of Pointer;
  I: Integer;
  DateEdit: TDateEdit;
  TimeEdit: TTimeEdit;
  PDt: ^TWDate;
  PTm: ^TWTime;
  Index: Integer;
  Checked: Boolean;
  CheckedCount: Integer;
begin
  CalendarFilterEnabled := CalendarCheckBox.Checked;
  IncLenFilterEnabled := IncLenCheckBox.Checked;
  IncDiameterFilterEnabled := IncDiameterCheckBox.Checked;
  VistObjFilterEnabled := VistObjCheckBox.Checked;
  VistTypFilterEnabled := VistTypCheckBox.Checked;

  A[0][0] := DateFromEdit;
  A[0][1] := @DateFrom;
  A[1][0] := DateToEdit;
  A[1][1] := @DateTo;

  for I := 0 to 1 do
  begin
    DateEdit := TDateEdit(A[I][0]);
    PDt := A[I][1];
    Txt := UTF8Trim(DateEdit.Text);
    try
      DtTm := StrToDate(Txt);
      if not Assigned(PDt^) then
        PDt^ := TWDate.Create;
      PDt^.Value := DtTm;
    except
      if Assigned(PDt^) then
      begin
        PDt^.Free;
        PDt^ := nil;
      end;
    end;
  end;


  A[0][0] := TimeFromEdit;
  A[0][1] := @TimeFrom;
  A[1][0] := TimeToEdit;
  A[1][1] := @TimeTo;

  for I := 0 to 1 do
  begin
    TimeEdit := TTimeEdit(A[I][0]);
    PTm := A[I][1];
    Txt := UTF8Trim(TimeEdit.Text);
    try
      DtTm := StrToTime(Txt);
      if not Assigned(PTm^) then
        PTm^ := TWTime.Create;
      PTm^.Value := DtTm;
    except
      if Assigned(PTm^) then
      begin
        PTm^.Free;
        PTm^ := nil;
      end;
    end;
  end;

  if Assigned(IncLenFrom) then
  begin
    IncLenFrom.Free;
    IncLenFrom := nil;
  end;
  try
    IncLenFrom := TWFloat32.Parse(UTF8Trim(IncLenFromEdit.Text));
  except
  end;

  if Assigned(IncLenTo) then
  begin
    IncLenTo.Free;
    IncLenTo := nil;
  end;
  try
    IncLenTo := TWFloat32.Parse(UTF8Trim(IncLenToEdit.Text));
  except
  end;

  if Assigned(IncDiameterFrom) then
  begin
    IncDiameterFrom.Free;
    IncDiameterFrom := nil;
  end;
  try
    IncDiameterFrom := TWFloat32.Parse(UTF8Trim(IncDiameterFromEdit.Text));
  except
  end;

  if Assigned(IncDiameterTo) then
  begin
    IncDiameterTo.Free;
    IncDiameterTo := nil;
  end;
  try
    IncDiameterTo := TWFloat32.Parse(UTF8Trim(IncDiameterToEdit.Text));
  except
  end;

  CheckedCount := 0;
  for I := 0 to VistTypCount - 1 do
  begin
    Index := FVistTypIndArr[I];
    Checked := VistTypListBox.Checked[I];
    FVistTypCheckedArr[Index] := Checked;
    if Checked then
      Inc(CheckedCount);
  end;
  SetLength(VistTypCheckedList, CheckedCount);
  Index := Low(VistTypCheckedList);
  for I := 0 to VistTypCount - 1 do
  begin
    if FVistTypCheckedArr[I] then
    begin
      VistTypCheckedList[Index] := I + VistTypFirstId;
      Inc(Index);
    end;
  end;

  CheckedCount := 0;
  for I := 0 to VistObjCount - 1 do
  begin
    Index := FVistObjIndArr[I];
    Checked := VistObjListbox.Checked[I];
    FVistObjCheckedArr[Index] := Checked;
    if Checked then
      Inc(CheckedCount);
  end;
  SetLength(VistObjCheckedList, CheckedCount);
  Index := Low(VistObjCheckedList);
  if FVistObjCheckedArr[0] then
  begin
    VistObjCheckedList[Index] := VistObjGeneralId;
    Inc(Index);
  end;
  if FVistObjCheckedArr[1] then
  begin
    VistObjCheckedList[Index] := VistObjShakerTableId;
    Inc(Index);
  end;

  Save;
end;

procedure TQueryForm.VistObjCheckBoxChange(Sender: TObject);
var
  Checked: Boolean;
begin
  Checked := VistObjCheckBox.Checked;
  VistObjListBox.Enabled := Checked;
  VistObjListBox.Color := GetEditColor(Checked);
end;

procedure TQueryForm.VistTypCheckBoxChange(Sender: TObject);
var
  Checked: Boolean;
begin
  Checked := VistTypCheckBox.Checked;
  VistTypListBox.Enabled := Checked;
  VistTypListBox.Color := GetEditColor(Checked);
end;

procedure TQueryForm.LanguageChanged;
var
  I,J: Integer;
  Index: Integer;
  IndexI: Integer;
  IndexJ: Integer;
begin
  Caption := Gti(2000);
  OkBtn.Caption := Gti(1000);
  CancelBtn.Caption := Gti(1001);
  CalendarCheckBox.Caption := Gti(2002);
  TimeFromLbl.Caption := Gti(2005);
  TimeToLbl.Caption := Gti(2006);
  DateFromLbl.Caption := Gti(2003);
  DateToLbl.Caption := Gti(2004);
  DateLbl.Caption := Gti(2007);
  TimeLbl.Caption := Gti(2008);
  IncTabSheet.Caption := Gti(1155);
  VistTabSheet.Caption := Gti(1156);
  IncDiameterCheckBox.Caption := Gti(2009);
  IncDiameterFromLbl.Caption := Gti(2011);
  IncDiameterToLbl.Caption := Gti(2012);
  IncDiameterFromUnitsLbl.Caption := Gti(2014);
  IncDiameterToUnitsLbl.Caption := Gti(2014);
  IncLenCheckBox.Caption := Gti(2010);
  IncLenFromLbl.Caption := Gti(2011);
  IncLenToLbl.Caption := Gti(2012);
  IncLenFromUnitsLbl.Caption := Gti(2013);
  IncLenToUnitsLbl.Caption := Gti(2013);
  VistObjCheckBox.Caption := Gti(2015);
  VistTypCheckBox.Caption := Gti(2016);

  with VistTypListBox do
  begin
    for I := 0 to VistTypListBox.Count - 1 do
    begin
      Index := FVistTypIndArr[I];
      FVistTypCheckedArr[Index] := Checked[I];
    end;

    Clear;

    {for I := 0 to VistTypCount - 2 do
    begin
      for J := I + 1 to VistTypCount - 1 do
      begin
        IndexI := FVistTypIndArr[I];
        IndexJ := FVistTypIndArr[J];
        if UTF8CompareText(FVistTypStrArr[IndexI],
           FVistTypStrArr[IndexJ]) > 0 then
        begin
          FVistTypIndArr[I] := IndexJ;
          FVistTypIndArr[J] := IndexI;
        end;
      end;
    end;}

    for I := 0 to VistTypCount - 1 do
    begin
      Index := FVistTypIndArr[I];
      VistTypListBox.Items.Add(FVistTypStrArr[Index]);
      VistTypListBox.Checked[I] := FVistTypCheckedArr[Index];
    end;
  end;

  with VistObjListBox do
  begin
    for I := 0 to VistObjListBox.Count - 1 do
    begin
      Index := FVistObjIndArr[I];
      FVistObjCheckedArr[Index] := Checked[I];
    end;

    Clear;

    for I := 0 to VistObjCount - 2 do
    begin
      for J := I + 1 to VistObjCount - 1 do
      begin
        IndexI := FVistObjIndArr[I];
        IndexJ := FVistObjIndArr[J];
        if UTF8CompareText(FVistObjStrArr[IndexI],
           FVistObjStrArr[IndexJ]) > 0 then
        begin
          FVistObjIndArr[I] := IndexJ;
          FVistObjIndArr[J] := IndexI;
        end;
      end;
    end;

    for I := 0 to VistObjCount - 1 do
    begin
      Index := FVistObjIndArr[I];
      VistObjListbox.Items.Add(FVistObjStrArr[Index]);
      VistObjListBox.Checked[I] := FVistObjCheckedArr[Index];
    end;
  end;
end;

class function TQueryForm.Execute(Ddt: TDevDataType): Boolean;
begin
  QueryForm := TQueryForm.Create(Application);
  QueryForm.DevDataType := Ddt;
  Result := QueryForm.ShowModal = mrOk;
  QueryForm.Free;
end;


end.

