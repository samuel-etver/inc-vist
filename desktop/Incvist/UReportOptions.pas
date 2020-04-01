unit UReportOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, Buttons, UGlobal, UDeviceData, lazUTF8;

type

  { TReportOptionsForm }

  TReportOptionsForm = class(TForm)
    AllCheckBox: TCheckBox;
    HintLbl: TLabel;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    DefaultBtn: TButton;
    DontShowCheckBox: TCheckBox;
    OptionsDrawGrid: TDrawGrid;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure DefaultBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OptionsDrawGridCheckboxToggled({%H-}sender: TObject; {%H-}aCol,
      {%H-}aRow: Integer; {%H-}aState: TCheckboxState);
    procedure OptionsDrawGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure OptionsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure OptionsDrawGridEditingDone(Sender: TObject);
    procedure OptionsDrawGridGetCheckboxState(Sender: TObject; {%H-}ACol,
      ARow: Integer; var Value: TCheckboxState);
    procedure OptionsDrawGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure OptionsDrawGridSetCheckboxState(Sender: TObject; {%H-}ACol,
      ARow: Integer; const {%H-}Value: TCheckboxState);
    procedure OptionsDrawGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure UpBtnClick(Sender: TObject);
  private
    { private declarations }
    FDevDataType: TDevDataType;
    FColumns: TList;
    FEditedCol: Integer;
    FEditedRow: Integer;
    FEditedText: String;
    procedure LanguageChanged;
  public
    { public declarations }
    class function Execute(Ddt: TDevDataType): Boolean;
    class function Execute(AOwner: TComponent; Ddt: TDevDataType): Boolean;
    property DevDataType: TDevDataType read FDevDataType write FDevDataType;
  end;

var
  ReportOptionsForm: TReportOptionsForm;

implementation

{$R *.lfm}

{ TReportOptionsForm }

procedure TReportOptionsForm.FormCreate(Sender: TObject);
begin
  FColumns := TList.Create;
  FDevDataType := ddtNone;
  OptionsDrawGrid.AlternateColor := UGlobal.AlternateColor;
  OptionsDrawGrid.ColWidths[0] := 32;
  DontShowCheckBox.Checked := DontShowReportOptions;
  FEditedText := '';
  FEditedCol := 0;
  FEditedRow := 0;
  LanguageChanged;
end;

procedure TReportOptionsForm.DownBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := OptionsDrawGrid.Row - 1;
  if (Index >= 0) and (Index < FColumns.Count - 1) then
  begin
    FColumns.Exchange(Index, Index + 1);
    OptionsDrawGrid.Row := Index + 2;
    OptionsDrawGrid.Invalidate;
  end;
end;

procedure TReportOptionsForm.DefaultBtnClick(Sender: TObject);
begin
  AllCheckBox.Checked := True;
  case FDevDataType of
    ddtInc:  IncSetReportColumnsDef(FColumns);
    ddtVist: VistSetReportColumnsDef(FColumns);
  end;
  OptionsDrawGrid.Invalidate;
end;

procedure TReportOptionsForm.FormDestroy(Sender: TObject);
var
  I: Integer;
  PColumn: ^TReportColumn;
begin
  for I := FColumns.Count - 1 downto 0 do
  begin
    PColumn := FColumns.Items[I];
    Dispose(PColumn);
  end;
  FColumns.Free;
end;

procedure TReportOptionsForm.FormShow(Sender: TObject);
var
  I: Integer;
  TempColumns: TList = nil;
  RowCount: Integer = 0;
  PNewColumn: ^TReportColumn;
  PCurColumn: ^TReportColumn;
  AllChecked: Boolean = True;
begin
  case FDevDataType of
    ddtInc:
    begin
      RowCount := IncColumnCount + 1;
      TempColumns := IncReportColumns;
      AllChecked := IncReportAllFields;
    end;

    ddtVist:
    begin
      RowCount := VistColumnCount + 1;
      TempColumns := VistReportColumns;
      AllChecked := VistReportAllFields;
    end;
  end;

  for I := 0 to RowCount - 2 do
  begin
    PCurColumn := TempColumns.Items[I];
    New(PNewColumn);
    PNewColumn^ := PCurColumn^;
    FColumns.Add(PNewColumn);
  end;
  OptionsDrawGrid.RowCount := RowCount;
  AllCheckBox.Checked := AllChecked;
end;

procedure TReportOptionsForm.OkBtnClick(Sender: TObject);
var
  I: Integer;
  TempColumns: TList = nil;
  PSrcColumn: ^TReportColumn;
  PDstColumn: ^TReportColumn;
  AllChecked: Boolean;
begin
  DontShowReportOptions := DontShowCheckBox.Checked;
  AllChecked := AllCheckBox.Checked;

  case FDevDataType of
    ddtInc:
      begin
        TempColumns := IncReportColumns;
        IncReportAllFields := AllChecked;
      end;
    ddtVist:
      begin
        TempColumns := VistReportColumns;
        VistReportAllFields := AllChecked;
      end;
  end;

  if Assigned(TempColumns) then
  begin
    for I := TempColumns.Count - 1 downto 0 do
    begin
      PDstColumn := TempColumns.Items[I];
      PSrcColumn := FColumns.Items[I];
      PDstColumn^ := PSrcColumn^;
    end;
  end;

  Save;
end;

procedure TReportOptionsForm.OptionsDrawGridCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
begin
  if AllCheckBox.Checked then
    AllCheckBox.Checked := False;
end;

procedure TReportOptionsForm.OptionsDrawGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
  begin
    FColumns.Exchange(sIndex - 1, tIndex - 1);
    OptionsDrawGrid.Invalidate;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
const
  OffsX = 8;
var
  Txt: UTF8String;
  Y: Integer;
  TxtAlign: Integer;
  PColumn: ^TReportColumn;
  Index: Integer;
begin
  if (ACol + ARow) = 0 then
    Exit;

  with (Sender as TDrawGrid).Canvas do
  begin
    Y := (aRect.Top + aRect.Bottom - TextHeight('Wg')) div 2;
    TxtAlign := 1;

    if ACol = 0 then
    begin
      Txt := IntToStr(ARow);
    end
    else if ARow = 0 then
    begin
    end
    else
    begin
      if FColumns.Count < ARow then
        Exit;
      PColumn := FColumns.Items[ARow - 1];
      case ACol of
        1: Exit;
        2:
        begin
          Index := PColumn^.Id;
          case FDevDataType of
            ddtInc:
            begin
              if Index = 11
                then Txt := Gti(IncColumnIds[Index]) + SigmaUnitsToStr(SigmaUnits)
                else Txt := Gti(IncColumnIds[Index]);
            end;
            ddtVist:
            begin
              Txt := Gti(VistColumnIds[Index]);
            end
            else     Txt := '';
          end;
          TxtAlign := -1;
        end;
        3: Txt := IntToStr(PColumn^.Width);
        else
          Txt := '';
      end;
    end;


    case TxtAlign of
      -1: TextRect(aRect, aRect.Left + OffsX, Y, Txt);
      0:  TextRect(aRect, (aRect.Left + aRect.Right - TextWidth(Txt)) div 2, Y, Txt);
      1:  TextRect(aRect, aRect.Right - OffsX - TextWidth(Txt), Y, Txt);
    end;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridEditingDone(Sender: TObject);
var
  Index: Integer;
  PColumn: ^TReportColumn;
begin
  if FEditedCol = 3 then
  begin
    Index := FEditedRow - 1;
    if (Index >= 0) and (Index < FColumns.Count) then
    begin
      PColumn := FColumns.Items[Index];
      try
        PColumn^.Width := Round(StrToFloat(UTF8Trim(FEditedText)));
      except
      end;
    end;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridGetCheckboxState(Sender: TObject;
  ACol, ARow: Integer; var Value: TCheckboxState);
var
  PColumn: ^TReportColumn;
begin
  Value := cbUnchecked;
  if (ARow >= 1) and (ARow <= FColumns.Count) then
  begin
    PColumn := FColumns.Items[ARow - 1];
    if PColumn^.Checked then
      Value := cbChecked;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
var
  Index: Integer;
  PColumn: ^TReportColumn;
begin
  FEditedCol := ACol;
  FEditedRow := ARow;

  if ACol = 3 then
  begin
    Index := ARow - 1;
    if (Index >= 0) and (Index < FColumns.Count) then
    begin
      PColumn := FColumns.Items[Index];
      Value := IntToStr(PColumn^.Width);
      FEditedText := Value;
    end;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridSetCheckboxState(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckboxState);
var
  PColumn: ^TReportColumn;
begin
  if (ARow >= 1) and (ARow <= FColumns.Count) then
  begin
    PColumn := FColumns.Items[ARow - 1];
    PColumn^.Checked := not PColumn^.Checked;
  end;
end;

procedure TReportOptionsForm.OptionsDrawGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  FEditedText := Value;
  FEditedCol := ACol;
  FEditedRow := ARow;
end;

procedure TReportOptionsForm.UpBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := OptionsDrawGrid.Row - 1;
  if Index > 0 then
  begin
    FColumns.Exchange(Index - 1, Index);
    OptionsDrawGrid.Row := Index;
    OptionsDrawGrid.Invalidate;
  end;
end;

procedure TReportOptionsForm.LanguageChanged;
begin
  Caption := Gti(3000);
  OkBtn.Caption := Gti(1000);
  CancelBtn.Caption := Gti(1001);
  AllCheckBox.Caption := Gti(3001);
  DontShowCheckBox.Caption := Gti(3011);
  OptionsDrawGrid.Columns[1].Title.Caption := Gti(3003);
  OptionsDrawGrid.Columns[2].Title.Caption := Gti(3002);
  UpBtn.Caption := Gti(3004);
  UpBtn.Hint := Gti(3008);
  DownBtn.Caption := Gti(3005);
  DownBtn.Hint := Gti(3009);
  DefaultBtn.Caption := Gti(3006);
  DefaultBtn.Hint := Gti(3007);
  HintLbl.Caption := Gti(3010);
end;

class function TReportOptionsForm.Execute(Ddt: TDevDataType): Boolean;
begin
  Result := Execute(Application, Ddt);
end;

class function TReportOptionsForm.Execute(AOwner: TComponent;
 Ddt: TDevDataType): Boolean;
begin
  ReportOptionsForm := TReportOptionsForm.Create(AOwner);
  ReportOptionsForm.DevDataType := Ddt;
  Result := ReportOptionsForm.ShowModal = mrOk;
  ReportOptionsForm.Free;
end;

end.

