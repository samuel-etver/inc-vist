unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, UGlobal, UDeviceData, {ActiveX,} Windows, LazUtf8,
  Variants, ComObj, UTypes, fpspreadsheet, xlsbiff8, fpstypes;

type
  TExportCloseReason = (
    ecrError, ecrUserAbort, ecrSuccess, ecrUnknown
  );

  { TExportThread }
  TExportThread = class(TThread)
  private
    FDevDataType: TDevDataType;
    FProgress: Integer;
    //FExcel: OLEVariant;
    FError: Boolean;
    FWb: TsWorkbook;
    FSh: TsWorksheet;
    FFileName: UTF8String;
    {procedure OleExport;
    procedure OleExportDescription;
    procedure OleExportInc;
    procedure OleExportVist;}
    procedure FileExport;
    procedure FileExportDescription;
    procedure FileExportInc;
    procedure FileExportVist;
    function ToVar(Value: TWInt): Variant;
    function ToVar(Value: TWFloat32): Variant;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property DevDataType: TDevDataType read FDevDataType write FDevDataType;
    property Progress: Integer read FProgress write FProgress;
    property Error: Boolean read FError;
    property FileName: UTF8String read FFileName write FFileName;
  end;

  { TExportForm }
  TExportForm = class(TForm)
    CancelBtn: TButton;
    ExportProgressBar: TProgressBar;
    UpdateTimer: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    { private declarations }
    FExportThread: TExportThread;
    FProgress: Integer;
    FCloseReason: TExportCloseReason;
    procedure LanguageChanged;
    function GetDevDataType: TDevDataType;
    procedure SetDevDataType(Value: TDevDataType);
    function GetFileName: UTF8String;
    procedure SetFileName(NewFileName: UTF8String);
  public
    { public declarations }
    property DevDataType: TDevDataType read GetDevDataType write SetDevDataType;
    property CloseReason: TExportCloseReason read FCloseReason;
    property FileName: UTF8String read GetFileName write SetFileName;
  end;

var
  ExportForm: TExportForm;

implementation

{const
  XlCenter = -4108;
  XlLeft = -4131;
  XlRight = -4152;}

{$R *.lfm}

{ TExportThread }
constructor TExportThread.Create;
begin
  inherited Create(True);
  FDevDataType := ddtNone;
  FProgress := 0;
  FError := False;
end;

procedure TExportThread.Execute;
begin
  // OleExport;
   Synchronize(@FileExport);
end;

{procedure TExportThread.OleExport;
begin
   if OleInitialize(nil) <> S_OK then
     Exit;

   FExcel := CreateOleObject('Excel.Application');

  try
    FExcel.Visible := True;
    FExcel.Workbooks.Add(1);

    case FDevDataType of
      ddtDescription: OleExportDescription;
      ddtInc:         OleExportInc;
      ddtVist:        OleExportVist;
    end;
  except
    on e: Exception do
    begin
      FError := True;
      LogFile.Write(e.ToString());
    end;
  end;

  FExcel := Unassigned;
  OleUninitialize;
end;

procedure TExportThread.OleExportDescription;
begin
  with DevQuery.Description do
  begin
    FExcel.Columns[1].ColumnWidth := 80;
    if Assigned(Text) then
      FExcel.Cells[1, 1] := WideString(Text.Value);
    FProgress := 100;
  end;
end;

procedure TExportThread.OleExportInc;
var
  I,N: Integer;
  It: TIncItem;
  Da: Variant;
  RowTxt: String;
  Row: Integer;
  ArrLen: Integer;
  LastCol: String;
begin
  with DevQuery.IncData do
  begin
    N := Count;
    ArrLen := 11;
    Da := VarArrayCreate([1, ArrLen], varVariant);
    LastCol := Chr(Ord('A') - 1 + ArrLen);

    FExcel.Columns[1].HorizontalAlignment := XlCenter;

    Da[1] := WideString(Gti( 150));
    Da[2] := WideString(Gti( 151));
    Da[3] := WideString(Gti( 152));
    Da[4] := WideString(Gti(1157));
    Da[5] := WideString(Gti(1158));
    Da[6] := WideString(Gti(1159));
    Da[7] := WideString(Gti(1160));
    Da[8] := WideString(Gti(1161));
    Da[9] := WideString(Gti(1163));
    Da[10] := WideString(Gti(1162));
    Da[11] := WideString(Gti(1166) +
     SigmaUnitsToStr(SigmaUnits));
    FExcel.Range[WideString('A1:'+ LastCol + '1')] := Da;

    for I := 1 to ArrLen do
    begin
      FExcel.Columns[I].HorizontalAlignment := XlRight;
      FExcel.Cells[1, I].HorizontalAlignment := XlLeft;
    end;

    FExcel.Columns[1].ColumnWidth := 10;
    FExcel.Columns[2].ColumnWidth := 10;
    FExcel.Columns[3].ColumnWidth := 8;
    FExcel.Columns[4].ColumnWidth := 13;
    FExcel.Columns[5].ColumnWidth := 11;
    FExcel.Columns[6].ColumnWidth := 8;
    FExcel.Columns[7].ColumnWidth := 8;
    FExcel.Columns[8].ColumnWidth := 8;
    FExcel.Columns[9].ColumnWidth := 8;
    FExcel.Columns[10].ColumnWidth := 8;

    FExcel.Columns[6].NumberFormat :=
     WideString('0' + DefaultFormatSettings.DecimalSeparator + '000');
    FExcel.Columns[7].NumberFormat :=
     WideString('0' + DefaultFormatSettings.DecimalSeparator + '000');
    FExcel.Columns[8].NumberFormat :=
     WideString('0');
    FExcel.Columns[9].NumberFormat :=
     WideString('0' + DefaultFormatSettings.DecimalSeparator + '0');
    FExcel.Columns[10].NumberFormat :=
     WideString('0' + DefaultFormatSettings.DecimalSeparator + '000');
    FExcel.Columns[11].NumberFormat :=
     WideString('0' + DefaultFormatSettings.DecimalSeparator + '00');

    Row := 2;
    for I := 1 to N do
    begin
      It := Items[I - 1];

      Da[1] := WideString(DateToStr(It));
      Da[2] := WideString(TimeToStr(It));
      Da[3] := ToVar(It.Number);
      Da[4] := ToVar(It.Diameter);
      Da[5] := ToVar(It.Len);
      Da[6] := ToVar(It.MeasureT);
      Da[7] := ToVar(It.MeasureF);
      Da[8] := ToVar(It.Noise);
      Da[9] := ToVar(It.Epsilon);
      Da[10] := ToVar(It.DeltaL);
      Da[11] := ToVar(It.Sigma);

      RowTxt := IntToStr(Row);
      FExcel.Range[WideString('A' + RowTxt + ':' + LastCol + RowTxt)] := Da;

      FProgress := MulDiv(100, I, N);

      Inc(Row);
    end;
  end;
end;

procedure TExportThread.OleExportVist;
var
  I,N: Integer;
  It: TVistItem;
  Da: Variant;
  RowTxt: String;
  Row: Integer;
  ArrLen: Integer;
  LastCol: String;
  Nf: WideString;
begin
  with DevQuery.VistData do
  begin
    N := Count;
    ArrLen := 13;
    Da := VarArrayCreate([1, ArrLen], varVariant);
    LastCol := Chr(Ord('A') - 1 + ArrLen);

    FExcel.Columns[1].HorizontalAlignment := XlCenter;

    Da[1] := WideString(Gti( 150));
    Da[2] := WideString(Gti( 151));
    Da[3] := WideString(Gti( 152));
    Da[4] := WideString(Gti(1159));
    Da[5] := WideString(Gti(1160));
    Da[6] := WideString(Gti(1161));
    Da[7] := WideString(Gti(1168));
    Da[8] := WideString(Gti(1170));
    Da[9] := WideString(Gti(1193));
    Da[10] := WideString(Gti(1169));
    Da[11] := WideString(Gti(1192));
    Da[12] := WideString(Gti(1190));
    Da[13] := WideString(Gti(1191));
    FExcel.Range[WideString('A1:'+ LastCol + '1')] := Da;

    for I := 1 to ArrLen do
    begin
      case I of
        7:   FExcel.Columns[I].HorizontalAlignment := XlCenter;
        else FExcel.Columns[I].HorizontalAlignment := XlRight;
      end;
      FExcel.Cells[1, I].HorizontalAlignment := XlLeft;
    end;

    FExcel.Columns[1].ColumnWidth := 10;
    FExcel.Columns[2].ColumnWidth := 10;
    FExcel.Columns[3].ColumnWidth := 8;
    FExcel.Columns[4].ColumnWidth := 8;
    FExcel.Columns[5].ColumnWidth := 8;
    FExcel.Columns[6].ColumnWidth := 8;
    FExcel.Columns[7].ColumnWidth := 16;
    FExcel.Columns[8].ColumnWidth := 11;
    FExcel.Columns[9].ColumnWidth := 11;
    FExcel.Columns[10].ColumnWidth := 11;
    FExcel.Columns[11].ColumnWidth := 11;
    FExcel.Columns[12].ColumnWidth := 11;
    FExcel.Columns[13].ColumnWidth := 11;

    Nf := WideString('0' + DefaultFormatSettings.DecimalSeparator + '000');
    FExcel.Columns[4].NumberFormat := Nf;
    FExcel.Columns[5].NumberFormat := Nf;
    FExcel.Columns[6].NumberFormat := WideString('0');
    FExcel.Columns[8].NumberFormat := Nf;
    FExcel.Columns[9].NumberFormat := Nf;
    FExcel.Columns[10].NumberFormat := Nf;
    FExcel.Columns[11].NumberFormat := Nf;
    FExcel.Columns[12].NumberFormat := Nf;
    FExcel.Columns[13].NumberFormat := Nf;

    Row := 2;
    for I := 1 to N do
    begin
      It := Items[I - 1];

      Da[1] := WideString(DateToStr(It));
      Da[2] := WideString(TimeToStr(It));
      Da[3] := ToVar(It.Number);
      Da[4] := ToVar(It.MeasureT);
      Da[5] := ToVar(It.MeasureF);
      Da[6] := ToVar(It.Noise);
      Da[7] := WideString(ObjToStr(It));
      Da[8] := ToVar(It.MeasureSrms);
      Da[9] := ToVar(It.MeasureSamp);
      Da[10] := ToVar(It.MeasureVrms);
      Da[11] := ToVar(It.MeasureVamp);
      Da[12] := ToVar(It.MeasureArms);
      Da[13] := ToVar(It.MeasureAamp);

      RowTxt := IntToStr(Row);
      FExcel.Range[WideString('A' + RowTxt + ':' + LastCol + RowTxt)] := Da;

      FProgress := MulDiv(100, I, N);

      Inc(Row);
    end;
  end;
end;}

procedure TExportThread.FileExport;
begin
  FWb := TsWorkbook.Create;

  try
    case FDevDataType of
      ddtDescription: FileExportDescription;
      ddtInc:         FileExportInc;
      ddtVist:        FileExportVist;
    end;
    FWb.WriteToFile(FFileName, sfExcel8, True);
  except
    on e: Exception do
    begin
      FError := True;
      LogFile.Write(e.ToString());
    end;
  end;

  FWb.Free;
end;

procedure TExportThread.FileExportDescription;
var
  StringList: TStringList;
  I, N: Integer;
  CellPtr: PCell;
begin
  FSh := FWb.AddWorksheet(Gti(154));
  with DevQuery.Description do
  begin
    StringList := TStringList.Create;
    if Assigned(Text) then
      StringList.Text := Text.Value;
    N := StringList.Count;
    for I := 0 to N - 1 do
    begin
      CellPtr := FSh.AddCell(I, 0);
      CellPtr^.ContentType := TCellContentType.cctUTF8String;
      CellPtr^.UTF8StringValue := StringList.Strings[I];
      FProgress := MulDiv(100, I + 1, N);
    end;
    FProgress := 100;
    if N <> 0 then
      FSh.WriteColWidth(0, 80, TsSizeUnits.suChars);
  end;
end;

procedure TExportThread.FileExportInc;
const
  ColsCount = 11;
  BasicNf = '0.000';
  ColIds: array[0..ColsCount-1] of Integer = (
    150,   151,  152, 1157, 1158,
    1159, 1160, 1161, 1163, 1162,
    1166
  );
  ColWidths: array[0..ColsCount-1] of Integer = (
    10, 10,  8, 13, 11,
     8,  8,  8,  8,  8,
    10
  );
  ColAlignments: array[0..ColsCount-1] of TsHorAlignment = (
    haLeft,  haLeft,  haRight, haRight, haRight,
    haRight, haRight, haRight, haRight, haRight,
    haRight
  );
  ColContentTypes: array[0..ColsCount-1] of TCellContentType = (
    cctUTF8String, cctUTF8String, cctNumber, cctNumber, cctNumber,
    cctNumber,     cctUTF8String, cctNumber, cctNumber, cctNumber,
    cctNumber
  );
  ColNumberFormats: array[0..ColsCount-1] of String = (
    '',      '',      '',      '',      '',
    BasicNf, BasicNf, BasicNf, BasicNf, BasicNf,
    BasicNf
  );
var
  I, N: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  CellPtr: PCell;
  It: TIncItem;
  Index: Integer;
  Txt: UTF8String;
  WFl32: TWFloat32;

function NewCell(V: Variant): PCell;
var
  Nf: String;
begin
  Inc(ColIndex);
  Inc(Index);

  Result := FSh.AddCell(RowIndex, ColIndex);
  CellPtr := Result;

  case VarType(V) of
    varByte,
    varSmallInt,
    varInteger,
    varSingle,
    varDouble:
    begin
      Result^.ContentType := TCellContentType.cctNumber;
      Result^.Numbervalue := V;
      Nf := ColNumberFormats[Index];
      if Length(Nf) > 0 then
        FSh.WriteNumberFormat(Result, TsNumberFormat.nfFixed, Nf);
    end;

    varString:
    begin
      Result^.ContentType := TCellContentType.cctUTF8String;
      Result^.UTF8StringValue := V;
    end;

    else
      Result^.ContentType := ColContentTypes[Index];
  end;

  FSh.WriteHorAlignment(Result, ColAlignments[Index]);
end;
begin
  WFl32 := TWFloat32.Create;

  FSh := FWb.AddWorksheet(Gti(1155));

  for I := 0 to ColsCount - 1 do
  begin
    ColIndex := I;
    CellPtr := FSh.AddCell(0, ColIndex);
    CellPtr^.ContentType := TCellContentType.cctUTF8String;
    Txt := Gti(ColIds[I]);
    if I = 10 then
      Txt := Txt + SigmaUnitsToStr(SigmaUnits);
    CellPtr^.UTF8StringValue := Txt;
    FSh.WriteHorAlignment(CellPtr, TsHorAlignment.haLeft);
    FSh.WriteColWidth(ColIndex, ColWidths[I], tsSizeUnits.suChars);
  end;

  with DevQuery.IncData do
  begin
    RowIndex := 1;
    N := Count - 1;
    for I := 0 to N do
    begin
      It := Items[I];
      ColIndex := -1;
      Index := -1;
      NewCell(DateToStr(It));
      NewCell(TimeToStr(It));
      NewCell(ToVar(It.Number));
      NewCell(ToVar(It.Diameter));
      NewCell(ToVar(It.Len));
      NewCell(ToVar(It.MeasureT));
      NewCell(ToVar(It.MeasureF));
      NewCell(ToVar(It.Noise));
      NewCell(ToVar(It.Epsilon));
      NewCell(ToVar(It.DeltaL));
      if not Assigned(It.Sigma) then
      begin
        NewCell(ToVar(TWFloat32(Nil)));
      end
      else
      begin
        WFl32.Value := SigmaGet(It.Sigma.Value, SigmaUnits);
        NewCell(ToVar(WFl32));
      end;
      Inc(RowIndex);
      FProgress := MulDiv(100, I + 1, N + 1);
    end;
    FProgress := 100;
  end;

  WFl32.Free;
end;

procedure TExportThread.FileExportVist;
const
  ColsCount = 10;
  BasicNf = '0.000';
  ColIds: array[0..ColsCount-1] of Integer = (
    150,   151,  152, 1159, 1160,
    1161, 1168, 1194, 1169, 1191
  );
  ColWidths: array[0..ColsCount-1] of Integer = (
    10, 10,  8,  8,  8,
     8, 16, 11, 11, 11
  );
  ColAlignments: array[0..ColsCount-1] of TsHorAlignment = (
    haLeft,  haLeft,  haRight, haRight, haRight,
    haRight, haLeft,  haRight, haRight, haRight
  );
  ColContentTypes: array[0..ColsCount-1] of TCellContentType = (
    cctUTF8String, cctUTF8String, cctNumber, cctNumber, cctNumber,
    cctNumber,     cctUTF8String, cctNumber, cctNumber, cctNumber
  );
  ColNumberFormats: array[0..ColsCount-1] of String = (
    '',      '',      '',  BasicNf, BasicNf,
    BasicNf, '',      '0', BasicNf, BasicNf
  );
var
  I, N: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  CellPtr: PCell;
  It: TVistItem;
  Index: Integer;

function NewCell(V: Variant): PCell;
var
  Nf: String;
begin
  Inc(ColIndex);
  Inc(Index);

  Result := FSh.AddCell(RowIndex, ColIndex);
  CellPtr := Result;

  case VarType(V) of
    varByte,
    varSmallInt,
    varInteger,
    varSingle,
    varDouble:
    begin
      Result^.ContentType := TCellContentType.cctNumber;
      Result^.Numbervalue := V;
      Nf := ColNumberFormats[Index];
      if Length(Nf) > 0 then
        FSh.WriteNumberFormat(Result, TsNumberFormat.nfFixed, Nf);
    end;

    varString:
    begin
      Result^.ContentType := TCellContentType.cctUTF8String;
      Result^.UTF8StringValue := V;
    end;

    else
      Result^.ContentType := ColContentTypes[Index];
  end;

  FSh.WriteHorAlignment(Result, ColAlignments[Index]);
end;

begin
  FSh := FWb.AddWorksheet(Gti(1156));

  for I := 0 to ColsCount - 1 do
  begin
    ColIndex := I;
    CellPtr := FSh.AddCell(0, ColIndex);
    CellPtr^.ContentType := TCellContentType.cctUTF8String;
    CellPtr^.UTF8StringValue := Gti(ColIds[I]);
    FSh.WriteHorAlignment(CellPtr, TsHorAlignment.haLeft);
    FSh.WriteColWidth(ColIndex, ColWidths[I], tsSizeUnits.suChars);
  end;

  with DevQuery.VistData do
  begin
    RowIndex := 1;
    N := Count - 1;
    for I := 0 to N do
    begin
      It := Items[I];
      ColIndex := -1;
      Index := -1;
      NewCell(DateToStr(It));
      NewCell(TimeToStr(It));
      NewCell(ToVar(It.Number));
      NewCell(ToVar(It.MeasureT));
      NewCell(ToVar(It.MeasureF));
      NewCell(ToVar(It.Noise));
      NewCell(ObjToStr(It));
      NewCell(ToVar(It.MeasureSpp));
      NewCell(ToVar(It.MeasureVrms));
      NewCell(ToVar(It.MeasureAamp));
      Inc(RowIndex);
      FProgress := MulDiv(100, I + 1, N + 1);
    end;
    FProgress := 100;
  end;
end;

function TExportThread.ToVar(Value: TWInt): Variant;
begin
  if not Assigned(Value)
    then Result := ''
    else Result := Value.Value;
end;

function TExportThread.ToVar(Value: TWFloat32): Variant;
begin
  if not Assigned(Value)
    then Result := ''
    else Result := Value.Value;
end;

{ TExportForm }
procedure TExportForm.FormCreate(Sender: TObject);
begin
  FProgress := 0;
  FCloseReason := ecrUnknown;
  LanguageChanged;
  FExportThread := TExportThread.Create;
end;

procedure TExportForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TExportForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not FExportThread.Finished then
    FExportThread.Terminate;
  FExportThread.Yield;

  if FExportThread.Error
    then FCloseReason := ecrError
    else if FExportThread.Terminated
      then FCloseReason := ecrUserAbort
      else FCloseReason := ecrSuccess;

  CanClose := True;
end;

procedure TExportForm.FormDestroy(Sender: TObject);
begin
  FExportThread.Free;
end;

procedure TExportForm.FormShow(Sender: TObject);
begin
  FExportThread.Start;
  UpdateTimer.Enabled := True;
end;

procedure TExportForm.UpdateTimerTimer(Sender: TObject);
var
  NewProgress: Integer;
begin
  NewProgress := FExportThread.Progress;
  if FProgress <> NewProgress then
  begin
    FProgress := NewProgress;
    ExportProgressBar.Position := FProgress;
  end;

  if FExportThread.Finished then
  begin
    UpdateTimer.Enabled := False;
    Close;
  end;
end;

procedure TExportForm.LanguageChanged;
begin
  Caption := Gti(2800);
  CancelBtn.Caption := Gti(1001);
end;

function TExportForm.GetDevDataType: TDevDataType;
begin
  Result := FExportThread.DevDataType;
end;

procedure TExportForm.SetDevDataType(Value: TDevDataType);
begin
  FExportThread.DevDataType := Value;
end;

function TExportForm.GetFileName: UTF8String;
begin
  Result := FExportThread.FileName;
end;

procedure TExportForm.SetFileName(NewFileName: UTF8String);
begin
  FExportThread.FileName := NewFileName;
end;

end.

