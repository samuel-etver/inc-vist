unit UReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Spin, Buttons, UGlobal, UReportOptions, Windows,
  UMetafile, UDeviceData, Printers, lazutf8, FPCanvas;

type

  { TReportForm }

  TReportForm = class(TForm)
    OptionsBtn: TToolButton;
    PreviewPaintBox: TPaintBox;
    PreviewScrollBox: TScrollBox;
    PrintDlg: TPrintDialog;
    ToolButton1: TToolButton;
    OptionsSep: TToolButton;
    ToolButton2: TToolButton;
    ZoomComboBox: TComboBox;
    ImageList1: TImageList;
    ZoomLbl: TLabel;
    Label2: TLabel;
    PageLbl: TLabel;
    PageCountLbl: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PageEdit: TSpinEdit;
    ToolBar1: TToolBar;
    PageSetupBtn: TToolButton;
    PrintBtn: TToolButton;
    ToolButton3: TToolButton;
    ZoomInBtn: TToolButton;
    ZoomOutBtn: TToolButton;
    FirstPageBtn: TToolButton;
    PriorPageBtn: TToolButton;
    NextPageBtn: TToolButton;
    LastPageBtn: TToolButton;
    procedure FirstPageBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastPageBtnClick(Sender: TObject);
    procedure NextPageBtnClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure PageSetupBtnClick(Sender: TObject);
    procedure PreviewPaintBoxPaint(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure PriorPageBtnClick(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
  private
    { private declarations }
    FDevDataType: TDevDataType;
    FBook: TList;
    FCurrPage: Integer;
    FRegFont: TFont;
    FHeaderFont: TFont;
    FFooterFont: TFont;
    FTextH: Real;
    FCellH: Real;
    FLines: TStringList;
    FPaperW: Real;
    FPaperH: Real;
    FPageW: Real;
    FPageH: Real;
    FZoomedPageW: Real;
    FZoomedPageH: Real;
    FPageWmm: Real;
    FPageHmm: Real;
    FDisplayKx: Real;
    FDisplayKy: Real;
    FKx: Real;
    FKy: Real;
    FMarginLeft: Real;
    FMarginTop: Real;
    FMarginRight: Real;
    FMarginBottom: Real;
    FHeaderY: Real;
    FHeaderH: Real;
    FFooterY: Real;
    FFooterH: Real;
    FBodyY: Real;
    FBodyH: Real;
    FBodyW: Real;
    FZoom: Real;
    FLeftFooterTxt: UTF8String;
    FColumns: TList;
    FColumnWidths: array[0..99] of Real;
    FTableW: Real;
    procedure LanguageChanged;
    procedure ClearBook;
    procedure MakeBook;
    procedure PrintHeader(G: TCanvas; LeftTxt, RightTxt: UTF8String);
    procedure PrintHeader(G: TCanvas; LeftTxtId: Integer);
    procedure PrintFooter(G: TCanvas; LeftTxt, CenterTxt, RightTxt: UTF8String);
    procedure PrintFooter(G: TCanvas; CurrPage: Integer);
    procedure PrintTable(G: TCanvas; Rows: Integer);
    procedure PrintCell(G: TCanvas; var R: TRect; TxtAlign: Integer; Txt: UTF8String);
    procedure PrintBegin;
    procedure PrintBeginDescription;
    procedure PrintBeginInc;
    procedure PrintBeginVist;
    procedure PrintEnd;
    procedure PrintEndDescription;
    procedure PrintEndInc;
    procedure PrintEndVist;
    function PrintPage(G: TCanvas; CurrPage: Integer): Boolean;
    function PrintPageDescription(G: TCanvas; CurrPage: Integer): Boolean;
    function PrintPageInc(G: TCanvas; CurrPage: Integer): Boolean;
    function PrintPageVist(G: TCanvas; CurrPage: Integer): Boolean;
    procedure ZoomComboBoxEditingDone(Sender: TObject);
    procedure UpdatePreview;
    procedure UpdateZoom;
    function GetPageCount: Integer;
    procedure PageEditChange(Sender: TObject);
    procedure SetPageCountLbl;
    procedure SetPageEdit;
    procedure UpdatePageInfo;
  protected
    procedure Loaded; override;
    procedure SetButtonsState;
    property PageCount: Integer read GetPageCount;
  public
    { public declarations }
    class function Execute(Ddt: TDevDataType): Boolean;
    property DevDataType: TDevDataType read FDevDataType write FDevDataType;
  end;

var
  ReportForm: TReportForm;


implementation

uses UMain;

const
  ShadowW = 4;
  ShadowColor = $333333;
  PageX = 8;
  PageY = 8;
  ZoomCount = 8;
  Zooms: array[0..ZoomCount-1] of Real = (
    25,   50,  75, 100,
    125, 150, 175, 200
  );

var
  WasShown: Boolean;
  SavedLeft: Integer;
  SavedTop: Integer;
  SavedRight: Integer;
  SavedBottom: Integer;

{$R *.lfm}

{ TReportForm }
class function TReportForm.Execute(Ddt: TDevDataType): Boolean;
begin
  ReportForm := TReportForm.Create(Application);
  ReportForm.DevDataType := Ddt;
  ReportForm.ShowModal;
  ReportForm.Free;
  Result := True;
end;

procedure TReportForm.FormCreate(Sender: TObject);
var
  I: Integer;
  DtTm: TDateTime;
begin
  DoubleBuffered := True;
  FDevDataType := ddtNone;
  FBook := TList.Create;
  FColumns := TList.Create;
  FCurrPage := 0;
  DtTm := Now;
  FLeftFooterTxt := SysUtils.DateToStr(DtTm) + '   ' + SysUtils.TimeToStr(DtTm);
  FLines := TStringList.Create;
  for I := 0 to ZoomCount-1 do
    ZoomComboBox.Items.Add(FloatToStrF(Zooms[I], ffFixed, 15, 0));
  FZoom := 50;
  LanguageChanged;
  FDisplayKx :=
   GetDeviceCaps(MainForm.Canvas.Handle, HORZRES)/
   GetDeviceCaps(MainForm.Canvas.Handle, HORZSIZE);
  FDisplayKy :=
   GetDeviceCaps(MainForm.Canvas.Handle, VERTRES)/
   GetDeviceCaps(MainForm.Canvas.Handle, VERTSIZE);
  UpdatePageInfo;
end;

procedure TReportForm.FormDestroy(Sender: TObject);
begin
  ClearBook;
  FBook.Free;
  FColumns.Free;
  FLines.Free;
end;

procedure TReportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Wp: TWindowPlacement;
begin
  Wp.length := SizeOf(Wp);
  GetWindowPlacement(Handle, Wp);

  with Wp.rcNormalPosition do
  begin
    SavedLeft := Left;
    SavedTop := Top;
    SavedRight := Right;
    SavedBottom := Bottom;
  end;
end;

procedure TReportForm.FirstPageBtnClick(Sender: TObject);
begin
  FCurrPage := 0;
  SetPageEdit;
  SetButtonsState;
  UpdatePreview;
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  WasShown := True;
  case FDevDataType of
    ddtInc,ddtVist: ;
    else
      OptionsBtn.Visible := False;
      OptionsSep.Visible := False;
  end;
  MakeBook;
  UpdateZoom;
  UpdatePreview;
end;

procedure TReportForm.LastPageBtnClick(Sender: TObject);
begin
  if PageCount = 0
    then FCurrPage := 0
    else FCurrPage := PageCount - 1;
  SetPageEdit;
  SetButtonsState;
  UpdatePreview;
end;

procedure TReportForm.NextPageBtnClick(Sender: TObject);
begin
  if PageCount = 0 then
    FCurrPage := 0
  else if FCurrPage < PageCount - 1 then
    Inc(FCurrPage);
  SetPageEdit;
  UpdatePreview;
  SetButtonsState;
end;

procedure TReportForm.OptionsBtnClick(Sender: TObject);
begin
  if TReportOptionsForm.Execute(Self, FDevDataType) then
  begin
    MakeBook;
    UpdateZoom;
    UpdatePreview;
  end;
end;

procedure TReportForm.PageSetupBtnClick(Sender: TObject);
begin
  if MainForm.PageSetup(Self) then
  begin
    UpdatePageInfo;
    MakeBook;
    UpdateZoom;
    UpdatePreview;
  end;
end;

procedure TReportForm.PreviewPaintBoxPaint(Sender: TObject);
var
  PageW,PageH: Integer;
  DestRect: TRect;
begin
  with (Sender as TPaintBox).Canvas do
  begin
    PageW := Trunc(FZoomedPageW);
    PageH := Trunc(FZoomedPageH);
    Brush.Color := clWhite;
    FillRect(PageX, PageY, PageX + PageW, PageY + PageH);
    Brush.Color := ShadowColor;
    FillRect(PageX + PageW, PageY + ShadowW, PageX + PageW + ShadowW,
     PageY + PageH + ShadowW);
    FillRect(PageX + ShadowW, PageY + PageH, PageX + PageW,
     PageY + PageH + ShadowW);

    DestRect.Left   := PageX;
    DestRect.Top    := PageY;
    DestRect.Right  := PageX + PageW;
    DestRect.Bottom := PageY + PageH;
    if FCurrPage < FBook.Count then
    begin
      StretchDraw(DestRect, TMetafile(FBook.Items[FCurrPage]));
    end;
  end;
end;

procedure TReportForm.PrintBtnClick(Sender: TObject);
var
  I: Integer;
  FromPage,ToPage: Integer;
  NewPage: Boolean = False;
  DtTm: TDateTime;
begin
  PrintDlg.FromPage := 1;
  PrintDlg.ToPage := FBook.Count;
  PrintDlg.PrintRange := prAllPages;

  if not PrintDlg.Execute then
    Exit;

  UpdatePageInfo;
  MakeBook;

  if PrintDlg.PrintRange = prAllPages then
  begin
    FromPage := 0;
    ToPage := FBook.Count - 1;
  end
  else
  begin
    FromPage := PrintDlg.FromPage - 1;
    ToPage := PrintDlg.ToPage - 1;
  end;

  try
    DtTm := Now;
    Printer.Title := SysUtils.DateToStr(DtTm) + ' ' + SysUtils.TimeToStr(DtTm) +
     ' ' + Gti(2928);

    Printer.BeginDoc;

    for I := 0 to FBook.Count - 1 do
    begin
      if (I >= FromPage) and (I <= ToPage) then
      begin
        if NewPage then
          Printer.NewPage;
        NewPage := True;
        Printer.Canvas.Draw(0, 0, TMetafile(FBook.Items[I]));
      end;
    end;

    Printer.EndDoc;
  except
  end;

  Close;
end;

procedure TReportForm.PriorPageBtnClick(Sender: TObject);
begin
  if FCurrPage > 0 then
  begin
    Dec(FCurrPage);
    SetPageEdit;
    SetButtonsState;
    UpdatePreview;
  end;
end;

procedure TReportForm.ZoomInBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ZoomCount-1 do
  begin
    if Zooms[I] > FZoom then
    begin
      FZoom := Zooms[I];
      UpdateZoom;
      UpdatePreview;
      Break;
    end;
  end;
end;

procedure TReportForm.ZoomOutBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := ZoomCount-1 downto 0 do
  begin
    if Zooms[I] < FZoom then
    begin
      FZoom := Zooms[I];
      UpdateZoom;
      UpdatePreview;
      Break;
    end;
  end;
end;

procedure TReportForm.ZoomComboBoxEditingDone(Sender: TObject);
var
  Txt: UTF8String;
  K: Real;
  OldZoom: Real;
begin
  OldZoom := FZoom;
  Txt := UTF8Trim(ZoomComboBox.Text);
  try
    K := StrToFloat(Txt);
    if K < Zooms[0]
      then K := Zooms[0]
      else if K > Zooms[ZoomCount-1]
        then K := Zooms[ZoomCount-1];
    FZoom := K;
  except
  end;
  UpdateZoom;
  if OldZoom <> FZoom then
    UpdatePreview;
end;

procedure TReportForm.UpdateZoom;
begin
  FZoomedPageW := 0.01*FZoom*FPageW;
  FZoomedPageH := 0.01*FZoom*FPageH;
  ZoomComboBox.OnEditingDone := nil;
  ZoomComboBox.Text := FloatToStrF(FZoom, ffFixed, 15, 0);
  ZoomComboBox.OnEditingDone := @ZoomComboBoxEditingDone;
  PreviewPaintBox.Width := Trunc(FZoomedPageW + 2*PageX);
  PreviewPaintBox.Height := Trunc(FZoomedPageH + 2*PageY);
  SetButtonsState;
end;

procedure TReportForm.UpdatePageInfo;
begin
  FPaperW := Printer.PaperSize.Width;
  FPaperH := Printer.PaperSize.Height;
  FKx := 1/(25.4/Printer.XDPI);
  FKy := 1/(25.4/Printer.YDPI);
  FPageWmm := FPaperW/FKx;
  FPageHmm := FPaperH/FKy;
  FPageW := FPageWmm*FDisplayKx;
  FPageH := FPageHmm*FDisplayKy;
end;

procedure TReportForm.LanguageChanged;
begin
  Caption := Gti(2900);
  ZoomLbl.Caption := Gti(2908);
  PageLbl.Caption := Gti(2907);
  OptionsBtn.Hint := Gti(2924);
  PrintBtn.Hint := Gti(2910);
  PageSetupBtn.Hint := Gti(2911);
  ZoomInBtn.Hint := Gti(2905);
  ZoomOutBtn.Hint := Gti(2906);
  FirstPageBtn.Hint := Gti(2904);
  PriorPageBtn.Hint := Gti(2902);
  NextPageBtn.Hint := Gti(2903);
  LastPageBtn.Hint := Gti(2901);
  SetPageCountLbl;
end;

procedure TReportForm.SetPageCountLbl;
begin
  PageCountLbl.Caption := Gti(2909) + ' ' + IntToStr(PageCount);
end;

procedure TReportForm.SetPageEdit;
begin
  PageEdit.OnChange := nil;
  PageEdit.Value := 1 + FCurrPage;
  PageEdit.OnChange := @PageEditChange;
end;

procedure TReportForm.Loaded;
begin
  inherited Loaded;

  if WasShown then
  begin
    Self.Position := TPosition.poDesigned;
    Self.SetBounds(SavedLeft, SavedTop,
     SavedRight - SavedLeft - 2*GetSystemMetrics(SM_CXSIZEFRAME),
     SavedBottom - SavedTop - 2*GetSystemMetrics(SM_CYSIZEFRAME) -
     GetSystemMetrics(SM_CYCAPTION));
  end;

  PageEdit.OnChange := @PageEditChange;
end;

function TReportForm.GetPageCount: Integer;
begin
  Result := FBook.Count;
end;

procedure TReportForm.SetButtonsState;
var
  Cnt: Integer;
begin
  Cnt := PageCount;
  PrintBtn.Enabled := Cnt > 0;
  FirstPageBtn.Enabled := FCurrPage > 0;
  PriorPageBtn.Enabled := FirstPageBtn.Enabled;
  NextPageBtn.Enabled := (Cnt > 0) and (FCurrPage < Cnt - 1);
  LastPageBtn.Enabled := NextPageBtn.Enabled;
  PageEdit.Enabled := Cnt > 0;
  ZoomOutBtn.Enabled := FZoom > 25;
  ZoomInBtn.Enabled := FZoom < 200;
end;

procedure TReportForm.UpdatePreview;
begin
  PreviewPaintBox.Invalidate;
end;

procedure TReportForm.PageEditChange(Sender: TObject);
begin
  FCurrPage := PageEdit.Value - 1;
  SetButtonsState;
  UpdatePreview;
end;

procedure TReportForm.ClearBook;
var
  I: Integer;
begin
  for I := FBook.Count - 1 downto 0 do
    TMetafile(FBook.Items[I]).Free;
  FBook.Clear;
end;

procedure TReportForm.MakeBook;
var
  Mf: TMetafile;
  G: TMetafileCanvas;
  Done: Boolean;
  I: Integer;
begin
  ClearBook;
  Done := False;
  I := 0;
  PrintBegin;
  while not Done do
  begin
    Mf := TMetafile.Create;
    Mf.Width := Trunc(FPaperW);
    Mf.Height := Trunc(FPaperH);
    G := TMetafileCanvas.Create(Mf, 0);
    Done := not PrintPage(G, I);
    G.Free;
    if Done
      then Mf.Free
      else FBook.Add(Mf);
    Inc(I);
  end;
  PrintEnd;

  PageEdit.OnChange := nil;
  if PageCount = 0 then
  begin
    PageEdit.MinValue := 0;
    PageEdit.Value := 0;
    PageEdit.MaxValue := 0;
    FCurrPage := 0;
  end
  else
  begin
    PageEdit.MinValue := 0;
    PageEdit.Value := 1;
    PageEdit.MinValue := 1;
    PageEdit.MaxValue := PageCount;
    if FCurrPage < 0
      then FCurrPage := 0
      else if FCurrPage > PageCount - 1 then
        FCurrPage := PageCount - 1;
    PageEdit.Value := FCurrPage + 1;
  end;
  PageEdit.OnChange := @PageEditChange;

  SetPageCountLbl;
  SetButtonsState;
end;

procedure TReportForm.PrintHeader(G: TCanvas; LeftTxt, RightTxt: UTF8String);
var
  X0,X1: Integer;
  Y: Integer;
  R: TRect;
  TxtW: Integer;
begin
  G.Pen.Color := clBlack;
  G.Pen.EndCap := TFPPenEndCap.pecFlat;

  X0 := Trunc(FMarginLeft);
  X1 := Trunc(FPaperW - FMarginRight);
{
  G.Pen.Width := 1;
  Y := Trunc(FHeaderY);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);
}
  G.Pen.Width := 1;
  Y := Trunc(FHeaderY + FHeaderH - 3*FKy);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);

  Y -= Trunc(1.5*FKy);
  G.Pen.Width := Trunc(1.0*FKy);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);

  G.Font := FHeaderFont;
  R.Left := X0;
  R.Right := R.Left + G.TextWidth(LeftTxt);
  if R.Right > X1 then
    R.Right := X1;
  R.Bottom := Trunc(Y - 0.25*FKy);
  R.Top := R.Bottom - G.TextHeight('Wg');
  G.TextRect(R, R.Left, R.Top, LeftTxt);

  TxtW := G.TextWidth(RightTxt);
  R.Right := Trunc(FMarginLeft + FBodyW);
  R.Left := R.Right - TxtW;
  if R.Left < FMarginLeft then
    R.Left := Trunc(FMarginLeft);
  G.TextRect(R, R.Right - TxtW, R.Top, RightTxt);
end;

procedure TReportForm.PrintHeader(G: TCanvas; LeftTxtId: Integer);
begin
  PrintHeader(G, Gti(LeftTxtId), Gti(1402));
end;

procedure TReportForm.PrintFooter(G: TCanvas; LeftTxt, CenterTxt, RightTxt: UTF8String);
var
  X0,X1: Integer;
  Y: Integer;
  R: TRect;
  TxtW: Integer;
begin
  G.Pen.Color := clBlack;
  G.Pen.EndCap := TFPPenEndCap.pecFlat;

  X0 := Trunc(FMarginLeft);
  X1 := Trunc(FPaperW - FMarginRight);
  {
  G.Pen.Width := 1;
  Y := Trunc(FFooterY + FFooterH);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);
  }
  G.Pen.Width := 1;
  Y  := Trunc(FFooterY + 5*FKy);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);

  Y += Trunc(1.5*FKy);
  G.Pen.Width := Trunc(1.0*FKy);
  G.MoveTo(X0, Y);
  G.LineTo(X1, Y);

  G.Font := FFooterFont;
  R.Left := X0;
  R.Right := R.Left + G.TextWidth(LeftTxt);
  if R.Right > X1 then
    R.Right := X1;
  R.Top := Trunc(Y + 1*FKy);
  R.Bottom := R.Top + G.TextHeight('Wg');
  G.TextRect(R, R.Left, R.Top, LeftTxt);

  TxtW := G.TextWidth(RightTxt);
  R.Right := Trunc(FMarginLeft + FBodyW);
  R.Left := R.Right - TxtW;
  if R.Left < FMarginLeft then
    R.Left := Trunc(FMarginLeft);
  G.TextRect(R, R.Right - TxtW, R.Top, RightTxt);

  if Length(CenterTxt) > 0 then
  begin
    TxtW := G.TextWidth(CenterTxt);
    R.Left := Trunc((FBodyW - TxtW)/2 + FMarginLeft);
    R.Right := R.Left + TxtW;
    X0 := R.Left;
    if R.Left < FMarginLeft then
      R.Left := Trunc(FMarginLeft);
    if R.Right > X1 then
      R.Right := X1;
    G.TextRect(R, X0, R.Top, CenterTxt);
  end;
end;

procedure TReportForm.PrintFooter(G: TCanvas; CurrPage: Integer);
begin
  PrintFooter(G, FLeftFooterTxt, '', Gti(2914) + IntToStr(CurrPage + 1));
end;

procedure TReportForm.PrintTable(G: TCanvas; Rows: Integer);
var
  I,N: Integer;
  X,W: Real;
  Y: Real;
  IntX0,IntX1: Integer;
  IntY0,IntY1: Integer;
begin
  if Rows < 1 then
    Exit;

  N := FColumns.Count - 1;
  if N < 0 then
    Exit;

  G.Pen.Width := 1;
  X := FMarginLeft;
  IntX0 := Trunc(X);
  IntY0 := Trunc(FBodyY);
  IntY1 := Trunc(FBodyY + FCellH*(Rows + 1));
  G.MoveTo(IntX0, IntY0);
  G.LineTo(IntX0, IntY1);
  for I := 0 to N do
  begin
    W := FColumnWidths[I];
    if W > 0 then
    begin
      X := X + W;
      if X > FMarginLeft + FBodyW then
        Break;
      IntX0 := Trunc(X);
      G.MoveTo(IntX0, IntY0);
      G.LineTo(IntX0, IntY1);
    end;
  end;

  Y := FBodyY;
  X := FMarginLeft;
  IntX0 := Trunc(X);
  IntX1 := Trunc(X + FTableW);
  for I := 0 to Rows + 1 do
  begin
    Y := FBodyY + I*FCellH;
    IntY0 := Trunc(Y);
    G.MoveTo(IntX0, IntY0);
    G.LineTo(IntX1 + 1, IntY0);
  end;
end;

procedure TReportForm.PrintCell(G: TCanvas; var R: TRect; TxtAlign: Integer;
 Txt: UTF8String);
var
  X: Integer;
begin
  if R.Left >= FMarginLeft + FBodyW then
    Exit;

  X := R.Right;

  if R.Right >= FMarginLeft + FBodyW then
    R.Right := Trunc(FMarginLeft + FBodyW);

  case TxtAlign of
    -1: G.TextRect(R, Trunc(R.Left + FKx*1.5), R.Top, Txt);
     0: G.TextRect(R, (R.Left + X - G.TextWidth(Txt)) div 2, R.Top, Txt);
     1: G.TextRect(R, Trunc(X - G.TextWidth(Txt) - FKx*1.5), R.Top, Txt);
  end;
end;

procedure TReportForm.PrintBegin;
begin
  FRegFont := TFont.Create;
  FRegFont.BeginUpdate;
  FRegFont.Name := 'Arial';
  FRegFont.Color := clBlack;
  FRegFont.Size := Trunc(2.60*FKy);
  FRegFont.EndUpdate;

  FHeaderFont := TFont.Create;
  FHeaderFont.BeginUpdate;
  FHeaderFont.Name := 'Arial';
  FHeaderFont.Color := clBlack;
  FHeaderFont.Size := Trunc(5.2*FKy);
  FHeaderFont.Style := [fsBold];
  FHeaderFont.EndUpdate;

  FFooterFont := TFont.Create;
  FFooterFont.BeginUpdate;
  FFooterFont.Name := 'Arial';
  FFooterFont.Color := clBlack;
  FFooterFont.Size := Trunc(2.4*FKy);
  FFooterFont.EndUpdate;

  FMarginLeft := UGlobal.PageMarginLeft*FKx;
  FMarginRight := UGlobal.PageMarginRight*FKx;
  FMarginTop  := UGlobal.PageMarginTop*FKy;
  FMarginBottom := UGlobal.PageMarginBottom*FKy;
  FHeaderY := FMarginTop;
  FHeaderH := 15.0*FKy;
  FFooterH := 11.3*FKy;
  FFooterY := FPaperH - FMarginBottom - FFooterH;
  FBodyY := FHeaderY + FHeaderH;
  FBodyH := FFooterY - FBodyY;
  FBodyW := FPaperW - FMarginLeft - FMarginRight;

  FTextH := -1;
  FCellH := -1;

  case FDevDataType of
    ddtDescription: PrintBeginDescription;
    ddtInc:         PrintBeginInc;
    ddtVist:        PrintBeginVist;
  end;
end;

procedure TReportForm.PrintEnd;
begin
  case FDevDataType of
    ddtDescription: PrintEndDescription;
    ddtInc:         PrintEndInc;
    ddtVist:        PrintEndVist;
  end;

  FRegFont.Free;
  FHeaderFont.Free;
  FFooterFont.Free;
end;

function TReportForm.PrintPage(G: TCanvas; CurrPage: Integer): Boolean;
var
  H: Real;
  I,N: Integer;
  PColumn: ^TReportColumn;
  TmpFloat: Real;
begin
  if (FBodyH < 1) or (FBodyW < 1) then
  begin
    Result := False;
    Exit;
  end;

  if FTextH < 0 then
  begin
    G.Font := FRegFont;
    H := G.TextHeight('Wg');
    FTextH := 1.05*H;
    FCellH := 1.25*H;
    FTableW := 0;

    if FDevDataType in [ddtInc,ddtVist] then
    begin
      N := FColumns.Count - 1;
      for I := 0 to N do
      begin
        PColumn := FColumns.Items[I];
        TmpFloat := FKx*PColumn^.Width;
        FColumnWidths[I] := TmpFloat;
        if TmpFloat > 0 then
          FTableW := FTableW + TmpFloat;
      end;
      if FTableW > FBodyW then
        FTableW := FBodyW;
    end;
  end;

  case FDevDataType of
    ddtDescription: Result := PrintPageDescription(G, CurrPage);
    ddtInc:         Result := PrintPageInc(G, CurrPage);
    ddtVist:        Result := PrintPageVist(G, CurrPage);
    else            Result := False;
  end;
end;

procedure TReportForm.PrintBeginDescription;
begin
  FLines.Clear;
  if Assigned(DevQuery.Description.Text) then
    FLines.Text := DevQuery.Description.Text.Value;
end;

procedure TReportForm.PrintEndDescription;
begin
end;

function TReportForm.PrintPageDescription(G: TCanvas; CurrPage: Integer): Boolean;
var
  I,N: Integer;
  R: TRect;
  Page: Integer = 0;
  Y: Real;
  NextY: Real;
begin
  Result := False;

  R.Left := Trunc(FMarginLeft);
  R.Right := Trunc(FPaperW - FMarginRight);

  Y := FBodyY;
  G.Font := FRegFont;
  N := FLines.Count - 1;
  for I := 0 to N do
  begin
    NextY := Y + FTextH;
    if Page = CurrPage then
    begin
      R.Top := Trunc(Y);
      R.Bottom := Trunc(R.Top + FTextH);
      G.TextRect(R, R.Left, R.Top, FLines.Strings[I]);
      Result := True;
    end;

    Y := NextY;

    if NextY > FFooterY then
    begin
      if Result then
        Break;
      Y := FBodyY;
      NextY := Y + FTextH;
      Inc(Page);
    end;
  end;

  if Result then
  begin
    PrintHeader(G, 2916);
    PrintFooter(G, CurrPage);
  end;
end;

function TReportForm.PrintPageInc(G: TCanvas; CurrPage: Integer): Boolean;
var
  Data: TIncData;
  Item: TIncItem;
  I,J,N: Integer;
  R: TRect;
  Page: Integer = 0;
  X,Y: Real;
  W: Real;
  NextX,NextY: Real;
  Txt: UTF8String;
  PColumn: ^TReportColumn;
  Rows: Integer = 0;
  TxtAlign: Integer;
  TxtMarginTop: Integer;
  TableHeader: Boolean = False;
begin
  Result := False;

  TxtMarginTop := Trunc(FKy*0.8);

  Data := DevQuery.IncData;
  Y := FBodyY;
  N := Data.Count - 1;
  for I := 0 to N do
  begin
    NextY := Y + FCellH;
    if not TableHeader then
    begin
      if Page = CurrPage then
      begin
        G.Font := FRegFont;
        X := FMarginLEft;
        R.Top := Trunc(Y) + TxtMarginTop;
        R.Bottom := Trunc(NextY);
        for J := 0 to FColumns.Count - 1 do
        begin
          PColumn := FColumns.Items[J];
          case PColumn^.Id of
            0..IncColumnCount-2:
              Txt := Gti(IncColumnIds[PColumn^.Id]);
            IncColumnCount-1:
              Txt := Gti(IncColumnIds[PColumn^.Id]) + SigmaUnitsToStr(SigmaUnits)
            else Txt := '';
          end;
          W := FColumnWidths[J];
          if W > 0 then
          begin
            NextX := X + W;
            R.Left := Trunc(X);
            R.Right := Trunc(NextX);
            PrintCell(G, R, 0, Txt);
            X := NextX;
          end;
        end;
      end;
      TableHeader := True;
      Y := NextY;
      NextY := Y + FCellH;
    end;

    if Page = CurrPage then
    begin
      G.Font := FRegFont;
      Inc(Rows);
      X := FMarginLeft;
      Item := Data.Items[I];
      R.Top := Trunc(Y) + TxtMarginTop;
      R.Bottom := Trunc(NextY);
      for J := 0 to FColumns.Count - 1 do
      begin
        PColumn := FColumns.Items[J];
        case PColumn^.Id of
          0: Txt := IntToStr(I + 1);
          1: Txt := DateToStr(Item);
          2: Txt := TimeToStr(Item);
          3: Txt := NumberToStr(Item);
          4: Txt := DiameterToStr(Item);
          5: Txt := LengthToStr(Item);
          6: Txt := MeasureTtoStr(Item);
          7: Txt := MeasureFtoStr(Item);
          8: Txt := NoiseToStr(Item);
          9: Txt := EpsilonToStr(Item);
          10: Txt := DeltaLtoStr(Item);
          11: Txt := SigmaToStr(Item, SigmaUnits);
          else Txt := '';
        end;

        case PColumn^.Id of
          100: TxtAlign := -1;
          101: TxtAlign := 0;
          else TxtAlign := 1;
        end;

        W := FColumnWidths[J];
        if W > 0 then
        begin
          NextX := X + W;
          R.Left := Trunc(X);
          R.Right := Trunc(NextX);
          PrintCell(G, R, TxtAlign, Txt);
          X := NextX;
        end;
      end;
      Result := True;
    end;
    Y := NextY;

    if NextY > FFooterY then
    begin
      if Result then
        Break;
      TableHeader := False;
      Y := FBodyY;
      NextY := Y + FCellH;
      Inc(Page);
    end;
  end;

  if Result then
  begin
    PrintTable(G, Rows);
    PrintHeader(G, 2913);
    PrintFooter(G, CurrPage);
  end;
end;

procedure TReportForm.PrintBeginInc;
var
  I,N: Integer;
  SrcColumns: TList;
  Item: ^TReportColumn;
begin
  FColumns.Clear;
  SrcColumns := IncReportColumns;
  N := SrcColumns.Count - 1;
  for I := 0 to N do
  begin
    Item := SrcColumns.Items[I];
    if Item^.Checked or UGlobal.IncReportAllFields then
      FColumns.Add(Item);
  end;
end;

procedure TReportForm.PrintEndInc;
begin
end;

function TReportForm.PrintPageVist(G: TCanvas; CurrPage: Integer): Boolean;
var
  Data: TVistData;
  Item: TVistItem;
  I,J,N: Integer;
  R: TRect;
  Page: Integer = 0;
  X,Y: Real;
  W: Real;
  NextX,NextY: Real;
  Txt: UTF8String;
  PColumn: ^TReportColumn;
  Rows: Integer = 0;
  TxtAlign: Integer;
  TxtMarginTop: Integer;
  TableHeader: Boolean = False;
begin
  Result := False;

  TxtMarginTop := Trunc(FKy*0.8);

  Data := DevQuery.VistData;
  Y := FBodyY;
  N := Data.Count - 1;
  for I := 0 to N do
  begin
    NextY := Y + FCellH;
    if not TableHeader then
    begin
      if Page = CurrPage then
      begin
        G.Font := FRegFont;
        X := FMarginLEft;
        R.Top := Trunc(Y) + TxtMarginTop;
        R.Bottom := Trunc(NextY);
        for J := 0 to FColumns.Count - 1 do
        begin
          PColumn := FColumns.Items[J];
          case PColumn^.Id of
            0..VistColumnCount-1:
              Txt := Gti(VistColumnIds[PColumn^.Id]);
            else Txt := '';
          end;
          W := FColumnWidths[J];
          if W > 0 then
          begin
            NextX := X + W;
            R.Left := Trunc(X);
            R.Right := Trunc(NextX);
            PrintCell(G, R, 0, Txt);
            X := NextX;
          end;
        end;
      end;
      TableHeader := True;
      Y := NextY;
      NextY := Y + FCellH;
    end;

    if Page = CurrPage then
    begin
      G.Font := FRegFont;
      Inc(Rows);
      X := FMarginLeft;
      Item := Data.Items[I];
      R.Top := Trunc(Y) + TxtMarginTop;
      R.Bottom := Trunc(NextY);
      for J := 0 to FColumns.Count - 1 do
      begin
        PColumn := FColumns.Items[J];
        case PColumn^.Id of
          0: Txt := IntToStr(I + 1);
          1: Txt := DateToStr(Item);
          2: Txt := TimeToStr(Item);
          3: Txt := NumberToStr(Item);
          4: Txt := MeasureTtoStr(Item);
          5: Txt := MeasureFtoStr(Item);
          6: Txt := NoiseToStr(Item);
          7: Txt := ObjToStr(Item);
          8: Txt := MeasureSppToStr(Item);
          9: Txt := MeasureVrmsToStr(Item);
          10: Txt := MeasureAampToStr(Item);
          else Txt := '';
        end;

        case PColumn^.Id of
          100: TxtAlign := -1;
          7: TxtAlign := 0;
          else TxtAlign := 1;
        end;

        W := FColumnWidths[J];
        if W > 0 then
        begin
          NextX := X + W;
          R.Left := Trunc(X);
          R.Right := Trunc(NextX);
          PrintCell(G, R, TxtAlign, Txt);
          X := NextX;
        end;
      end;
      Result := True;
    end;
    Y := NextY;

    if NextY > FFooterY then
    begin
      if Result then
        Break;
      TableHeader := False;
      Y := FBodyY;
      NextY := Y + FCellH;
      Inc(Page);
    end;
  end;

  if Result then
  begin
    PrintTable(G, Rows);
    PrintHeader(G, 2927);
    PrintFooter(G, CurrPage);
  end;
end;

procedure TReportForm.PrintBeginVist;
var
  I,N: Integer;
  SrcColumns: TList;
  Item: ^TReportColumn;
begin
  FColumns.Clear;
  SrcColumns := VistReportColumns;
  N := SrcColumns.Count - 1;
  for I := 0 to N do
  begin
    Item := SrcColumns.Items[I];
    if Item^.Checked or UGlobal.VistReportAllFields then
      FColumns.Add(Item);
  end;
end;

procedure TReportForm.PrintEndVist;
begin
end;

procedure Init;
begin
  WasShown := False;
end;

initialization

  Init;

end.

