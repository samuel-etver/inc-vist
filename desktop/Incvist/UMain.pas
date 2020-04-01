unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, StdCtrls, UStartup, UFileName, UAbout, ULang,
  UGlobal, Windows, UInfo, UQuery, UMemDump, UExport, UDescription, URead,
  UVistView, UIncView, PrintersDlgs, UDeviceData, UTypes, UReport, LazUTF8,
  LazFileUtils, Printers, UReportOptions;

const
  UM_STARTUP = WM_USER + 1;

type
  { TMainForm }
  TMainForm = class(TForm)
    DescriptionFrame1: TDescriptionFrame;
    IncFrame: TIncViewFrame;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    ExportDlg: TSaveDialog;
    SigmaUnitsComboBox: TComboBox;
    ProjNameFrame: TFileNameFrame;
    ImageList1: TImageList;
    SigmaUnitsLbl: TLabel;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    DataPageControl: TPageControl;
    Panel1: TPanel;
    SaveAsMenuItem: TMenuItem;
    MenuItem11: TMenuItem;
    ReportMenuItem: TMenuItem;
    MenuItem13: TMenuItem;
    QueryMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    MenuItem16: TMenuItem;
    ReadMenuItem: TMenuItem;
    ExportMenuItem: TMenuItem;
    MenuItem19: TMenuItem;
    OptionsMenuItem: TMenuItem;
    ClearFileHistoryMenuItem: TMenuItem;
    MenuItem21: TMenuItem;
    ReopenMenuItem: TMenuItem;
    MenuItem4: TMenuItem;
    InfoMenuItem: TMenuItem;
    PageMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    LangMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    DescriptionTabSheet: TTabSheet;
    IncTabSheet: TTabSheet;
    AddDataBtn: TToolButton;
    VistTabSheet: TTabSheet;
    ToolBar1: TToolBar;
    NewBtn: TToolButton;
    MemDumpBtn: TToolButton;
    ReadBtn: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ReportBtn: TToolButton;
    DebugSepBtn: TToolButton;
    OpenBtn: TToolButton;
    CloseBtn: TToolButton;
    SaveBtn: TToolButton;
    ToolButton5: TToolButton;
    ExportBtn: TToolButton;
    ToolButton7: TToolButton;
    DeleteBtn: TToolButton;
    QueryBtn: TToolButton;
    VistFrame: TVistViewFrame;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddDataBtnClick(Sender: TObject);
    procedure ClearFileHistoryMenuItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfoMenuItemClick(Sender: TObject);
    procedure LangMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure MemDumpBtnClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure PageMenuItemClick(Sender: TObject);
    procedure QueryMenuItemClick(Sender: TObject);
    procedure ReadMenuItemClick(Sender: TObject);
    procedure ReportMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SigmaUnitsComboBoxChange(Sender: TObject);
  private
    { private declarations }
    FMruMenuItemList: TList;
    procedure LanguageChanged;
    procedure UmStartup(var {%H-}Message: TMessage); message UM_STARTUP;
    function CloseProj: Boolean;
    function OpenProj(FileName: UTF8String): Boolean;
    procedure NewProj;
    function SaveProj: Boolean;
    function SaveAsProj: Boolean;
    procedure SetMenuItemsState;
    procedure SetProjNameLabel;
    procedure GotoFirstRecord;
    procedure DataEdited(Sender: TObject);
    procedure CommitData;
    procedure DataChanged;
    function GetDevDataType: TDevDataType;
    procedure SetDataPageControlVisible;
    procedure MruMenuItemClick(Sender: TObject);
    procedure RetitleMruMenuItems;
    procedure DeleteMruMenuItem(ProjName: UTF8String);
    procedure AddMruMenuItem(ProjName: UTF8String);
  public
    { public declarations }
    function PageSetup(Form: TForm): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  I,N: Integer;
  MenuItem: TMenuItem;
begin
  LoadTextRes;
  LoadPicRes;

  Width := MainFrameWidth - 2*GetSystemMetrics(SM_CXSIZEFRAME);
  Height := MainFrameHeight - 2*GetSystemMetrics(SM_CYSIZEFRAME) -
   GetSystemMetrics(SM_CYCAPTION);
  MemDumpBtn.Visible := Debug;
  AddDataBtn.Visible := Debug;
  DebugSepBtn.Visible := Debug;
  FMruMenuItemList := TList.Create;
  N := MruList.Count - 1;
  for I := 0 to N do
  begin
    MenuItem := TMenuItem.Create(MainMenu1);
    MenuItem.Caption := IntToStr(I);
    MenuItem.OnClick := @MruMenuItemClick;
    FMruMenuItemList.Add(MenuItem);
  end;
  for I := N downto 0 do
    ReopenMenuItem.Insert(0, TMenuItem(FMruMenuItemList.Items[I]));
  RetitleMruMenuItems;
  DataPageControl.Align := alClient;
  DataPageControl.PageIndex := 0;
  SetMenuItemsState;
  LanguageChanged;
  DescriptionFrame1.DataEdited := @DataEdited;
  IncFrame.DataEdited := @DataEdited;
  VistFrame.DataEdited := @DataEdited;
 end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMruMenuItemList.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if StartupDialogEnabled then
    PostMessage(Handle, UM_STARTUP, 0, 0);
end;

procedure TMainForm.InfoMenuItemClick(Sender: TObject);
begin
  TInfoForm.Execute;
end;

procedure TMainForm.UmStartup(var Message: TMessage);
var
  Choice: UStartup.TChoice;

begin
  Choice := scNone;
  StartupForm := TStartupForm.Create(Self);
  if MruList.Count = 0 then
    StartupForm.DisableOpenLast;
  if StartupForm.ShowModal = mrOk then
    Choice := StartupForm.Choice;
  StartupForm.Free;

  case Choice of
    scCreateNew:    NewMenuItem.Click;
    scOpenExisting: OpenMenuItem.Click;
    scOpenLast:     TMenuItem(FMruMenuItemList.Items[0]).Click;
  end;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  TAboutForm.Execute;
end;

procedure TMainForm.AddDataBtnClick(Sender: TObject);
var
  I,N: Integer;
  IncItem: TIncItem;
  VistItem: TVistItem;
  Hour,Min,Sec: Word;
  Day,Mon,Year: Word;
  DtTm: TDateTime;

  function RandomDate: TDateTime;
  begin
    Hour := Random(23);
    Min := Random(59);
    Sec := Random(59);
    Day := Random(27) + 1;
    Mon := Random(11) + 1;
    Year := Random(10) + 2000;

    Result := EncodeTime(Hour, Min, Sec, 0) + EncodeDate(Year, Mon, Day);
  end;

begin
  CommitData;

  IncDataRead := 0;
  IncDataMatched := 0;
  IncDataAdded := 0;
  VistDataRead := 0;
  VistDataMatched := 0;
  VistDataAdded := 0;

  Randomize;

  IncFrame.BeginAddData;
  VistFrame.BeginAddData;

  N := Random(100);
  for I := 0 to N do
  begin
    Inc(IncDataRead);
    DtTm := RandomDate;
    if DevData.IncData.Find(Trunc(DtTm), Frac(DtTm)) >= 0 then
    begin
      Inc(IncDataMatched);
    end
    else
    begin
      IncItem := TIncItem.Create;
      with IncItem do
      begin
        Date := TWDate.Create;
        Date.Value := Trunc(DtTm);
        Time := TWTime.Create;
        Time.Value := Frac(DtTm);
        Number := TWInt.Create;
        Number.Value := Random(100) + 950;
        Noise := TWFloat32.Create;
        Noise.Value := Random(100000)/1000.0;
        MeasureT := TWFloat32.Create;
        MeasureT.Value := Random(100000)/1000.0;
        Diameter := TWInt.Create;
        Diameter.Value := Random(10) + 10;
        Len := TWFloat32.Create;
        Len.Value := (Random(20) + 90)/100;
        Epsilon := TWFloat32.Create;
        Epsilon.Value := Random(100000)/1000.0;
        DeltaL := TWFloat32.Create;
        DeltaL.Value := Random(100000)/1000.0;
        Sigma := TWFloat32.Create;
        Sigma.Value := Random(100000)/1000.0;
      end;
      DevData.IncData.Add(IncItem);
      Inc(IncDataAdded);
    end;
  end;

  N := Random(100);
  for I := 0 to N do
  begin
    Inc(VistDataRead);
    DtTm := RandomDate;
    if DevData.VistData.Find(Trunc(DtTm), Frac(DtTm)) >= 0 then
    begin
      Inc(VistDataMatched);
    end
    else
    begin
      VistItem := TVistItem.Create;
      with VistItem do
      begin
        Date := TWDate.Create;
        Date.Value := Trunc(DtTm);
        Time := TWTime.Create;
        Time.Value := Frac(DtTm);
        Number := TWInt.Create;
        Number.Value := Random(100) + 950;
        Noise := TWFloat32.Create;
        Noise.Value := Random(100000)/1000.0;
        MeasureT := TWFloat32.Create;
        MeasureT.Value := Random(100000)/1000.0;
        Typ := TWInt.Create;
        Typ.Value := VistTypFirstId + Random(3);
        case Typ.Value of
          VistTypSId:
          begin
            MeasureSpp := TWFloat32.Create;
            MeasureSpp.Value := Random(100000)/1000.0;
          end;
          VistTypVId:
          begin
            MeasureVrms := TWFloat32.Create;
            MeasureVrms.Value := Random(100000)/1000.0;
          end;
          VistTypAId:
          begin
            MeasureAamp := TWFloat32.Create;
            MeasureAamp.Value := Random(100000)/1000.0;
          end;
        end;
        Obj := TWInt.Create;
        Obj.Value := VistObjGeneralId + Random(2);
      end;
      DevData.VistData.Add(VistItem);
      Inc(VistDataAdded);
    end;
  end;

  DevData.Sort;

  MakeQuery;

  IncFrame.EndAddData;
  VistFrame.EndAddData;

  ProjEdited := True;
  SetMenuItemsState;

  InfoMenuItem.Click;
end;

procedure TMainForm.ClearFileHistoryMenuItemClick(Sender: TObject);
var
  I: Integer;
begin
  MruList.Clear;
  for I := FMruMenuItemList.Count - 1 downto 0 do
  begin
    TMenuItem(FMruMenuItemList.Items[I]).Free;
  end;
  FMruMenuItemList.Clear;
  SetMenuItemsState;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  CloseProj;
end;

procedure TMainForm.DeleteMenuItemClick(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(DevQuery) then
    Exit;

  I := MessageBoxW(Handle, PWideChar(WideString(Gti(179))),
     PWideChar(WideString(Gti(178))), MB_YESNO or MB_ICONQUESTION);
  if I <> IDYES then
    Exit;

  case DataPageControl.PageIndex of
    0:
    begin
      DevQuery.Description.Clear;
      DevData.Description.Clear;
      DescriptionFrame1.DataChanged;
      DescriptionFrame1.GotoFirstRecord;
    end;

    1:
    begin
      for I := DevQuery.IncData.Count - 1 downto 0 do
        DevData.IncData.Delete(DevQuery.IncData.Items[I]);
      DevQuery.IncData.Clear;
      IncFrame.DataChanged;
      IncFrame.GotoFirstRecord;
    end;

    2:
    begin
      for I := DevQuery.VistData.Count - 1 downto 0 do
        DevData.VistData.Delete(DevQuery.VistData.Items[I]);
      DevQuery.VistData.Clear;
      VistFrame.DataChanged;
      VistFrame.GotoFirstRecord;
    end;
  end;

  ProjEdited := True;
  SetMenuItemsState;
end;

function TMainForm.GetDevDataType: TDevDataType;
begin
  case DataPageControl.ActivePageIndex of
    0: Result   := ddtDescription;
    1: Result   := ddtInc;
    2: Result   := ddtVist;
    else Result := ddtNone;
  end;
end;

procedure TMainForm.ExportMenuItemClick(Sender: TObject);
var
  CloseReason: TExportCloseReason;
  FullFileName: UTF8String;
begin
  if not ExportFileNameAvailable then
  begin
    ExportDlg.FileName := Gti(127);
  end
  else
  begin
    ExportDlg.FileName := ExportFileName;
  end;

  if not ExportDlg.Execute then
    Exit;

  ExportFileNameAvailable := True;

  CommitData;

  ExportForm := TExportForm.Create(Application);
  ExportForm.DevDataType := GetDevDataType;
  FullFileName := ExportDlg.FileName;
  ExportFileName := ExtractFileName(FullFileName);
  ExportForm.FileName := FullFileName;
  ExportForm.ShowModal;
  CloseReason := ExportForm.CloseReason;
  ExportForm.Free;

  case CloseReason of
    ecrError:
      MessageBoxW(Handle, PWideChar(WideString(Gti(2801))),
       PWideChar(WideString(Gti(2802))), MB_OK or MB_ICONERROR);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Wp: TWindowPlacement;
begin
  Wp.length := SizeOf(Wp);
  GetWindowPlacement(Handle, Wp);
  with Wp.rcNormalPosition do
  begin
    MainFrameWidth := Right - Left;
    MainFrameHeight := Bottom - Top;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CloseProj;
end;

procedure TMainForm.LangMenuItemClick(Sender: TObject);
var
  SavedLang: TLanguage;
begin
  SavedLang := Lang;
  TLangForm.Execute;
  if SavedLang <> Lang then
  begin
    LoadTextRes;
    LanguageChanged;
  end;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MemDumpBtnClick(Sender: TObject);
begin
  if not Assigned(MemDumpForm) then
    MemDumpForm := TMemDumpForm.Create(Application);
  MemDumpForm.Visible := not MemDumpForm.Visible;
end;

procedure TMainForm.NewMenuItemClick(Sender: TObject);
begin
  NewProj;
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
begin
  if DirectoryExistsUTF8(CurrPath) then
    OpenDlg.InitialDir := CurrPath;

  OpenDlg.Filter := Gti(183);
  OpenDlg.FilterIndex := 0;
  if not OpenDlg.Execute then
    Exit;

  CurrPath := ExtractFilePath(OpenDlg.FileName);
  Save;

  if not CloseProj then Exit;

  ProjName := OpenDlg.FileName;
  if not FileExistsUTF8(ProjName) then
    if UTF8Length(ExtractFileExt(ProjName)) = 0 then
      ProjName := ProjName + '.xml';
  OpenProj(ProjName);
end;

procedure TMainForm.PageMenuItemClick(Sender: TObject);
begin
  PageSetup(Self);
end;

function TMainForm.PageSetup(Form: TForm): Boolean;
var
  Margins: TRect;
  Dlg: TPageSetupDialog;
begin
  Dlg := TPageSetupDialog.Create(Form);
  Margins.Left := Trunc(100*PageMarginLeft);
  Margins.Top := Trunc(100*PageMarginTop);
  Margins.Right := Trunc(100*PageMarginRight);
  Margins.Bottom := Trunc(100*PageMarginBottom);
//  Dlg.Margins := Margins;
  Dlg.MarginLeft := Margins.Left;
  Dlg.MarginTop  := Margins.Top;
  Dlg.MarginRight := Margins.Right;
  Dlg.MarginBottom := Margins.Bottom;
  Result := Dlg.Execute;
  if Result then
  begin
  //  Margins := Dlg.Margins;
    Margins.Left := Dlg.MarginLeft;
    Margins.Top := Dlg.MarginTop;
    Margins.Right := Dlg.MarginRight;
    Margins.Bottom := Dlg.MarginBottom;
    PageMarginLeft := Margins.Left*0.01;
    PageMarginTop := Margins.Top*0.01;
    PageMarginRight := Margins.Right*0.01;
    PageMarginBottom := Margins.Bottom*0.01;
    Printer.RestoreDefaultBin;
    Save;
  end;
end;

procedure TMainForm.QueryMenuItemClick(Sender: TObject);
begin
  if TQueryForm.Execute(GetDevDataType) then
  begin
    CommitData;
    MakeQuery;
    IncFrame.DataChanged;
    VistFrame.DataChanged;
    IncFrame.GotoFirstRecord;
    VistFrame.GotoFirstRecord;
  end;
end;

procedure TMainForm.ReadMenuItemClick(Sender: TObject);
var
  JobDone: Boolean = False;
  Err: Integer = 0;
  MsgId: Integer = -1;
  TitleId: Integer = -1;
  Flags: Integer = MB_OK;
begin
  CommitData;

  IncDataRead     := 0;
  IncDataAdded    := 0;
  IncDataMatched  := 0;
  VistDataRead    := 0;
  VistDataAdded   := 0;
  VistDataMatched := 0;

  ReadForm := TReadForm.Create(Application);
  if not ReadForm.Opened then
  begin
    MessageBoxW(Handle, PWideChar(WideString(Gti(2102))),
     PWideChar(WideString(Gti(2101))), MB_OK or MB_ICONERROR);
  end
  else
  begin
    ReadForm.ShowModal;
    JobDone := ReadForm.JobDone;
    Err := ReadForm.Error;
  end;
  ReadForm.Free;

  if JobDone then
  begin
    if Assigned(MemDumpForm) then
      if MemDumpForm.Visible then
        MemDumpForm.UpdateView;

    IncFrame.BeginAddData;
    VistFrame.BeginAddData;
    UGlobal.Analyzer.Analyze;
    InfoMenuItemClick(nil);
    if IncDataAdded + VistDataAdded > 0 then
    begin
      DevData.Sort;
      MakeQuery;
      ProjEdited := True;
      SetMenuItemsState;
      IncFrame.EndAddData;
      VistFrame.EndAddData;
    end;
  end
  else
  begin
    case Err of
      0,USER_ABORT: ;
      DATA_CHANGED_ERROR:
      begin
        Flags := Flags or MB_ICONEXCLAMATION;
        MsgId   := 2106;
        TitleId := 2105;
      end;
      WRONG_MODEL:
      begin
        Flags := Flags or MB_ICONERROR;
        MsgId   := 2110;
        TitleId := 2109;
      end;
      WRONG_VERSION,WRONG_DEVICE:
      begin
        Flags := Flags or MB_ICONERROR;
        MsgId   := 2108;
        TitleId := 2107;
      end;
      else
        Flags := Flags or MB_ICONERROR;
        MsgId   := 2104;
        TitleId := 2103;
    end;
    if MsgId > 0 then
      MessageBoxW(Handle, PWideChar(WideString(Gti(MsgId))),
       PWideChar(WideString(Gti(TitleId))), Flags);
    if IncDataRead + VistDataRead > 0 then
      InfoMenuItemClick(nil);
  end;
end;

procedure TMainForm.ReportMenuItemClick(Sender: TObject);
var
  DevDataType: TDevDataType;
begin
  if Printer.Printers.Count = 0 then
  begin
    MessageBoxW(Handle, PWideChar(WideString(Gti(2926))),
     PWideChar(WideString(Gti(2925))), MB_OK or MB_ICONEXCLAMATION);
    Exit;
  end;

  CommitData;
  DevDataType := GetDevDataType;

  if (not DontShowReportOptions) and (DevDataType in [ddtInc,ddtVist]) then
  begin
    if not TReportOptionsForm.Execute(DevDataType) then
      Exit;
  end;

  TReportForm.Execute(DevDataType);
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
begin
  SaveAsProj;
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
begin
  if ProjNameAvailable
    then SaveProj
    else SaveAsProj;
end;

procedure TMainForm.SigmaUnitsComboBoxChange(Sender: TObject);
begin
  SigmaUnits := SigmaUnitsComboBox.ItemIndex;
  IncFrame.LanguageChanged;
end;

procedure TMainForm.LanguageChanged;
begin
  SetProjNameLabel;
  if Assigned(MemDumpForm) then
    MemDumpForm.LanguageChanged;
  FileMenuItem.Caption := Gti(100);
  OptionsMenuItem.Caption := Gti(102);
  HelpMenuItem.Caption := Gti(104);
  NewMenuItem.Caption := Gti(116);
  OpenMenuItem.Caption := Gti(118);
  ReopenMenuItem.Caption := Gti(109);
  ClearFileHistoryMenuItem.Caption := Gti(111);
  CloseMenuItem.Caption := Gti(120);
  SaveMenuItem.Caption := Gti(122);
  SaveAsMenuItem.Caption := Gti(123);
  ExportMenuItem.Caption := Gti(124) + '...';
  InfoMenuItem.Caption := Gti(125);
  ReadMenuItem.Caption := Gti(107);
  QueryMenuItem.Caption := Gti(106);
  DeleteMenuItem.Caption := Gti(101);
  PageMenuItem.Caption := Gti(103);
  ReportMenuItem.Caption := Gti(105);
  ExitMenuItem.Caption := Gti(112);
  LangMenuItem.Caption := Gti(108);
  AboutMenuItem.Caption := Gti(110);
  NewBtn.Hint := Gti(147);
  OpenBtn.Hint := Gti(146);
  CloseBtn.Hint := Gti(145);
  SaveBtn.Hint := Gti(144);
  ExportBtn.Hint := Gti(143);
  ReadBtn.Hint := Gti(142);
  QueryBtn.Hint := Gti(141);
  DeleteBtn.Hint := Gti(140);
  ReportBtn.Hint := Gti(139);
  MemDumpBtn.Hint := Gti(138);
  AddDataBtn.Hint := Gti(137);
  ProjNameFrame.Caption := Gti(126);
  DescriptionTabSheet.Caption := Gti(154);
  IncTabSheet.Caption := Gti(1155);
  VistTabSheet.Caption := Gti(1156);
  SigmaUnitsLbl.Caption := Gti(1167);
  with SigmaUnitsComboBox do
  begin
    Clear;
    Items.Add(SigmaUnitsToStr(0));
    Items.Add(SigmaUnitsToStr(1));
    ItemIndex := SigmaUnits;
  end;
  SaveDlg.Title := Gti(182);
  SaveDlg.Filter := Gti(183);
  ExportDlg.Title := Gti(182);
  ExportDlg.Filter := Gti(191);
  DescriptionFrame1.LanguageChanged;
  IncFrame.LanguageChanged;
  VistFrame.LanguageChanged;
end;

function TMainForm.CloseProj: Boolean;
var
  H: Integer;
  TmpInt: Integer;
  F: Boolean;
begin
  Result := True;

  if not ProjOpened then
    Exit;

  if ProjEdited then
  begin
    TmpInt := MessageBoxW(Handle, PWideChar(WideString(Gti(186))),
     PWideChar(WideString(Gti(185))), MB_YESNOCANCEL or MB_ICONQUESTION);
    case TmpInt of
      IDYES:
      begin
        if ProjNameAvailable
          then F := SaveProj
          else F := SaveAsProj;
        if not F then
        begin
          Result := False;
          Exit;
        end;
      end;

      IDNO:
      begin
      end;

      else
      begin
        Result := False;
        Exit;
      end;
    end
  end;

  case DataPageControl.TabIndex of
    1: H := IncTabSheet.Height;
    2: H := VistTabSheet.Height;
    else H := DescriptionTabSheet.Height;
  end;
  if H > 0 then
  begin
    IncSplitterPos := 100.0*IncFrame.PairSplitter1.Position/H;
    VistSplitterPos := 100.0*VistFrame.PairSplitter1.Position/H;
  end;
  DataPageControl.Visible := False;

  ProjOpened := False;

  if ProjNameAvailable then
  begin
    AddMruMenuItem(ProjName);
    RetitleMruMenuItems;
  end;

  SetProjNameLabel;
  SetMenuItemsState;
end;

function TMainForm.OpenProj(FileName: UTF8String): Boolean;
begin
  Result := False;

  if not DevData.Load(FileName) then
  begin
    MessageBoxW(Handle, PWideChar(WideString(Gti(187) + '"' + FileName + '".')),
     PWideChar(WideString(Gti(181))), MB_ICONERROR or MB_OK);
    Exit;
  end;

  DataPageControl.Visible := True;

  MakeQuery;
  DataChanged;
  GotoFirstRecord;

  ProjOpened := True;
  ProjEdited := False;
  ProjName := FileName;
  ProjNameAvailable := True;
  IncDataRead := 0;
  IncDataMatched := 0;
  IncDataAdded := 0;
  VistDataRead := 0;
  VistDataMatched := 0;
  VistDataAdded := 0;
  SetProjNameLabel;
  SetDataPageControlVisible;
  DeleteMruMenuItem(ProjName);
  RetitleMruMenuItems;
  SetMenuItemsState;
  Result := True;
end;

procedure TMainForm.NewProj;
begin
  if not CloseProj then
    Exit;

  IncDataRead := 0;
  IncDataMatched := 0;
  IncDataAdded := 0;
  VistDataRead := 0;
  VistDataMatched := 0;
  VistDataAdded := 0;

  ProjOpened := True;
  ProjEdited := True;
  ProjName := GenNewProjName;
  ProjNameAvailable := False;

  DevData.Clear;
  MakeQuery;
  GotoFirstRecord;
  SetProjNameLabel;
  SetDataPageControlVisible;
  SetMenuItemsState;
end;

function TMainForm.SaveProj: Boolean;
label
  OnError;
begin
  CommitData;

  if FileExistsUTF8(ProjName) then
  begin
    if not DeleteFileUTF8(ProjName) then
    begin
      Result := False;
      goto OnError;
    end;
  end;

  Result := DevData.Save(ProjName);
  if not Result then
    goto OnError;

  ProjEdited := False;
  SetMenuItemsState;
  Exit;

  OnError:
  begin
    MessageBoxW(Handle, PWideChar(WideString(Gti(180) + '"' + ProjName + '".')),
     PWideChar(WideString(Gti(181))), MB_ICONERROR or MB_OK);
  end;
end;

function TMainForm.SaveAsProj: Boolean;
var
  NewProjName: UTF8String;
begin
  Result := False;

  if UTF8Length(ExtractFilePath(ProjName)) = 0 then
    NewProjName := TrimFileName(CurrPath + PathDelim + ProjName)
  else
    NewProjName := ProjName;
  SaveDlg.InitialDir := ExtractFilePath(NewProjName);
  SaveDlg.FileName := ExtractFileName(NewProjName);
  if not SaveDlg.Execute then
    Exit;
  NewProjName := SaveDlg.FileName;
  if UTF8Length(ExtractFileExt(NewProjName)) = 0 then
    NewProjName := NewProjName + '.xml';
  CurrPath := ExtractFilePath(NewProjName);
  Save;
  if FileExistsUTF8(NewProjName) then
  begin
    if MessageBoxW(Handle, PWideChar(WideString(Gti(184))),
       PWideChar(WideString(Gti(185))),
       MB_YESNOCANCEL or MB_ICONQUESTION) <> IDYES then
      Exit;
  end;

  ProjName := NewProjName;
  ProjNameAvailable := True;
  SetProjNameLabel;
  Result := SaveProj;
end;

procedure TMainForm.SetMenuItemsState;
begin
  CloseMenuItem.Enabled := ProjOpened;
  SaveMenuItem.Enabled := ProjOpened and ProjEdited;
  SaveAsMenuItem.Enabled := ProjOpened;
  ExportMenuItem.Enabled := ProjOpened;
  InfoMenuItem.Enabled := ProjOpened;
  ReadMenuItem.Enabled := ProjOpened;
  QueryMenuItem.Enabled := ProjOpened;
  DeleteMenuItem.Enabled := ProjOpened;
  ReportMenuItem.Enabled := ProjOpened;
  SaveBtn.Enabled := SaveMenuItem.Enabled;
  CloseBtn.Enabled := CloseMenuItem.Enabled;
  ExportBtn.Enabled := ExportMenuItem.Enabled;
  ReadBtn.Enabled := ReadMenuItem.Enabled;
  QueryBtn.Enabled := QueryMenuItem.Enabled;
  DeleteBtn.Enabled := DeleteMenuItem.Enabled;
  ReportBtn.Enabled := ReportMenuItem.Enabled;
  AddDataBtn.Enabled := ProjOpened;
  ClearFileHistoryMenuItem.Enabled := MruList.Count > 0;
end;

procedure TMainForm.SetProjNameLabel;
begin
  if ProjOpened then
  begin
    ProjNameFrame.FileName := ProjName;
    Caption := Gti(1402) + ' [' + ExtractFileName(ProjName) + ']';
  end
  else
  begin
    ProjNameFrame.FileName := '';
    Caption := Gti(1402);
  end;
end;

procedure TMainForm.GotoFirstRecord;
begin
  DescriptionFrame1.DataChanged;
  IncFrame.DataChanged;
  VistFrame.DataChanged;
  IncFrame.GotoFirstRecord;
  VistFrame.GotoFirstRecord;
end;

procedure TMainForm.DataEdited(Sender: TObject);
begin
  SetMenuItemsState;
end;

procedure TMainForm.CommitData;
begin
  DescriptionFrame1.CommitData;
  IncFrame.CommitData;
  VistFrame.CommitData;
end;

procedure TMainForm.DataChanged;
begin
  DescriptionFrame1.DataChanged;
  IncFrame.DataChanged;
  VistFrame.DataChanged;
end;

procedure TMainForm.SetDataPageControlVisible;
var
  H: Integer;
begin
  DataPageControl.Visible := True;
  case DataPageControl.PageIndex of
    1: H := IncTabSheet.Height;
    2: H := VistTabSheet.Height;
    else H := DescriptionTabSheet.Height;
  end;
  IncFrame.PairSplitter1.Position := Trunc(0.01*IncSplitterPos*H);
  VistFrame.PairSplitter1.Position := Trunc(0.01*VistSplitterPos*H);
end;

procedure TMainForm.MruMenuItemClick(Sender: TObject);
var
  Index: Integer;
  NewProjName: UTF8String;
  MenuItem: TMenuItem;
begin
  if not CloseProj then
    Exit;

  MenuItem := Sender as TMenuItem;
  Index := MenuItem.Tag;
  NewProjName := MruList.Strings[Index];
  MruList.Delete(Index);
  FMruMenuItemList.Delete(Index);
  MenuItem.Free;
  RetitleMruMenuItems;
  SetMenuItemsState;
  OpenProj(NewProjName);
end;

procedure TMainForm.RetitleMruMenuItems;
var
  I,N: Integer;
  MenuItem: TMenuItem;
begin
  N := MruList.Count - 1;
  for I := 0 to N do
  begin
    MenuItem := TMenuItem(FMruMenuItemList.Items[I]);
    MenuItem.Caption := '&' + IntToStr(I + 1) + ': ' + MruList.Strings[I];
    MenuItem.Tag := I;
  end;
end;

procedure TMainForm.DeleteMruMenuItem(ProjName: UTF8String);
var
  I: Integer;
begin
  for I := MruList.Count - 1 downto 0 do
  begin
    if UTF8CompareText(ProjName, MruList.Strings[I]) = 0 then
    begin
      MruList.Delete(I);
      TMenuItem(FMruMenuItemList.Items[I]).Free;
      FMruMenuItemList.Delete(I);
    end;
  end;
end;

procedure TMainForm.AddMruMenuItem(ProjName: UTF8String);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  DeleteMruMenuItem(ProjName);

  for I := MruList.Count - 1 downto MruCountMax - 2 do
  begin
    MruList.Delete(I);
    TMenuItem(FMruMenuItemList.Items[I]).Free;
    FMruMenuItemList.Delete(I);
  end;

  MenuItem := TMenuItem.Create(ReopenMenuItem);
  MenuItem.OnClick := @MruMenuItemClick;
  ReopenMenuItem.Insert(0, MenuItem);
  FMruMenuItemList.Insert(0, MenuItem);
  MruList.Insert(0, ProjName);
end;

end.

