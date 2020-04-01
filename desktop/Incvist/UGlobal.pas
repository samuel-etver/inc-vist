unit UGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, UTextRes, UPicRes, Graphics, ULog, UDeviceData,
  Shlobj, LazUTF8, LazFileUtils, UTypes, Zipper, Windows, UAnalyzer, UCfgFile;

type
  TLanguage = (lRussian, lEnglish);
  TGetTextResIProc = function(Id: Integer): String;
  TGetTextResSProc = function(Id: String): String;
  TReportColumn = record
    Id:      Integer;
    Width:   Integer;
    Checked: Boolean;
  end;

  THelperForm = class(TForm)
  private
    FDone: Boolean;
    FFiles: TStringList;
  protected
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure UnzipBegin(FileList: TStringList);
    procedure UnzipEnd;
    procedure UnzipCreateTextResStream(Sender: TObject; var Stream: TStream;
     {%H-}Item: TFullZipFileEntry);
    procedure UnzipTextResDone(Sender: TObject; var Stream: TStream;
     {%H-}Item: TFullZipFileEntry);
    procedure UnzipCreatePicResStream(Sender: TObject; var Stream: TStream;
     {%H-}Item: TFullZipFileEntry);
    procedure UnzipPicResDone(Sender: TObject; var Stream: TStream;
     Item: TFullZipFileEntry);
    property Done: Boolean read FDone write FDone;
  end;

const
  Debug = False;
  LogFileEnabled = Debug;
  LogConsoleEnabled = Debug;
  AppBaseName = 'Incvist';
  AppName = AppBaseName;
  AppVer = '1.0';
  ResFileExt = '.resources';
  AppResFileName = AppName + ResFileExt;
  AppCommonResFileName = 'common' + ResFileExt;
  MruCountMax = 8;
  AlternateColor = $00D2FFFF;

  LetterSigma = #$CF#$83;
  LetterEpsilon = #$CE#$B5;
  LetterDelta = #$CE#$94;

  IncColumnCount = 12;
  VistColumnCount = 11;
  VistObjCount = 2;
  VistTypCount = 3;

  IncColumnIds: array[0..IncColumnCount-1] of Integer = (
     188,  150,  151,  152, 1157,
    1158, 1159, 1160, 1161, 1163,
    1162, 1166
  );
  VistColumnIds: array[0..VistColumnCount-1] of Integer = (
     188,  150,  151,  152, 1159,
    1160, 1161, 1168, 1194, 1169,
    1191
  );
  IncReportColumnWidths: array[0..IncColumnCount-1] of Integer = (
    15, 25, 20, 20, 30,
    25, 20, 20, 20, 15,
    20, 20
  );
  VistReportColumnWidthsEng: array[0..VistColumnCount-1] of Integer = (
    15, 25, 20, 20, 20,
    20, 20, 40, 20, 20,
    25
  );
  VistReportColumnWidthsRus: array[0..VistColumnCount-1] of Integer = (
    15, 25, 20, 20, 20,
    20, 20, 40, 23, 20,
    25
  );

  SigmaUnitsCount = 2;


var
  Lang: TLanguage = lEnglish;
  MainFrameWidth: Integer = 800;
  MainFrameHeight: Integer = 600;
  StartupDialogEnabled: Boolean = True;
  IncSplitterPos: Real = 75.0;
  VistSplitterPos: Real = 75.0;

  Gti: TGetTextResIProc;
  Gts: TGetTextResSProc;
  AppCfgPath: UTF8String;
  AppPath: UTF8String;
  Analyzer: TAnalyzer = nil;

  ProductId: Integer = -1;
  ProductVersion: Integer = -1;
  ProductModel: Integer = -1;

  IncDataRead: Integer = 0;
  IncDataAdded: Integer = 0;
  IncDataMatched: Integer = 0;
  VistDataRead: Integer = 0;
  VistDataAdded: Integer = 0;
  VistDataMatched: Integer = 0;

  SigmaUnits: Integer = 0;
  CurrPath: UTF8String = '';

  MemDump: PByte = nil;
  MemDumpSize: Integer = 0;

  DevData: TDeviceData;
  DevQuery: TDeviceQuery = nil;
  ProjOpened: Boolean = False;
  ProjEdited: Boolean = False;
  ProjNameAvailable: Boolean = False;
  ProjNameCount: Integer = 1;
  ProjName: UTF8String = '';

  MruList: TStringList;

  CalendarFilterEnabled: Boolean = False;
  DateFrom: TWDate = nil;
  DateTo: TWDate = nil;
  TimeFrom: TWTime = nil;
  TimeTo: TWTime = nil;
  IncLenFilterEnabled: Boolean = False;
  IncLenFrom: TWFloat32 = nil;
  IncLenTo: TWFloat32 = nil;
  IncDiameterFilterEnabled: Boolean = False;
  IncDiameterFrom: TWFloat32 = nil;
  IncDiameterTo: TWFloat32 = nil;
  VistObjFilterEnabled: Boolean = False;
  VistObjCheckedList: array of Integer;
  VistTypFilterEnabled: Boolean = False;
  VistTypCheckedList: array of Integer;

  PageMarginLeft: Float32 = 20.0;
  PageMarginTop: Float32 = 10.0;
  PageMarginRight: Float32 = 10.0;
  PageMarginBottom: Float32 = 10.0;
  DontShowReportOptions: Boolean = False;
  IncReportAllFields: Boolean = True;
  VistReportAllFields: Boolean = True;
  IncReportColumns: TList;
  VistReportColumns: TList;

  ExportFileNameAvailable: Boolean = False;
  ExportFileName: UTF8String;

  LogFile: TLogFile = nil;
  LogConsole: TLogConsole = nil;

procedure Load;
procedure Save;

procedure LoadTextRes;
function GetTextRes(Id: Integer): String;
function GetTextRes(Id: String): String;
procedure LoadPicRes;
function GetPicRes(Id: String): TPicture;
function Capitalize(Txt: String): String;
function LangToStr(Lang: TLanguage): String;
procedure AllocMemDump(NewSize: Integer);
function SigmaUnitsToStr(Value: Integer): String;
procedure MakeQuery;
function GenNewProjName: String;
function IsVistTypChecked(Id: Integer): Boolean;
function IsVistObjChecked(Id: Integer): Boolean;
procedure IncSetReportColumnsDef(Columns: TList);
procedure VistSetReportColumnsDef(Columns: TList);

implementation

const
  {Cfg}
  MainFrameWidthCfgName = 'MainFrameWidth';
  MainFrameHeightCfgName = 'MainFrameHeightCfgName';
  LanguageCfgName = 'Language';
  MruCfgName = 'Mru';
  StartupDialogEnabledCfgName = 'StartupDialogEnabled';
  IncSplitterPosCfgName = 'IncSplitterPos';
  VistSplitterPosCfgName = 'VistSplitterPos';
  DateFromCfgName = 'DateFrom';
  DateToCfgName = 'DateTo';
  TimeFromCfgName = 'TimeFrom';
  TimeToCfgName = 'TimeTo';
  IncLenFromCfgName = 'IncLengthFrom';
  IncLenToCfgName = 'IncLengthTo';
  IncDiameterFromCfgName = 'IncDiameterFrom';
  IncDiameterToCfgName = 'IncDiameterTo';
  CurrPathCfgName = 'Path';
  SigmaUnitsCfgName = 'Units';
  IncReportColumnCfgName = 'IncReportColumn';
  VistReportColumnCfgName = 'VistReportColumn';
  VistObjectCheckedListCfgName = 'VistObjectCheckedList';
  VistTypeCheckedListCfgName = 'VistTypeCheckedList';
  PageMarginLeftCfgName = 'PageMarginLeft';
  PageMarginTopCfgName = 'PageMarginTop';
  PageMarginRightCfgName = 'PageMarginRight';
  PageMarginBottomCfgName = 'PageMarginBottom';
  DontShowReportOptionsCfgName = 'DontShowReportOptions';
  IncReportAllFieldsCfgName = 'IncReportAllFields';
  VistReportAllFieldsCfgName = 'VistReportAllFields';

var
  CommonCfgFileName: UTF8String;
  CfgFileName: UTF8String;
  TextRes: TTextRes;
  PicRes: TPicRes;

  procedure CreateLogConsole(Enabled: Boolean); forward;
  procedure CreateLogFile(Enabled: Boolean); forward;

procedure Init;
var
  Buff: PChar;
  I: Integer;
  PColumn: ^TReportColumn;
begin
  TextRes := TTextRes.Create;
  Gti := @GetTextRes;
  Gts :=  @GetTextRes;
  PicRes := TPicRes.Create;

  MruList := TStringList.Create;

  DevData := TDeviceData.Create;
  Buff := AllocMem(4096*SizeOf(WideChar));
  if Assigned(Buff) then
  begin
    if SHGetFolderPathW(0, CSIDL_LOCAL_APPDATA, 0, SHGFP_TYPE_CURRENT,
     PWideChar(Buff)) = S_OK then
    begin
      CurrPath := UTF16toUTF8(WideString(PWideChar(Buff)));
    end;
    FreeMem(Buff);
  end;

  IncReportColumns := TList.Create;
  for I := 0 to IncColumnCount - 1 do
  begin
    New(PColumn);
    IncReportColumns.Add(PColumn);
  end;
  IncSetReportColumnsDef(IncReportColumns);
  VistReportColumns := TList.Create;
  for I := 0 to VistColumnCount - 1 do
  begin
    New(PColumn);
    VistReportColumns.Add(PColumn);
  end;
  VistSetReportColumnsDef(VistReportColumns);

  CreateLogConsole(LogConsoleEnabled);

  AppPath := ExtractFilePath(ParamStrUTF8(0));
  if Length(AppPath) > 0 then
    if AppPath[Length(AppPath)] <> PathDelim then
      AppPath := AppPath + PathDelim;
end;

procedure Fin;
var
  I: Integer;
  PColumn: ^TReportColumn;
begin
  Save;
  TextRes.Free;
  PicRes.Free;
  MruList.Free;
  if Assigned(Analyzer) then
    Analyzer.Free;
  for I := IncReportColumns.Count - 1 downto 0 do
  begin
    PColumn := IncReportColumns.Items[I];
    Dispose(PColumn);
  end;
  IncReportColumns.Free;
  for I := VistReportColumns.Count - 1 downto 0 do
  begin
    PColumn := VistReportColumns.Items[I];
    Dispose(PColumn);
  end;
  VistReportColumns.Free;
  if Assigned(DateFrom) then
    DateFrom.Free;
  if Assigned(DateTo) then
    DateTo.Free;
  if Assigned(TimeFrom) then
    TimeFrom.Free;
  if Assigned(TimeTo) then
    TimeTo.Free;
  if Assigned(MemDump) then
    FreeMem(MemDump);
  if Assigned(DevQuery) then
    DevQuery.Free;
  DevData.Free;
  if Assigned(LogFile) then
    LogFile.Free;
  LogConsole.Free;
end;

procedure Load;
var
  SplitList: TStringList;
  Txt: UTF8String;
  Found: Boolean;
  TmpInt: Integer;
  TmpFloat: Real;
  I,N: Integer;
  Ids: array[0..9] of Integer;
  IdsCount: Integer;
  AppName: UTF8String;
  CfgFile: TCfgFile;

  function ReadStr(const Key: UTF8String; Def: UTF8String = ''): UTF8String;
  begin
    Result := CfgFile.Read(Key, Def);
  end;

  function ReadBool(const Key: UTF8String; Def: Boolean): Boolean;
  begin
    Result := CfgFile.Read(Key, Def);
  end;

  function ReadInt(const Key: UTF8String; Def: Integer): Integer;
  begin
    Result := CfgFile.Read(Key, Def);
  end;

  function ReadFloat(const Key: UTF8String; Def: Real): Real;
  begin
    Result := CfgFile.Read(Key, Def);
  end;

  procedure ReadReportColumns(Key: UTF8String; List: TList);
  var
    I,J,N: Integer;
    PColumn: ^TReportColumn;
    Found: Boolean;
    IntVal: Integer;
  begin
    // Load
    N := List.Count - 1;
    for I := 0 to N do
    begin
      PColumn := List.Items[I];

      SplitList.DelimitedText := ReadStr(Key + IntToStr(I), '');

      try
        IntVal := StrToInt(UTF8Trim(SplitList.Strings[0]));
        if (IntVal >= 0) and (IntVal <= N) then
          PColumn^.Id := IntVal;
      except
      end;

      try
        PColumn^.Checked := StrToBool(UTF8Trim(SplitList.Strings[1]));
      except
      end;

      try
        PColumn^.Width := StrToInt(UTF8Trim(SplitList.Strings[2]));
      except
      end;
    end;

    // Remove duplicate
    for I := 0 to N - 1 do
    begin
      PColumn := List.Items[I];
      IntVal := PColumn^.Id;
      if IntVal < 0 then
        Continue;

      for J := I + 1 to N do
      begin
        PColumn := List.Items[J];
        if PColumn^.Id = IntVal then
          PColumn^.Id := -1;
      end;
    end;

    // Insert absent
    for I := 0 to N do
    begin
      Found := False;
      for J := 0 to N do
      begin
        PColumn := List.Items[J];
        if I = PColumn^.Id then
        begin
          Found := True;
          Break;
        end;
      end;

      if Found then
        Continue;

      for J := 0 to N do
      begin
        PColumn := List.Items[J];
        if PColumn^.Id < 0 then
        begin
          PColumn^.Id := I;
          Break;
        end;
      end;
    end;
  end;

begin
  AppCfgPath := TrimFileName(GetAppConfigDirUTF8(False) + PathDelim + AppVer);
  if not DirectoryExistsUTF8(AppCfgPath) then
    ForceDirectoriesUTF8(AppCfgPath);

  CreateLogFile(LogFileEnabled);

  SplitList := TStringList.Create;
  SplitList.Delimiter := ';';

  AppName := ChangeFileExt(ExtractFileName(Application.ExeName), '');
  CommonCfgFileName := TrimFileName(AppCfgPath + PathDelim + AppBaseName + 'Common.cfg');
  CfgFileName := TrimFileName(AppCfgPath + PathDelim + AppName + '.cfg');

  CfgFile := TCfgFile.Create;
  CfgFile.Load(CfgFileName);

  TmpInt := ReadInt(MainFrameWidthCfgName, MainFrameWidth);
  if TmpInt > 200 then
    MainFrameWidth := TmpInt;
  TmpInt := ReadInt(MainFrameHeightCfgName, MainFrameHeight);
  if TmpInt > 200 then
    MainFrameHeight := TmpInt;
  TmpFloat := ReadFloat(IncSplitterPosCfgName, IncSplitterPos);
  if (TmpFloat >= 0.0) and (TmpFloat <= 100.0) then
    IncSplitterPos := TmpFloat;
  TmpFloat := ReadFloat(VistSplitterPosCfgName, VistSplitterPos);
  if (TmpFloat >= 0.0) and (TmpFloat <= 100.0) then
    VistSplitterPos := TmpFloat;

  StartupDialogEnabled :=
   ReadBool(StartupDialogEnabledCfgName, StartupDialogEnabled);
  TmpInt := ReadInt(SigmaUnitsCfgName, SigmaUnits);
  if (TmpInt >= 0) and (TmpInt < SigmaUnitsCount) then
    SigmaUnits := TmpInt;
  Txt := AnsiLowerCase(ReadStr(LanguageCfgName));
  Found := False;
  for Lang in TLanguage do
  begin
    if Txt = AnsiLowerCase(LangToStr(Lang)) then
    begin
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    case SysLocale.DefaultLCID of
      1049,2073: Lang := lRussian;
    end;
  end;

  CurrPath := UTF8Trim(ReadStr(CurrPathCfgName, ''));
  if Length(CurrPath) = 0 then
    CurrPath := AppCfgPath;

  try
    DateFrom := TWDate.Parse(ReadStr(DateFromCfgName, ''));
  except
  end;

  try
    DateTo := TWDate.Parse(ReadStr(DateToCfgName, ''));
  except
  end;

  try
    TimeFrom := TWTime.Parse(ReadStr(TimeFromCfgName, ''));
  except
  end;

  try
    TimeTo := TWTime.Parse(ReadStr(TimeToCfgName, ''));
  except
  end;

  try
    IncLenFrom := TWFloat32.Parse(ReadStr(IncLenFromCfgName, ''));
  except
  end;

  try
    IncLenTo := TWFloat32.Parse(ReadStr(IncLenToCfgName, ''));
  except
  end;

  try
    IncDiameterFrom := TWFloat32.Parse(ReadStr(IncDiameterFromCfgName, ''));
  except
  end;

  try
    IncDiameterTo := TWFloat32.Parse(ReadStr(IncDiameterToCfgName, ''));
  except
  end;

  SplitList.DelimitedText := ReadStr(VistObjectCheckedListCfgName, '');
  N := SplitList.Count - 1;
  for I := 0 to N do
    SplitList.Strings[I] := UTF8Trim(SplitList.Strings[I]);
  IdsCount := 0;
  if SplitList.IndexOf(IntToStr(VistObjGeneralId)) >= 0 then
  begin
    Ids[IdsCount] := VistObjGeneralId;
    Inc(IdsCount);
  end;
  if SplitList.IndexOf(IntToStr(VistObjShakerTableId)) >= 0 then
  begin
    Ids[IdsCount] := VistObjShakerTableId;
    Inc(IdsCount);
  end;
  SetLength(VistObjCheckedList, IdsCount);
  for I := 0 to IdsCount - 1 do
    VistObjCheckedList[Low(VistObjCheckedList) + I] := Ids[I];

  SplitList.DelimitedText := ReadStr(VistTypeCheckedListCfgName, '');
  N := SplitList.Count - 1;
  for I := 0 to N do
    SplitList.Strings[I] := UTF8Trim(SplitList.Strings[I]);
  IdsCount := 0;
  if SplitList.IndexOf(IntToStr(VistTypSId)) >= 0 then
  begin
    Ids[IdsCount] := VistTypSId;
    Inc(IdsCount);
  end;
  if SplitList.IndexOf(IntToStr(VistTypVId)) >= 0 then
  begin
    Ids[IdsCount] := VistTypVId;
    Inc(IdsCount);
  end;
  SetLength(VistTypCheckedList, IdsCount);
  for I := 0 to IdsCount - 1 do
    VistTypCheckedList[Low(VistTypCheckedList) + I] := Ids[I];

  TmpFloat := ReadFloat(PageMarginLeftCfgName, PageMarginLeft);
  if TmpFloat >= 0.0 then
    PageMarginLeft := TmpFloat;
  TmpFloat := ReadFloat(PageMarginTopCfgName, PageMarginTop);
  if TmpFloat >= 0.0 then
    PageMarginTop := TmpFloat;
  TmpFloat := ReadFloat(PageMarginRightCfgName, PageMarginRight);
  if TmpFloat >= 0.0 then
    PageMarginRight := TmpFloat;
  TmpFloat := ReadFloat(PageMarginBottomCfgName, PageMarginBottom);
  if TmpFloat >= 0.0 then
    PageMarginBottom := TmpFloat;
  DontShowReportOptions :=
   ReadBool(DontShowReportOptionsCfgName, DontShowReportOptions);
  IncReportAllFields := ReadBool(IncReportAllFieldsCfgName, IncReportAllFields);
  VistReportAllFields := ReadBool(VistReportAllFieldsCfgName, VistReportAllFields);

  ReadReportColumns(IncReportColumnCfgName, IncReportColumns);
  ReadReportColumns(VistReportColumnCfgName, VistReportColumns);

  for I := 0 to MruCountMax - 1 do
  begin
    Txt := UTF8Trim(ReadStr(MruCfgName + IntToStr(I), ''));
    if UTF8Length(Txt) > 0 then
      MruList.Add(Txt);
  end;


  SplitList.Free;
  CfgFile.Free;
end;

procedure Save;
var
  SplitList: TStringList;
  I,N: Integer;
  CfgFile: TCfgFile;

  procedure WriteStr(const Key: UTF8String; Val: UTF8String);
  begin
    CfgFile.Write(Key, Val);
  end;

  procedure WriteBool(const Key: UTF8String; Val: Boolean);
  begin
    CfgFile.Write(Key, Val);
  end;

  procedure WriteInt(const Key: UTF8String; Val: Integer);
  begin
    CfgFile.Write(Key, Val);
  end;

  procedure WriteFloat(const Key: UTF8String; Val: Real);
  begin
    CfgFile.Write(Key, Val);
  end;

  procedure WriteReportColumns(const Key: UTF8String; List: TList);
  var
    I: Integer;
    N: Integer;
    PColumn: ^TReportColumn;
  begin
    N := List.Count - 1;
    for I := 0 to N do
    begin
      PColumn := List.Items[I];
      SplitList.Clear;
      SplitList.Add(IntToStr(PColumn^.Id));
      SplitList.Add(BoolToStr(PColumn^.Checked, True));
      SplitList.Add(IntToStr(PColumn^.Width));
      CfgFile.Write(Key + IntToStr(I), SplitList.DelimitedText);
    end;
  end;

begin
  CfgFile := TCfgFile.Create;
  SplitList := TStringList.Create;
  SplitList.Delimiter := ';';

  WriteInt(MainFrameWidthCfgName, MainFrameWidth);
  WriteInt(MainFrameHeightCfgName, MainFrameHeight);
  WriteFloat(IncSplitterPosCfgName, IncSplitterPos);
  WriteFloat(VistSplitterPosCfgName, VistSplitterPos);
  WriteBool(StartupDialogEnabledCfgName, StartupDialogEnabled);
  WriteStr(LanguageCfgName, Capitalize(LangToStr(Lang)));
  WriteInt(SigmaUnitsCfgName, SigmaUnits);
  WriteStr(CurrPathCfgName, CurrPath);
  WriteStr(DateFromCfgName, TWDate.ToStr(DateFrom));
  WriteStr(DateToCfgName, TWDate.ToStr(DateTo));
  WriteStr(TimeFromCfgName, TWTime.ToStr(TimeFrom));
  WriteStr(TimeToCfgName, TWTime.ToStr(TimeTo));
  WriteStr(IncLenFromCfgName, TWFloat32.ToStr(IncLenFrom));
  WriteStr(IncLenToCfgName, TWFloat32.ToStr(IncLenTo));
  WriteStr(IncDiameterFromCfgName, TWFloat32.ToStr(IncDiameterFrom));
  WriteStr(IncDiameterToCfgName, TWFloat32.ToStr(IncDiameterTo));

  SplitList.Clear;
  N := High(VistObjCheckedList);
  for I := Low(VistObjCheckedList) to N do
    SplitList.Add(IntToStr(VistObjCheckedList[I]));
  WriteStr(VistObjectCheckedListCfgName, SplitList.DelimitedText);

  SplitList.Clear;
  N := High(VistTypCheckedList);
  for I := Low(VistTypCheckedList) to N do
    SplitList.Add(IntToStr(VistTypCheckedList[I]));
  WriteStr(VistTypeCheckedListCfgName, SplitList.DelimitedText);

  WriteFloat(PageMarginLeftCfgName, PageMarginLeft);
  WriteFloat(PageMarginTopCfgName, PageMarginTop);
  WriteFloat(PageMarginRightCfgName, PageMarginRight);
  WriteFloat(PageMarginBottomCfgName, PageMarginBottom);
  WriteBool(DontShowReportOptionsCfgName, DontShowReportOptions);
  WriteBool(IncReportAllFieldsCfgName, IncReportAllFields);
  WriteBool(VistReportAllFieldsCfgName, VistReportAllFields);

  N := MruList.Count - 1;
  for I := 0 to N do
    WriteStr(MruCfgName + IntToStr(I), MruList.Strings[I]);
  for I := N + 1 to MruCountMax - 1 do
    WriteStr(MruCfgName + IntToStr(I), '');

  WriteReportColumns(IncReportColumnCfgName, IncReportColumns);
  WriteReportColumns(VistReportColumnCfgName, VistReportColumns);

  CfgFile.Save(CfgFileName);

  SplitList.Free;
  CfgFile.Free;
end;

function LangToStr(Lang: TLanguage): String;
begin
  case Lang of
    lRussian: Result := 'russian';
    lEnglish: Result := 'english';
    else      Result := '';
  end;
end;

procedure LoadTextRes;
const
  ResFileNames: array[0..1] of UTF8String = (
    AppCommonResFileName, AppResFileName
  );
  AltResFolders: array[0..1] of UTF8String = (
    '..' + PathDelim + 'CommonResources', 'Resources'
  );
var
  HelperForm: THelperForm;
  Z: TUnZipper = nil;
  FileList: TStringList = nil;
  ResFileName: UTF8String;
  I: Integer;
begin
  TextRes.Clear;

  for I := Low(ResFileNames) to High(ResFileNames) do
  begin
    ResFileName := AppPath + ResFileNames[I];
    if not FileExistsUTF8(ResFileName) then
    begin
      ResFileName :=
       AppPath + AltResFolders[I] + PathDelim + LangToStr(Lang) + '.txt';
      TextRes.LoadFromFile(ResFileName);
    end
    else
    begin
      if not Assigned(FileList) then
      begin
        FileList := TStringList.Create;
        FileList.Add('resources/' + LangToStr(Lang) + '.txt');
      end;
      HelperForm := THelperForm.CreateNew(Application);
      Z := TUnZipper.Create;
      try
        Z.FileName := ResFileName;
        Z.OnDoneStream := @HelperForm.UnzipTextResDone;
        Z.OnCreateStream := @HelperForm.UnzipCreateTextResStream;
        HelperForm.UnzipBegin(FileList);
        Z.UnZipFiles(FileList);
        HelperForm.UnzipEnd;
      except
      end;
      HelperForm.Free;
      Z.Free;
    end;
  end;

  if Assigned(FileList) then
    FileList.Free;
end;

function GetTextRes(Id: Integer): String;
begin
  Result := GetTextRes(IntToStr(Id));
end;

function GetTextRes(Id: String): String;
begin
  Result := TextRes.Values[Id];
end;

procedure LoadPicRes;
var
  LangI: TLanguage;
  Key: String;
  HelperForm: THelperForm;
  Z: TUnZipper;
  FileList: TStringList = nil;
  ResFileName: UTF8String;

function CreateKey: String;
begin
  Result := 'flag-' + LangToStr(LangI) + '.png';
end;

begin
  PicRes.Clear;

  ResFileName := AppPath + AppCommonResFileName;
  if not FileExistsUTF8(ResFileName) then
  begin
    for LangI in TLanguage do
    begin
      Key := CreateKey;
      PicRes.Add(Key, AppPath + '..' + PathDelim + 'CommonResources' +
       PathDelim + Key);
    end;
  end
  else
  begin
    HelperForm := THelperForm.CreateNew(Application);
    if not Assigned(FileList) then
    begin
      FileList := TStringList.Create;
      for LangI in TLanguage do
      begin
        Key := CreateKey;
        FileList.Add(Key);
        FileList.Add('resources/' + Key);
      end;
    end;
    Z := TUnZipper.Create;
    try
      Z.FileName := ResFileName;
      Z.OnCreateStream := @HelperForm.UnzipCreatePicResStream;
      Z.OnDoneStream := @HelperForm.UnzipPicResDone;
      HelperForm.UnzipBegin(FileList);
      Z.UnZipFiles(FileList);
      HelperForm.UnzipEnd;
    except
    end;
    Z.Free;
    HelperForm.Free;
  end;

  if Assigned(FileList) then
    FileList.Free;
end;

function GetPicRes(Id: String): TPicture;
begin
  Result := PicRes.Pics[Id];
end;

function Capitalize(Txt: String): String;
begin
  if Length(Txt) = 0 then
    Result := 'txt'
  else
    Result := UpperCase(Txt[1]) + LowerCase(Copy(Txt, 2));
end;

{ THelperForm }
constructor THelperForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FDone := True;
  FFiles := TStringList.Create;
end;

destructor THelperForm.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure THelperForm.UnzipCreateTextResStream(Sender: TObject; var Stream: TStream;
 Item: TFullZipFileEntry);
begin
  Stream := TMemoryStream.Create;
end;

procedure THelperForm.UnzipTextResDone(Sender: TObject; var Stream: TStream;
 Item: TFullZipFileEntry);
begin
  Stream.Position := 0;
  TextRes.LoadFromStream(Stream);
  Stream.Free;
  FFiles.Clear;
  FDone := True;
end;

procedure THelperForm.UnzipCreatePicResStream(Sender: TObject; var Stream: TStream;
 Item: TFullZipFileEntry);
begin
  Stream := TMemoryStream.Create;
end;

procedure THelperForm.UnzipPicResDone(Sender: TObject; var Stream: TStream;
 Item: TFullZipFileEntry);
var
  I: Integer;
  Key: String;
begin
  Stream.Position := 0;
  I := FFiles.IndexOf(Item.ArchiveFileName);
  if I >= 0 then
  begin
    Key := FFiles.Strings[I - 1];
    FFiles.Delete(I);
    FFiles.Delete(I - 1);
    PicRes.Add(Key, Stream);
  end;
  Stream.Free;
  if FFiles.Count = 0 then
    FDone := True;
end;

procedure THelperForm.UnzipBegin(FileList: TStringList);
var
  I,N: Integer;
begin
  FDone := True;
  FFiles.Clear;
  if Assigned(FileList) then
  begin
    N := FileList.Count - 1;
    if N >= 0 then
    begin
      FDone := False;
      for I := 0 to N do
        FFiles.Add(FileList.Strings[I]);
    end;
  end;
end;

procedure THelperForm.UnzipEnd;
begin
  while not Done do
    Application.ProcessMessages;
end;

function SigmaUnitsToStr(Value: Integer): String;
begin
  case Value of
    0: Result := Gti(1164);
    1: Result := Gti(1165);
    else Result := '';
  end;
end;

procedure MakeQuery;
var
  NewQuery: TDeviceQuery;
  OldQuery: TDeviceQuery;
  IncItem: TIncItem;
  VistItem: TVistItem;
  I,J,N: Integer;
  Dt: TWDate;
  Tm: TWTime;
  IncLen: TWFloat32;
  IncDiam: TWInt;
  Id: TWint;
  Found: Boolean;
begin
  NewQuery := TDeviceQuery.Create;

  NewQuery.Description.Text := DevData.Description.Text;

  with DevData.IncData do
  begin
    N := Count - 1;
    for I := 0 to N do
    begin
      IncItem := Items[I];

      if CalendarFilterEnabled then
      begin
        Dt := IncItem.Date;
        if not Assigned(Dt) then
          Continue;
        if Assigned(DateFrom) then
          if DateFrom.Value > Dt.Value then
            Continue;
        if Assigned(DateTo) then
          if DateTo.Value < Dt.Value then
            Continue;

        Tm := IncItem.Time;
        if not Assigned(Tm) then
          Continue;
        if Assigned(TimeFrom) then
          if TimeFrom.Value > Tm.Value then
            Continue;
        if Assigned(TimeTo) then
          if TimeTo.Value < Tm.Value then
            Continue;
      end;

      if IncLenFilterEnabled then
      begin
        IncLen := IncItem.Len;
        if not Assigned(IncLen) then
          Continue;
        if Assigned(IncLenFrom) then
          if IncLen.Value < IncLenFrom.Value then
            Continue;
        if Assigned(IncLenTo) then
          if IncLen.Value > IncLenTo.Value then
            Continue;
      end;

      if IncDiameterFilterEnabled then
      begin
        IncDiam := IncItem.Diameter;
        if not Assigned(IncDiam) then
          Continue;
        if Assigned(IncDiameterFrom) then
          if IncDiam.Value < IncDiameterFrom.Value then
            Continue;
        if Assigned(IncDiameterTo) then
          if IncDiam.Value > IncDiameterTo.Value then
            Continue;
      end;

      NewQuery.IncData.Add(IncItem);
    end;
  end;

  with DevData.VistData do
  begin
    N := Count - 1;
    for I := 0 to N do
    begin
      VistItem := Items[I];

      if CalendarFilterEnabled then
      begin
        Dt := VistItem.Date;
        if not Assigned(Dt) then
          Continue;
        if Assigned(DateFrom) then
          if DateFrom.Value > Dt.Value then
            Continue;
        if Assigned(DateTo) then
          if DateTo.Value < Dt.Value then
            Continue;

        Tm := VistItem.Time;
        if not Assigned(Tm) then
          Continue;
        if Assigned(TimeFrom) then
          if TimeFrom.Value > Tm.Value then
            Continue;
        if Assigned(TimeTo) then
          if TimeTo.Value < Tm.Value then
            Continue;
      end;

      if VistObjFilterEnabled then
      begin
        Id := VistItem.Obj;
        if not Assigned(Id) then
          Continue;
        Found := False;
        for J := Low(VistObjCheckedList) to High(VistObjCheckedList) do
        begin
          Found := VistObjCheckedList[J] = Id.Value;
          if Found then
            Break;
        end;
        if not Found then
          Continue;
      end;

      if VistTypFilterEnabled then
      begin
        Id := VistItem.Typ;
        if not Assigned(Id) then
          Continue;
        Found := False;
        for J := Low(VistTypCheckedList) to High(VistTypCheckedList) do
        begin
          Found := VistTypCheckedList[J] = Id.Value;
          if Found then Break;
        end;
        if not Found then
          Continue;
      end;

      NewQuery.VistData.Add(VistItem);
    end;
  end;

  OldQuery := DevQuery;
  DevQuery := NewQuery;
  if Assigned(OldQuery) then
    OldQuery.Free;
end;

function GenNewProjName: String;
begin
  Result := Gti(127) + IntToStr(ProjNameCount) + '.xml';
  Inc(ProjNameCount);
end;

function IsVistTypChecked(Id: Integer): Boolean;
var
  I: Integer;
begin
  for I := Low(VistTypCheckedList) to High(VistTypCheckedList) do
  begin
    if VistTypCheckedList[I] = Id then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function IsVistObjChecked(Id: Integer): Boolean;
var
  I: Integer;
begin
  for I := Low(VistObjCheckedList) to High(VistObjCheckedList) do
  begin
    if VistObjCheckedList[I] = Id then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure IncSetReportColumnsDef(Columns: TList);
var
  I,N: Integer;
  PColumn: ^TReportColumn;
begin
  N := Columns.Count - 1;
  for I := 0 to N do
  begin
    PColumn := Columns.Items[I];
    PColumn^.Id := I;
    PColumn^.Checked := True;
    PColumn^.Width := IncReportColumnWidths[I];
  end;
end;

procedure VistSetReportColumnsDef(Columns: TList);
var
  I,N: Integer;
  PColumn: ^TReportColumn;
begin
  N := Columns.Count - 1;
  for I := 0 to N do
  begin
    PColumn := Columns.Items[I];
    PColumn^.Id := I;
    PColumn^.Checked := True;
    if Lang = lRussian then
      PColumn^.Width := VistReportColumnWidthsRus[I]
    else
      PColumn^.Width := VistReportColumnWidthsEng[I];
  end;
end;

procedure AllocMemDump(NewSize: Integer);
var
  I: Integer;
begin
  if NewSize = MemDumpSize then
    Exit;

  if NewSize <= 0 then
  begin
    if Assigned(MemDump) then
      FreeMem(MemDump);
    MemDump := nil;
    MemDumpSize := 0;
  end
  else
  begin
    if Assigned(MemDump) then
      FreeMem(MemDump);
    MemDump := nil;
    MemDumpSize := 0;
    MemDump := GetMem(NewSize);
    MemDumpSize := NewSize;
    for I := 0 to MemDumpSize - 1 do
      MemDump[I] := $FF;
  end;
end;


procedure CreateLogConsole(Enabled: Boolean);
begin
  LogConsole := TLogConsole.Create;
  if Enabled then
    LogConsole.Open;
end;

procedure CreateLogFile(Enabled: Boolean);
begin
  LogFile := TLogFile.Create(ChangeFileExt(Application.ExeName, '.log'));
  if Enabled then
    LogFile.Open(False);
end;

initialization
  Init;

finalization
  Fin;

end.

