unit UGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, UTextRes, UPicRes, Graphics, ULog,
  Shlobj, LazUTF8, LazFileUtils, Zipper, Windows, UCfgFile;

type
  TLanguage = (lRussian, lEnglish);
  TGetTextResIProc = function(Id: Integer): String;
  TGetTextResSProc = function(Id: String): String;
  PObject = ^TObject;

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
  AppBaseName = 'IncvistPicture';
  AppName = AppBaseName;
  AppVer = '1.0';
  ResFileExt = '.resources';
  AppResFileName = AppName + ResFileExt;
  AppCommonResFileName = 'common' + ResFileExt;
  DevScreenWidthDef = 240;
  DevScreenHeightDef = 320;
  DevScreenOutset = 8;
  DevScreenBorderColor = clBlack;
  DevScreenBorderWidth = 1;
  DevScreenColorDef = clWhite;


var
  Lang: TLanguage = lEnglish;

  Gti: TGetTextResIProc;
  Gts: TGetTextResSProc;
  AppCfgPath: UTF8String;
  AppPath: UTF8String;

  ProductId: Integer = -1;
  ProductVersion: Integer = -1;
  ProductModel: Integer = -1;

  CurrPath: UTF8String = '';

  MemDump: PByte = nil;
  MemDumpSize: Integer = 0;

  DevScreenBmpFileNameAvailable: Boolean = False;
  DevScreenBmpFileName: UTF8String = '';

  LogFile: TLogFile = nil;
  LogConsole: TLogConsole = nil;

  DevScreenCachedBmp: Graphics.TBitmap;
  DevScreenBmp: Graphics.TBitmap;

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
function GetDevScreenWidth: Integer;
function GetDevScreenHeight: Integer;
procedure FreeObj(Ptr: PObject);

implementation

const
  {Cfg}
  LanguageCfgName = 'Language';
  CurrPathCfgName = 'Path';

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
begin
  TextRes := TTextRes.Create;
  Gti := @GetTextRes;
  Gts :=  @GetTextRes;
  PicRes := TPicRes.Create;

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

  CreateLogConsole(LogConsoleEnabled);

  AppPath := ExtractFilePath(ParamStrUTF8(0));
  if Length(AppPath) > 0 then
    if AppPath[Length(AppPath)] <> PathDelim then
      AppPath := AppPath + PathDelim;

  DevScreenCachedBmp := nil;
  DevScreenBmp := nil;
end;

procedure Fin;
begin
  Save;
  FreeObj(@TextRes);
  FreeObj(@PicRes);
  if Assigned(MemDump) then
    FreeMem(MemDump);
  FreeObj(@DevScreenCachedBmp);
  FreeObj(@DevScreenBmp);
  FreeObj(@LogFile);
  FreeObj(@LogConsole);
end;

procedure Load;
var
  SplitList: TStringList;
  Txt: UTF8String;
  Found: Boolean;
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


  SplitList.Free;
  CfgFile.Free;
end;

procedure Save;
var
  SplitList: TStringList;
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

begin
  CfgFile := TCfgFile.Create;
  SplitList := TStringList.Create;
  SplitList.Delimiter := ';';

  WriteStr(LanguageCfgName, Capitalize(LangToStr(Lang)));
  WriteStr(CurrPathCfgName, CurrPath);

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

function GetDevScreenWidth: Integer;
begin
  if Assigned(DevScreenBmp) then
    Result := DevScreenBmp.Width
  else
    Result := DevScreenWidthDef;
end;

function GetDevScreenHeight: Integer;
begin
  if Assigned(DevScreenBmp) then
    Result := DevScreenBmp.Height
  else
    Result := DevScreenHeightDef;
end;

procedure FreeObj(Ptr: PObject);
begin
  if Assigned(Ptr) then
  begin
    if Assigned(Ptr^) then
    begin
      Ptr^.Free;
      Ptr^ := nil;
    end;
  end;
end;

initialization
  Init;

finalization
  Fin;

end.

