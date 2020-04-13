; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyBaseName  "IncVist-3.0"
#define MainAppPublisher "<MANUFACTURER>"
#define MainAppURL "http://www.<manufacturer>.com"
#define MyPrDir ".."
#define DriverDir "..\incvistUSB"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, cliVims Tools | Generate GUID inside the IDE.)
AppId={{A311FD82-7303-4E45-9D60-EABFFB4D207F}
AppName={cm:MainAppName}
AppVersion ={cm:MainAppVersion}
AppPublisher={#MainAppPublisher}
AppPublisherURL={#MainAppURL}
DefaultDirName={pf}\{cm:MainAppName}
DefaultGroupName={cm:MainAppName}
OutputBaseFilename=incvist-setup
Compression=lzma
SolidCompression=yes
DisableWelcomePage=no

; This installation requires admin priveledges.  This is needed to install
; drivers on windows vista and later.
PrivilegesRequired = admin

; "ArchitecturesInstallIn64BitMode=x64 ia64" requests that the install
; be done in "64-bit mode" on x64 & Itanium, meaning it should use the
; native 64-bit Program Files directory and the 64-bit view of the
; registry. On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64 ia64

[Code]
function IsX64: Boolean;
begin
  Result := Is64BitInstallMode and (ProcessorArchitecture = paX64);
end;

function IsI64: Boolean;
begin
  Result := Is64BitInstallMode and (ProcessorArchitecture = paIA64);
end;

function IsX86: Boolean;
begin
  Result := not IsX64 and not IsI64;
end;

function Is64: Boolean;
begin
  Result := IsX64 or IsI64;
end;

[Languages]
Name: ru; MessagesFile: "compiler:Languages\Russian.isl"
Name: en; MessagesFile: "compiler:Default.isl"

[Messages]
en.BeveledLabel=English
ru.BeveledLabel=Russian

[CustomMessages]
en.Manufacturer=<MANUFACTURER>
ru.Manufacturer=<�������������>
en.MainAppName=INC-VIST-3.0
ru.MainAppName=���-����-3.0
en.PictureAppName=INC-VIST-3.0 (Device screen picture)
ru.PictureAppName=���-����-3.0 (������ � ������ �������)
en.MainAppVersion=1.00 (2020-04-08)
ru.MainAppVersion=1.00 (08.04.2020)
ru.DriverMsg=���������� �������
en.DriverMsg=Install driver
ru.CustomMsg=���������� ���������
en.CustomMsg=Custom installation
ru.InstallingDriverMsg=��������������� ������� (��� ����� ������ ��������� ������) ...
en.InstallingDriverMsg=Installing driver (this may take a few seconds) ...

[Types]
Name: "custom"; Description: {cm:CustomMsg}; Flags: iscustom 

[Tasks]
Name: installdriver; Description: {cm:DriverMsg}

[Files]
Source: "{#MyPrDir}\Incvist\common.resources"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyPrDir}\Incvist\Incvist.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyPrDir}\Incvist\incvist.resources"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyPrDir}\IncvistPicture\IncvistPicture.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#MyPrDir}\IncvistPicture\IncvistPicture.resources"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#DriverDir}\*"; Excludes: "*.exe"; Flags: recursesubdirs; DestDir: "{app}\driver"; Tasks: installdriver
Source: "{#DriverDir}\x86\libusb0_x86.dll"; DestName: "libusb0.dll"; DestDir: "{sys}"; Tasks: installdriver; Flags: uninsneveruninstall replacesameversion restartreplace promptifolder; Check: IsX86;
Source: "{#DriverDir}\amd64\libusb0.dll"; DestDir: "{sys}"; Tasks: installdriver; Flags: uninsneveruninstall replacesameversion restartreplace promptifolder; Check: IsX64;
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{cm:MainAppName}"; Filename: "{app}\Incvist.exe"
Name: "{group}\{cm:PictureAppName}"; Filename: "{app}\IncvistPicture.exe"
Name: "{group}\{cm:UninstallProgram,{cm:MainAppName}}"; Filename: "{uninstallexe}"

[Run]
; invoke libusb's DLL to install the .inf file
Filename: "rundll32"; Parameters: "libusb0.dll,usb_install_driver_np_rundll {app}\driver\incvist.inf"; Tasks: installdriver; StatusMsg: {cm:InstallingDriverMsg}

