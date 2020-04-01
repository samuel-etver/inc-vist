program incvist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, laz_fpspreadsheet, lazcontrols, runtimetypeinfocontrols,
  printer4lazarus, UMain, UStartup, UFileName, UAbout, ULang, UGlobal, UInfo,
  UQuery, UPicRes, ULog, UMemDump, UExport, UDescription, UIncView, UVistView,
  URead, UDeviceData, UTypes, UNav, UUsb, UReportOptions, UReport, ULibUsb,
  UAnalyzer, UCfgFile;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  UGlobal.Load;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

