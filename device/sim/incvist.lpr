program incvist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UDeviceThread, UGlobal, UDevice, UArchiveMemForm,
  UMemJumpForm, UMemSearchForm, UMemFillForm,
  runtimetypeinfocontrols, udevicevarsform, UDeviceVars, 
UTypes;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

