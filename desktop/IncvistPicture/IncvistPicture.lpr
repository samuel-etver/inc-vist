program IncvistPicture;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain,
  { you can add units after this }
  UGlobal;

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  UGlobal.Load;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

