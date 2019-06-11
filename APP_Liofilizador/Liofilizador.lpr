program Liofilizador;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, anchordockpkg, tachartbgra, MainFormU, uecontrols,
  etpackage, sdposeriallaz, Ch_FrameU, DataU, datamu, CHConfig_FrameU, WaitU
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataM, DataM);
  Application.Run;
end.

