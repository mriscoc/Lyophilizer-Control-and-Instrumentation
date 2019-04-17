unit WaitU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TWaitF }

  TWaitF = class(TForm)
    ProgressBar: TProgressBar;
    WaitLabel: TLabel;
  private
    procedure Wait;
    { private declarations }
  public
    { public declarations }
    procedure Step;
    procedure EndStep;
  end;

var
  WaitF: TWaitF;

Procedure ShowWait(Msg:String; pasos:Integer);

implementation

{$R *.lfm}

Procedure ShowWait(Msg:String; pasos:Integer);
Begin
  With WaitF do
  begin
    WaitLabel.Caption:=Msg;
    WaitF.Show;
//    Anim.Animate:=true;
    ProgressBar.Style:=pbstNormal;
    ProgressBar.Position:=0;
    ProgressBar.Max:=pasos;
    ProgressBar.Refresh;
    WaitLabel.Refresh;
  end;
end;

{ TWaitF }

procedure TWaitF.Step;
begin
  ProgressBar.Position:=ProgressBar.Position+1;
  Application.ProcessMessages;
end;

procedure TWaitF.EndStep;
var i:integer;
begin
  ProgressBar.Style:=pbstNormal;
  ProgressBar.Position:=ProgressBar.Max;
  for i:=0 to 10 do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;
end;

procedure TWaitF.Wait;
var i:integer;
begin
  ProgressBar.Style:=pbstMarquee;
  for i:=0 to 10 do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;
end;

end.

