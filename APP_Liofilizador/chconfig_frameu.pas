unit CHConfig_FrameU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Graphics, Spin, StdCtrls, ExtCtrls,
  uEKnob;

type

  { TChConfigFrame }

  TChConfigFrame = class(TFrame)
    Bevel1: TBevel;
    FloatSpinEdit1: TFloatSpinEdit;
    Panel_title: TPanel;
    Shape1: TShape;
    uEKnob1: TuEKnob;
    procedure FloatSpinEdit1EditingDone(Sender: TObject);
    procedure uEKnob1Change(Sender: TObject);
  private
    FTitle: string;
    FValue: real;
    procedure SetTitle(AValue: string);
    procedure SetValue(AValue: real);
    { private declarations }
  public
    { public declarations }
    procedure ColorUpd(Cl: TColor);
    property Title:string read FTitle write SetTitle;
    property Value:real read FValue write SetValue;
  end;

implementation

{$R *.lfm}

uses DataU;

{ TChConfigFrame }

procedure TChConfigFrame.uEKnob1Change(Sender: TObject);
begin
  if FValue=uEKnob1.Position then Exit;
  FValue:=uEKnob1.Position;
  FloatSpinEdit1.Value:=FValue;
  AOffset[Self.Tag]:=FValue;
end;

procedure TChConfigFrame.FloatSpinEdit1EditingDone(Sender: TObject);
begin
  uEKnob1.Position:=FloatSpinEdit1.Value;
end;

procedure TChConfigFrame.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Panel_title.Caption:=AValue;
end;

procedure TChConfigFrame.SetValue(AValue: real);
begin
  if FValue=AValue then Exit;
  uEKnob1.Position:=AValue;
end;

procedure TChConfigFrame.ColorUpd(Cl: TColor);
begin
  Shape1.Brush.Color:=Cl;
//  Color:=Cl;
end;


end.

