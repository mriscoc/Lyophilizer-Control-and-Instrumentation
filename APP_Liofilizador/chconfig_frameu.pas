unit CHConfig_FrameU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Graphics, Spin,
  StdCtrls, ExtCtrls, MaskEdit, uEKnob;

type

  { TChConfigFrame }

  TChConfigFrame = class(TFrame)
    Bevel1: TBevel;
    EdScale: TFloatSpinEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    Panel_title: TPanel;
    Shape1: TShape;
    uEKnob1: TuEKnob;
    procedure EdScaleChange(Sender: TObject);
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure uEKnob1Change(Sender: TObject);
  private
    FScale: real;
    FTitle: string;
    FOffset: real;
    procedure SetScale(AValue: real);
    procedure SetTitle(AValue: string);
    procedure SetOffset(AValue: real);
    { private declarations }
  public
    { public declarations }
    procedure ColorUpd(Cl: TColor);
    property Title:string read FTitle write SetTitle;
    property Offset:real read FOffset write SetOffset;
    property Scale:real read FScale write SetScale;
  end;

implementation

{$R *.lfm}

uses DataU;

{ TChConfigFrame }

procedure TChConfigFrame.uEKnob1Change(Sender: TObject);
begin
  if FOffset=uEKnob1.Position then Exit;
  FOffset:=uEKnob1.Position;
  FloatSpinEdit1.Value:=FOffset;
  AOffset[Self.Tag]:=FOffset;
end;

procedure TChConfigFrame.FloatSpinEdit1Change(Sender: TObject);
begin
  if FloatSpinEdit1.Value = uEKnob1.Position then exit;
  uEKnob1.Position:=FloatSpinEdit1.Value;
end;

procedure TChConfigFrame.EdScaleChange(Sender: TObject);
begin
  if FScale=EdScale.Value then exit;
  FScale:=EdScale.Value;
  AScale[Self.Tag]:=FScale;
end;

procedure TChConfigFrame.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Panel_title.Caption:=AValue;
end;

procedure TChConfigFrame.SetScale(AValue: real);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  EdScale.Value:=FScale;
end;

procedure TChConfigFrame.SetOffset(AValue: real);
begin
  if FOffset=AValue then Exit;
  uEKnob1.Position:=AValue;
end;

procedure TChConfigFrame.ColorUpd(Cl: TColor);
begin
  Shape1.Brush.Color:=Cl;
//  Color:=Cl;
end;


end.

