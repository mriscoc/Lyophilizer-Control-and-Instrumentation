unit Ch_FrameU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  StdCtrls, ExtCtrls, Graphics;

type

  { TChFrame }

  TChFrame = class(TFrame)
    Ch_Chart: TChart;
    Ch_Serie: TAreaSeries;
    Ch_Val: TLabel;
    Label1: TLabel;
    l_maxmin: TLabel;
    Panel_title: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
  private
    FTitle: string;
    procedure SetTitle(AValue: string);
    { private declarations }
  public
    { public declarations }
    YMax,YMin:Real;
    constructor Create(TheOwner: TComponent); override;
  //  destructor Destroy();override;
    procedure ColorUpd(Cl:TColor);
    procedure PutValue(val:real);
    property Title:string read FTitle write SetTitle;
  end;

implementation

{$R *.lfm}

uses Math;

const
  maxpoints=30;

{ TChFrame }

procedure TChFrame.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Panel_title.Caption:=AValue;
end;

constructor TChFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Ch_Chart.Extent.XMax:=maxpoints;
  Ch_Chart.Extent.UseXMax:=true;
  YMax:=NegInfinity;
  YMin:=Infinity;
end;

procedure TChFrame.ColorUpd(Cl: TColor);
begin
  Shape1.Brush.Color:=Cl;
  Ch_Serie.SeriesColor:=Cl;
  Color:=Cl;
end;

procedure TChFrame.PutValue(val: real);
begin
  Ch_Serie.Add(val);
  if Ch_Serie.Count > maxpoints then
  begin
    Ch_Serie.ListSource.Delete(0);
    Ch_Chart.Extent.UseXMax:=false;
  end;
  if IsNan(val) then exit;
  YMax:=Max(YMax,val);
  YMin:=Min(YMin,val);
  Ch_Val.Caption:=Format('%5.1f',[val]);
  l_maxmin.caption:=Format('%5.1f'#$0d#$0a'%5.1f',[YMax,YMin]);
end;

end.

