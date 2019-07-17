unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAIntervalSources, TAGraph, TASeries, TATransformations,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons,
  ComCtrls, DBGrids, ActnList, IniPropStorage, DbCtrls, sqlite3conn, sqldb, db,
  ueled, TATools, TADataTools, TADbSource, LazFileUtils, Types,lclintf,
  TACustomSource, TAGUIConnectorBGRA, fptimer;

type

  { TMainForm }

  TMainForm = class(TForm)
    B_TaraCh4: TBitBtn;
    B_Export: TBitBtn;
    BSendSetP: TBitBtn;
    ed_SetPointH: TFloatSpinEdit;
    ed_SetPointL: TFloatSpinEdit;
    Label10: TLabel;
    Label9: TLabel;
    LineSetTH: TConstantLine;
    LineSetTL: TConstantLine;
    WeightChart: TChart;
    MainTic: TFPTimer;
    ACreateDB: TAction;
    AReconectHW: TAction;
    ActionL: TActionList;
    AxisTimeUserDefinedAxisTransform1: TUserDefinedAxisTransform;
    BClearAll: TBitBtn;
    B_Connect: TBitBtn;
    B_EnableDB: TBitBtn;
    B_OpenDB: TBitBtn;
    B_SavePrj: TButton;
    AxisTime: TChartAxisTransformations;
    ChartGUIConnectorBGRA1: TChartGUIConnectorBGRA;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ConfigPanel: TPanel;
    DataSource: TDataSource;
    Panel3: TPanel;
    P_Table: TPanel;
    PrjDataS: TDataSource;
    Ed_PrjName: TDBEdit;
    DBGrid1: TDBGrid;
    Ed_PrjDescrip: TDBMemo;
    Ed_COMPort: TSpinEdit;
    GraphSheet: TTabSheet;
    ImageList1: TImageList;
    IniStor: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LED_Con: TuELED;
    LED_DB: TuELED;
    MainChart: TChart;
    OpenDB: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    L_Time: TStaticText;
    Ed_Uplimit: TSpinEdit;
    Ed_Lolimit: TSpinEdit;
    SaveDialog: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Ed_Sampletime: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    VDataSource: TDataSource;
    TimeSource: TDateTimeIntervalChartSource;
    Data_Panel: TPanel;
    ChPanel: TPanel;
    Control_Panel: TPanel;
    RegdataTimer: TTimer;
    procedure ACreateDBExecute(Sender: TObject);
    procedure AReconectHWExecute(Sender: TObject);
    procedure BClearAllClick(Sender: TObject);
    procedure BSendSetPClick(Sender: TObject);
    procedure B_ConnectClick(Sender: TObject);
    procedure B_EnableDBClick(Sender: TObject);
    procedure B_ExportClick(Sender: TObject);
    procedure B_OpenDBClick(Sender: TObject);
    procedure B_SavePrjClick(Sender: TObject);
    procedure B_TaraCh4Click(Sender: TObject);
    procedure ChartToolset1DataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DbChartGetItem(ASender: TDbChartSource; var AItem: TChartDataItem);
    procedure Ed_SampletimeChange(Sender: TObject);
    procedure ed_SetPointHChange(Sender: TObject);
    procedure ed_SetPointLChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MainTicTimer(Sender: TObject);
    procedure Ed_COMPortChange(Sender: TObject);
    procedure Ed_LolimitChange(Sender: TObject);
    procedure Ed_UplimitChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RegdataTimerTimer(Sender: TObject);
  private
    { private declarations }
    procedure Connect;
    procedure CreateChFrames;
    procedure CreateChSeries;
    procedure DeleteChSeries;
    procedure DisableDB;
    procedure Disconnect;
    procedure EnableDB;
    function GetConfigValues: boolean;
    procedure GetData;
    procedure InitHardware;
    function SaveConfigValues: boolean;
    procedure UpdateDsply;
    procedure ResetData;
    procedure VDatasetAfterOpen(DataSet: TDataSet);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses Ch_FrameU, CHConfig_FrameU, datamu, DataU, HWU, WaitU, VersionSupportU, Math;

var
  AChannels: Array [1..HWchannels] of TChFrame;
  ACF: Array [1..HWchannels] of TChConfigFrame;
  ASeries: Array [1..HWchannels] of TlineSeries;

{ TMainForm }

procedure TMainForm.B_ConnectClick(Sender: TObject);
begin
  If MainTic.Enabled then DisConnect else Connect;
end;

procedure TMainForm.Connect;
begin
  if (not Simulate) and (HW<>nil) then HW.Purge;
  LED_Con.Color:=IFTHEN(Simulate,clAqua,clLime);
  B_Connect.Caption:='Disconnect';
  B_Connect.Glyph:=nil;
  ImageList1.GetBitmap(1,B_Connect.Glyph);
  B_EnableDB.Enabled:=true;
  MainTic.Enabled:=true;
  ChartToolset1DataPointCrosshairTool1.Enabled:=false;
  ChartToolset1DataPointHintTool1.Enabled:=false;
end;


procedure TMainForm.Disconnect;
begin
  MainTic.Enabled:=false;
  LED_Con.Color := clRed;
  B_Connect.Caption:='Connect';
  B_Connect.Glyph:=nil;
  ImageList1.GetBitmap(0,B_Connect.Glyph);
  DisableDB;
  B_EnableDB.Enabled:=false;
  ChartToolset1DataPointCrosshairTool1.Enabled:=true;
  ChartToolset1DataPointHintTool1.Enabled:=true;
end;

procedure TMainForm.ACreateDBExecute(Sender: TObject);
begin
  DBName:=FormatDateTime('YYYYMMDD-hhnnss',Now)+'.db';
  if DataM.CreateDB(DBPath+DBName,nChannels) then
  begin
    DataM.VDataset.AfterOpen:=@VDatasetAfterOpen;
    DataM.ReOpenDataSets;
  end;
end;

procedure TMainForm.AReconectHWExecute(Sender: TObject);
begin
  Disconnect;
  If Assigned(HW) then freeandnil(HW);
  InitHardware;
end;

procedure TMainForm.BClearAllClick(Sender: TObject);
begin
  DisableDB;
  DataM.ClearAll;
  MainChart.Invalidate;
  WeightChart.Invalidate;
end;

procedure TMainForm.BSendSetPClick(Sender: TObject);
var
 sph,spl:string;
begin
  if simulate then exit;
  sph:=rightstr(inttohex(round((ed_SetPointH.Value)*100),4),4);
  spl:=rightstr(inttohex(round((ed_SetPointL.Value)*100),4),4);
  HW.senddata('C'+sph+spl);
end;

procedure TMainForm.B_EnableDBClick(Sender: TObject);
begin
  If RegdataTimer.Enabled then DisableDB else EnableDB;
end;

procedure TMainForm.B_ExportClick(Sender: TObject);
begin
  SaveDialog.Filter:='Excel file|*.xlsx';
  SaveDialog.DefaultExt:='.xlsx';
  SaveDialog.InitialDir:=DBPath;
  SaveDialog.FileName:=ExtractFileNameOnly(DBName);
  if SaveDialog.Execute then DataM.DatasetToExcel(DataM.DataSet,SaveDialog.FileName);
  if FileExists(SaveDialog.FileName) then OpenDocument(SaveDialog.FileName);
end;

procedure TMainForm.B_OpenDBClick(Sender: TObject);
var chans:Integer;
begin
  DataM.VDataset.AfterOpen:=@VDatasetAfterOpen;
  Chans:=0;
  OpenDB.InitialDir:=DBPath;
  if OpenDB.Execute then
  begin
    chans:=DataM.OpenDataBase(OpenDB.FileName);
    if Chans>0 then
    begin
      DBPath:=AppendPathDelim(ExtractFilePath(OpenDB.FileName));
      DBName:=ExtractFileName(OpenDB.FileName);
    end else ShowMessage('Database file has not data');
  end;
end;

procedure TMainForm.B_SavePrjClick(Sender: TObject);
begin
  if (DataM.PrjDataset.State=dsEdit) or (DataM.PrjDataset.State=dsInsert) then
  begin
    DataM.PrjDataset.FieldByName('DateTime').AsDateTime:=Now;
    DataM.PrjDataset.Post;
  end;
end;

procedure TMainForm.B_TaraCh4Click(Sender: TObject);
var i:integer;
begin
  B_TaraCh4.Enabled:=false;
  ACF[nChannels].Offset:=0;
  For i:=0 to 100 do begin
    sleep(10);
    Application.ProcessMessages;
  end;
  ACF[nChannels].Offset:=-1 * AData[nChannels];
  B_TaraCh4.Enabled:=true;
end;

procedure TMainForm.ChartToolset1DataPointHintTool1Hint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
begin
  AHint:=Format('%.1f',[ATool.NearestGraphPoint.y]);
end;

procedure TMainForm.DbChartGetItem(ASender: TDbChartSource;
  var AItem: TChartDataItem);
begin
  ASender.DefaultGetItem(AItem);
  AItem.Y:=IFThen((AItem.Y>=LoLimit) and (AItem.Y<=UpLimit),AItem.Y,NaN);
end;

procedure TMainForm.Ed_SampletimeChange(Sender: TObject);
begin
  RegdataTimer.Interval:=Ed_Sampletime.Value*1000;
end;

procedure TMainForm.ed_SetPointHChange(Sender: TObject);
begin
  LineSetTH.Position:=ed_SetPointH.Value;
end;

procedure TMainForm.ed_SetPointLChange(Sender: TObject);
begin
  LineSetTL.Position:=ed_SetPointL.Value;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  UpLimit:=Ed_UpLimit.Value;
  LoLimit:=Ed_LoLimit.Value;
  ComPort:=Ed_ComPort.Value;
end;

//// Compatibilidad con MDA
procedure TMainForm.MainTicTimer(Sender: TObject);
begin
  if (HW<>nil) then
  begin
    if HW.Actualizar then
    begin
      GetData;
      UpdateDsply;
    end;
    if (not simulate) and HW.error then
    begin
      Disconnect;
      MessageDlg('No se puede establecer comunicación con el dispositivo', mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.GetData;
var
  i: Integer;
begin
  for i:=1 to nChannels do AData[i]:=HW.Temperatura[i];
end;

procedure TMainForm.Ed_COMPortChange(Sender: TObject);
begin
  Disconnect;
  COMPort:=Ed_COMPort.Value;
//  Serial.Device := OSSerialDev + Ed_COMPort.Text;
end;

procedure TMainForm.Ed_LolimitChange(Sender: TObject);
begin
  LoLimit:=Ed_Lolimit.Value;
end;

procedure TMainForm.Ed_UplimitChange(Sender: TObject);
begin
  UpLimit:=Ed_Uplimit.Value;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  DataM.CloseDataBase;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WaitF:=TWaitF.Create(Self);
  versionst:=GetFileVersion;
  caption:=MainTitle+' v'+versionst;
  GetConfigValues;
  ResetData;
  MainTic := TFPTimer.create(self);
  MainTic.enabled := false;  // if you want to start it later
  MainTic.UseTimerThread := true;
  MainTic.interval := 100;  // optional here, could be set later
  MainTic.OnTimer := @MainTicTimer;
  CreateChFrames;
  CreateChSeries;
  DisableDB;
  //Compatibilidad con MDA
  InitHardware;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MainTic.Enabled:=false;
  SaveConfigValues;
  MainTic.Free;
  if HW<>nil then FreeAndNil(HW);
  If WaitF<>nil then WaitF.Free;
end;

procedure TMainForm.RegdataTimerTimer(Sender: TObject);
begin
  If DataM.DataSet.Active then
  begin
    DataM.InsertData(AData);
    DataM.VDataSet.Refresh;
    MainChart.LeftAxis.Range.Max := HW.YMax + 2;
    MainChart.LeftAxis.Range.Min := HW.YMin - 2;
    WeightChart.LeftAxis.Range.Max := HW.WMax + 1;
    WeightChart.LeftAxis.Range.Min := HW.WMin - 1;
  end;
end;

procedure TMainForm.CreateChFrames;
Var
  i:integer;
  chcolor:TColor;

begin
  for i:=1 to nChannels do
  begin
    chcolor:=DefColor[i]; //Random($FFFFFF);
//
    AChannels[i]:=TChFrame.Create(ChPanel);
    AChannels[i].Name:='Ch'+inttostr(i)+'_Frame';
    AChannels[i].Parent:=ChPanel;
    AChannels[i].Title:=ALabel[i];
    AChannels[i].Align:=alNone;
    AChannels[i].ColorUpd(chcolor);
//
    ACF[i]:=TChConfigFrame.Create(ConfigPanel);
    ACF[i].Name:='Ch'+inttostr(i)+'_Config';
    ACF[i].Parent:=ConfigPanel;
    ACF[i].Title:='Scale/Offset '+inttostr(i);
    ACF[i].Align:=alNone;
    ACF[i].ColorUpd(chcolor);
    ACF[i].Tag:=i;
    ACF[i].Offset:=AOffset[i];
    ACF[i].Scale:=AScale[i];
  end;
end;

procedure TMainForm.CreateChSeries;
var
  i:integer;
  dbs:TDbChartSource;
  Aowner:TChart;
begin
  MainChart.LeftAxis.Range.UseMax:=true;
  MainChart.LeftAxis.Range.UseMin:=true;
  WeightChart.LeftAxis.Range.UseMax:=true;
  WeightChart.LeftAxis.Range.UseMin:=true;
  for i:=1 to nChannels do
  begin
    if i<nChannels then Aowner:=MainChart else Aowner:=WeightChart;
    dbs:=TDbChartSource.Create(Aowner);
    dbs.DataSource:=VDataSource;
    dbs.FieldX:='DateTime';
    dbs.OnGetItem:=@DbChartGetItem;
    ASeries[i]:=TlineSeries.Create(Aowner);
    ASeries[i].LinePen.Color:=AChannels[i].Color;
    ASeries[i].Title:=AChannels[i].Title;
    ASeries[i].Source:=dbs;
    ASeries[i].LinePen.Width:=2;
    Aowner.AddSeries(ASeries[i]);
    ASeries[i].ZPosition:=0;
  end;
  ChartToolset1DataPointCrosshairTool1.Enabled:=true;
  ChartToolset1DataPointHintTool1.Enabled:=true;
end;

procedure TMainForm.DeleteChSeries;

  procedure RemoveFromChart(Chart:TChart);
  var
    MySeries: TlineSeries;
    i: integer;
    dbs:TDbChartSource;
  begin
    while Chart.SeriesCount > 0 do
      for i := Chart.SeriesCount-1 downto 0 do
      begin
        MySeries := TlineSeries(Chart.Series.Items[i]);
        Chart.RemoveSeries(Chart.Series.Items[i]);
        dbs:=TDbChartSource(MySeries.Source);
        FreeAndNil(MySeries);
        FreeAndNil(dbs);
      end;
  end;

begin
  RemoveFromChart(MainChart);
  RemoveFromChart(WeightChart);
end;

procedure TMainForm.VDatasetAfterOpen(DataSet: TDataSet);
var
  i:integer;
begin
  for i:=1 to nChannels do
  if i<DataSet.FieldCount-1 then
    TDbChartSource(ASeries[i].Source).FieldY:=DataSet.Fields[i+1].FieldName
  else
    TDbChartSource(ASeries[i].Source).FieldY:='';
end;

procedure TMainForm.UpdateDsply;
var i:integer;
begin
  for i:=1 to nChannels do AChannels[i].PutValue(AData[i]);
  if RegdataTimer.Enabled then L_Time.Caption:=FormatDateTime('hh:nn:ss',Now-StartTime);
end;

procedure TMainForm.ResetData;
begin
//  RxBuf:='';
  StartTime:=0;
end;

procedure TMainForm.EnableDB;
begin
  if not DataM.DataSet.Active then ACreateDBExecute(nil);
  if DataM.DataSet.Active then
  begin
    RegdataTimer.Interval:=Ed_Sampletime.Value*1000;
    RegdataTimer.Enabled:=true;
    LED_DB.Color := clLime;
    B_EnableDB.Caption:='Stop Acq.';
    B_EnableDB.Glyph:=nil;
    ImageList1.GetBitmap(3,B_EnableDB.Glyph);
    If StartTime=0 then StartTime:=Now;
    DBGrid1.Columns[0].Width:=500;
  end;
end;

procedure TMainForm.DisableDB;
begin
  RegdataTimer.Enabled:=False;
  LED_DB.Color := clRed;
  B_EnableDB.Caption:='Acquire';
  B_EnableDB.Glyph:=nil;
  ImageList1.GetBitmap(2,B_EnableDB.Glyph);
end;

function TMainForm.GetConfigValues:boolean;
var
  i:integer;
begin
  result:=false;
  With IniStor do
  begin
    ConfigDir:=AppendPathDelim(ReadString('ConfigDir',GetAppConfigDirUTF8(false)));
    If Not DirectoryExistsUTF8(ConfigDir) then
      If Not ForceDirectoriesUTF8(ConfigDir) Then
      begin
        ShowMessage('Failed to create config folder!: '+ConfigDir);
        Exit;
      end;
    IniFileName:=GetAppConfigFile(false);
    DBPath:=AppendPathDelim(ReadString('DBPath',ConfigDir+'DBPath'));
    If Not DirectoryExistsUTF8(DBPath) then
      If Not ForceDirectoriesUTF8(DBPath) Then
      begin
        ShowMessage('Failed to create database folder: '+DBPath+#13#10' the system is going to try with the default path: '+ConfigDir+'DBPath'+PathDelim);
        DBPath:=ConfigDir+'DBPath'+PathDelim;
        If Not ForceDirectoriesUTF8(DBPath) Then Exit;
      end;
    for i:=1 to nChannels do
    begin
      AOffset[i]:=StrtoFloatDef(ReadString('AOffset'+inttostr(i),'0'),0);
      AScale[i]:=StrtoFloatDef(ReadString('AScale'+inttostr(i),'1'),1);
    end;
    ALabel[1]:=ReadString('ALabel'+inttostr(i),'T1 (°C)');
    ALabel[2]:=ReadString('ALabel'+inttostr(i),'T2 (°C)');
    ALabel[3]:=ReadString('ALabel'+inttostr(i),'T3 (°C)');
    ALabel[4]:=ReadString('ALabel'+inttostr(i),'Peso (g)');
  end;
  result:=true;
end;

function TMainForm.SaveConfigValues:boolean;
var
  i:integer;
begin
  result:=false;
  With IniStor do
  begin
    WriteString('DBPath',DBPath);
    for i:=1 to nChannels do
    begin
      WriteString('AOffset'+inttostr(i),Format('%.3f',[AOffset[i]]));
      WriteString('AScale'+inttostr(i),Format('%.3f',[AScale[i]]));
    end;
  end;
  result:=true;
end;

/////// Compatibilidad con MDA

procedure TMainForm.InitHardware;
var S:String;
    timeOut:integer;
    Conectado:boolean;
begin
  ShowWait('Buscando Hardware en COM'+inttostr(COMPort),COMLimit+1);
  try
    HW := THW.Create();
    WaitF.Step;
    HW.COM_Port := COMPort;
  except
    MessageDlg('Un error grave ha ocurrido al tratar de inicializar el hardware', mtError, [mbOK], 0);
    MainForm.Close;
  end;
  Conectado:=HW.Conectar;  // Try to connect to default COM Port

  // If not connected then search for COM Port if in AutoCOM mode
  if (not Conectado) and AutoCOM then
  begin
    HW.COM_Port := 0;
    repeat
      HW.COM_Port := HW.COM_Port + 1;
      WaitF.WaitLabel.Caption:='Buscando Hardware en COM'+inttostr(HW.COM_Port);
      WaitF.Step;
      Sleep(50);
      Conectado:=HW.Conectar();
    until (Conectado OR (HW.COM_Port = COMLimit));
  end;
  WaitF.Step;

  // Test if connected device
  if Conectado then
  begin
    ComPort:=HW.COM_Port;
    Ed_Comport.Value:=ComPort;
    WaitF.WaitLabel.Caption:='Encontrado un dispositivo conectado en COM'+inttostr(COMPort);
    timeOut:=100;
    HW.SendData('V');
    while (timeOut>0) and not HW.Actualizar do
    begin
      dec(timeOut);
      Sleep(50);
      application.ProcessMessages;
    end;
    if timeOut=0 then Conectado:=false;
  end;

  // Config Simulation mode if was not possible to recognize a hardware device
  if not Conectado then
  begin
    WaitF.WaitLabel.Caption:='No se ha encontrado Hardware compatible, el sistema se iniciará en modo simulación';
    WaitF.EndStep;
    MainForm.Caption:=MainTitle+' versión: '+versionst+' en modo simulación';
    HW.CloseCom;
    Simulate:=true;
  end else
  begin
    S:='versión firmware: '+HW.hardwareID;
    WaitF.WaitLabel.Caption:='Hardware encontrado en COM'+inttostr(COMPort)+#$0d+S;
    WaitF.EndStep;
    MainForm.Caption:=MainTitle+' versión: '+versionst+' - Hardware en COM'+inttostr(COMPort)+' '+S;
    Simulate:=false;
  end;
  WaitF.Close;
end;

end.
