unit MainFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAIntervalSources, TAGraph, TASeries, TATransformations,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons,
  ComCtrls, DBGrids, ActnList, IniPropStorage, DbCtrls, sqlite3conn, sqldb, db,
  ueled, TATools, TADataTools, TADbSource, LazFileUtils, Types, TACustomSource,
  TAGUIConnectorBGRA, fptimer;

type

  { TMainForm }

  TMainForm = class(TForm)
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
    procedure B_ConnectClick(Sender: TObject);
    procedure B_EnableDBClick(Sender: TObject);
    procedure B_OpenDBClick(Sender: TObject);
    procedure B_SavePrjClick(Sender: TObject);
    procedure ChartToolset1DataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DbChartGetItem(ASender: TDbChartSource; var AItem: TChartDataItem);
    procedure Ed_SampletimeChange(Sender: TObject);
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
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses Ch_FrameU, CHConfig_FrameU, datamu, DataU, HWU, WaitU, VersionSupportU, Math;

var
  AChannels: Array [0..HWchannels-1] of TChFrame;
  ASeries: Array [0..HWchannels-1] of TlineSeries;

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
end;

procedure TMainForm.ACreateDBExecute(Sender: TObject);
begin
  DBName:=FormatDateTime('YYYYMMDD-hhnnss',Now)+'.db';
  if DataM.CreateDB(DBPath+DBName,nChannels) then DataM.ReOpenDataSets;
end;

procedure TMainForm.AReconectHWExecute(Sender: TObject);
begin
  Disconnect;
  If Assigned(HW) then freeandnil(HW);
  InitHardware;
end;

procedure TMainForm.BClearAllClick(Sender: TObject);
begin
  DataM.ClearAll;
  MainChart.Invalidate;
end;

procedure TMainForm.B_EnableDBClick(Sender: TObject);
begin
  If RegdataTimer.Enabled then DisableDB else EnableDB;
end;

procedure TMainForm.B_OpenDBClick(Sender: TObject);
var chans:Integer;
begin
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
  for i:=0 to nChannels-1 do AData[i]:=HW.Temperatura[i];
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
  caption:='Data Logger v'+versionst;
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
  end;
end;

procedure TMainForm.CreateChFrames;
Var
  i:integer;
  chcolor:TColor;
  CF:TChConfigFrame;
begin
  for i:=0 to nChannels-1 do
  begin
    chcolor:=DefColor[i]; //Random($FFFFFF);
//
    AChannels[i]:=TChFrame.Create(ChPanel);
    AChannels[i].Name:='Ch'+inttostr(i)+'_Frame';
    AChannels[i].Parent:=ChPanel;
    AChannels[i].Title:='Ch'+inttostr(i+1);
    AChannels[i].Align:=alNone;
    AChannels[i].ColorUpd(chcolor);
//
    CF:=TChConfigFrame.Create(ConfigPanel);
    CF.Name:='Ch'+inttostr(i)+'_Config';
    CF.Parent:=ConfigPanel;
    CF.Title:='Offset Ch'+inttostr(i+1);
    CF.Align:=alNone;
    CF.ColorUpd(chcolor);
    CF.Tag:=i;
    CF.Value:=AOffset[i];
  end;
end;

procedure TMainForm.CreateChSeries;
var
  i:integer;
  dbs:TDbChartSource;
begin
  MainChart.LeftAxis.Range.UseMax:=true;
  MainChart.LeftAxis.Range.UseMin:=true;
  for i:=0 to nChannels-1 do
  begin
    dbs:=TDbChartSource.Create(MainChart);
    dbs.DataSource:=VDataSource;
    dbs.FieldX:='DateTime';
    dbs.FieldY:='CH'+inttostr(i);
    dbs.OnGetItem:=@DbChartGetItem;
    ASeries[i]:=TlineSeries.Create(MainChart);
    ASeries[i].LinePen.Color:=AChannels[i].Color;
    ASeries[i].Title:=AChannels[i].Title;
    ASeries[i].Source:=dbs;
    MainChart.AddSeries(ASeries[i]);
    ASeries[i].ZPosition:=0;
  end;
  ASeries[1].LinePen.Width:=2;
  ASeries[3].LinePen.Width:=2;
end;


procedure TMainForm.DeleteChSeries;
var
  MySeries: TlineSeries;
  i: integer;
  dbs:TDbChartSource;
begin
  while MainChart.SeriesCount > 0 do
    for i := MainChart.SeriesCount-1 downto 0 do
    begin
      MySeries := TlineSeries(MainChart.Series.Items[i]);
      MainChart.RemoveSeries(MainChart.Series.Items[i]);
      dbs:=TDbChartSource(MySeries.Source);
      FreeAndNil(MySeries);
      FreeAndNil(dbs);
    end;
end;

procedure TMainForm.UpdateDsply;
var i:integer;
begin
  for i:=0 to nChannels-1 do AChannels[i].PutValue(AData[i]);
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
    RegdataTimer.Enabled:=true;
    LED_DB.Color := clLime;
    B_EnableDB.Caption:='Stop Acq.';
    B_EnableDB.Glyph:=nil;
    ImageList1.GetBitmap(3,B_EnableDB.Glyph);
    If StartTime=0 then StartTime:=Now;
    DBGrid1.Columns[0].Width:=500;
    ChartToolset1DataPointCrosshairTool1.Enabled:=false;
    ChartToolset1DataPointHintTool1.Enabled:=false;
  end;
end;

procedure TMainForm.DisableDB;
begin
  RegdataTimer.Enabled:=False;
  LED_DB.Color := clRed;
  B_EnableDB.Caption:='Acquire';
  B_EnableDB.Glyph:=nil;
  ImageList1.GetBitmap(2,B_EnableDB.Glyph);
  ChartToolset1DataPointCrosshairTool1.Enabled:=true;
  ChartToolset1DataPointHintTool1.Enabled:=true;
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
    for i:=0 to nChannels-1 do
      AOffset[i]:=StrtoFloatDef(ReadString('AOffset'+inttostr(i),'0'),0);
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
    for i:=0 to nChannels-1 do
      WriteString('AOffset'+inttostr(i),Format('%.2f',[AOffset[i]]));
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

  // Test if connected device is the Multichannel
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

  // Config Simulation mode if was not possible to recognize a Multichannel device
  if not Conectado then
  begin
    WaitF.WaitLabel.Caption:='No se ha encontrado Hardware compatible, el sistema se iniciará en modo simulación';
    WaitF.EndStep;
    MainForm.Caption:=MainTitle+' versión: '+versionst+' en modo simulación';
    HW.CloseCom;
    Simulate:=true;
  end else
  begin
    S:='';//'versión firmware: '+HW.hardwareID;
    WaitF.WaitLabel.Caption:='Hardware encontrado en COM'+inttostr(COMPort)+#$0d+S;
    WaitF.EndStep;
    MainForm.Caption:=MainTitle+' versión: '+versionst+' - Hardware en COM'+inttostr(COMPort)+' '+S;
    Simulate:=false;
  end;
  WaitF.Close;
end;

end.
