unit datamu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, sqlite3conn, sqldb, db, DataU;

type

  { TDataM }

  TDataM = class(TDataModule)
    Dataset: TSQLQuery;
    PrjDataset: TSQLQuery;
    VDataset: TSQLQuery;
    DBConn: TSQLite3Connection;
    Transaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    FAutoSave: Boolean;
    FAutoSaveCounts: integer;
    ASCountDown:Integer;
    procedure SetAutoSave(AValue: Boolean);
    procedure SetAutoSaveCounts(AValue: integer);
    { private declarations }
  public
    { public declarations }
    function CreateDB(DBName: String; Chn: integer): boolean;
    function OpenDataBase(DBName:String):integer;
    function InsertData(var data: TAData): boolean;
    procedure ReOpenDataSets;
    procedure CloseDataBase;
    procedure ClearAll;
    property AutoSave:Boolean read FAutoSave write SetAutoSave;
    property AutoSaveCounts:integer read FAutoSaveCounts write SetAutoSaveCounts;
  end;

var
  DataM: TDataM;

implementation

{$R *.lfm}

{ TDataM }

procedure TDataM.ReOpenDataSets;
var i:integer;
begin
  DataSet.DisableControls;
  Dataset.Open;
  With Dataset do
  begin
    Fields[0].Visible:=false; // This is the ID
    for i:=2 to FieldCount-1 do TNumericField(Fields[i]).DisplayFormat:='#0.0';
  end;
  if (DataSet.FieldCount-2)=nChannels then
  begin
    DataSet.EnableControls;
    VDataSet.Open;
    VDataSet.Refresh;
    PrjDataSet.Open;
    PrjDataSet.Refresh;
  end else ShowMessage('The database file has a different number of channels: '+IntToStr(DataSet.FieldCount-2));
end;

procedure TDataM.CloseDataBase;
begin
  If DataSet.Active then Transaction.Commit;
  Transaction.CloseDataSets;
  Transaction.Active:=false;
  DBConn.Close;
end;

procedure TDataM.DataModuleCreate(Sender: TObject);
begin
  FAutoSaveCounts:=10;
  ASCountDown:=FAutoSaveCounts;
  FAutoSave:=true;
end;

procedure TDataM.SetAutoSave(AValue: Boolean);
begin
  if FAutoSave=AValue then Exit;
  FAutoSave:=AValue;
  If FAutoSave then ASCountDown:=FAutoSaveCounts;
end;

procedure TDataM.SetAutoSaveCounts(AValue: integer);
begin
  if FAutoSaveCounts=AValue then Exit;
  FAutoSaveCounts:=AValue;
end;

function TDataM.CreateDB(DBName:String; Chn:integer):boolean;
var
 newFile:Boolean;
 sqlstr:string;
 i:integer;

begin
 result:=false;
 DBConn.Close; // Ensure the connection is closed when we start

 try
   // Since we're making this database for the first time,
   // check whether the file already exists
   newFile := not FileExists(DBName);

   if newFile then
   begin
     // Create the database and the tables
     try
       DBConn.DatabaseName:= DBName;
       DBConn.Open;
       Transaction.Active := true;

       // Here we're setting up a table named "DATA" in the new database
       sqlstr:='CREATE TABLE "Data"('+
               ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
               ' "DateTime" DateTime NOT NULL,';
       if Chn>1 then for i:=0 to Chn-2 do sqlstr+=Format(' "CH%d" REAL, ',[i]);
       sqlstr+=Format(' "CH%d" REAL);',[Chn-1]);
       DBConn.ExecuteDirect(sqlstr);
       // Creating an index based upon id in the DATA Table
       DBConn.ExecuteDirect('CREATE UNIQUE INDEX "Data_id_idx" ON "Data"( "id" );');
       Transaction.Commit;

       // Here we're setting up a table named "PROJECT" in the new database
       sqlstr:='CREATE TABLE "Project"('+
               ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
               ' "DateTime" DateTime,'+
               ' "Name" String,'+
               ' "Description" String'+
               ');';
       DBConn.ExecuteDirect(sqlstr);
       // Creating an index based upon id in the PROJECT Table
       DBConn.ExecuteDirect('CREATE UNIQUE INDEX "Project_id_idx" ON "Project"( "id" );');
       Transaction.Commit;

       ASCountDown:=FAutoSaveCounts;
       result:=true;
     except
       on E: Exception do ShowMessage('Unable to Create new Database: '+E.Message);
     end;
   end else ShowMessage('The Database file already exists.');
 except
   on E: Exception do ShowMessage('Unable to check if database file exists: '+E.Message);
 end;
end;

function TDataM.OpenDataBase(DBName: String): integer; // return the number of channels
begin
  result:=0;
  if FileExists(DBName) then
  begin
    CloseDataBase;
    DBConn.DatabaseName:=DBName;
    try
      DBConn.Open;
      Transaction.Active := true;
      ReOpenDataSets;
      ASCountDown:=FAutoSaveCounts;
      result:=DataSet.FieldCount-2; // 1 Id, 2 TimeDate
    except
      on E: Exception do ShowMessage('Unable to open database: '+E.Message);
    end;
  end;
end;

function TDataM.InsertData(var data:TAData):boolean;
var
  i:integer;
begin
  with DataM do
  try
    DataSet.DisableControls;
    DataSet.Append;
    DataSet.FieldByName('DateTime').AsDateTime:=Now;
    for i:=0 to nChannels-1 do DataSet.FieldByName('CH'+InttoStr(i)).value:=data[i];
    DataSet.Post;
    if FAutoSave then
    begin
      dec(ASCountDown);
      if ASCountDown=0 then
      begin
        ASCountDown:=FAutoSaveCounts;
        Transaction.Commit;
      end;
    end;
    result:=true;
    DataSet.EnableControls;
  except
    on E: Exception do ShowMessage('Unable to Insert new values to Database: '+E.Message);
  end;
end;

procedure TDataM.ClearAll;
begin
  DataSet.DisableControls;
  CloseDataBase;
  DataSet.EnableControls;
  VDataSet.Refresh;
  PrjDataSet.Refresh;
end;

end.

