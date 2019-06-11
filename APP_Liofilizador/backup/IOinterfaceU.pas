//****************************************************************
//*  Name    : IOInterfaceU.pas                                  *
//*  Author  : Miguel A. Risco Castilo                           *
//*  Notice  : Copyright (c) 2016                                *
//*          : All Rights Reserved                               *
//*  Date    : 08/07/2016                                        *
//*  Version : 4.1                                               *
//*  Notes   : v2.1 Added MinFrameSize and clear buffer on purge *
//*            v3.0 Added COM Timeout error and rbuflen function *
//*            v4.0 Implement Changes to support Delphi2010      *
//*            v4.1 Change CHK (checksum variable) to word       *
//****************************************************************

unit IOInterfaceU;

Interface

Uses
  Windows, Dialogs, Forms, SysUtils, extctrls,
  GREIC_CommOBJ;          //Comunicacion Serial

Type
  TIOInterface = Class(TObject)
  private
     SComm : TGREIC_Comm;
    function BufFull: boolean;
  public
     COM_Port : Integer;
     PacketLost : Integer;
     error : Boolean;
     hardwareID : string;
     TimerAct : TTimer;
     Constructor Create;
     Destructor  Destroy; override;
     Function Conectar : Boolean;
     function OpenCom : Boolean;
     procedure CloseCom;
     function verifycon : boolean;
     function extractdata : ansistring;
     function rbuflen:integer;
     procedure ResetTimeOut;
     procedure senddata(s:ansistring);
     function rawsend(s:ansistring):integer;
     function rawextract:ansistring;
     Procedure Purge;
     Procedure ProcessTimer(Sender:TObject);
     Procedure Recibir;
     procedure Enviar;
  end;

Var
  IOInterface : TIOinterface;

implementation

Const
  FrameHeader = '@#';
  FHSize = Length(FrameHeader);
  MinFrameSize = 5;  // Minimum expected size for data frame
  MaxFrameSize = 36; // Maximun expected size for data frame

var
  RDataBuf, SDataBuf : AnsiString;
  DataFrameSize:byte;       //  tamaño del frame de datos
  COMTimeout:integer;       //  timeout communication

constructor TIOInterface.Create();
begin
  inherited Create();
  SComm := TGREIC_Comm.Create;
  COM_Port := 1;
  SComm.Port := COM_Port;
  SComm.BaudRate := CBR_115200; //CBR_38400; //
  TimerAct := TTimer.Create(nil);
  TimerAct.Enabled := False;
  TimerAct.Interval := 250;
  TimerAct.OnTimer := @ProcessTimer;
  ResetTimeOut;
  error := False;
  RDataBuf := '';
  SDataBuf := '';
  DataFrameSize:=0;
  PacketLost:=0;
end;

destructor TIOInterface.Destroy;
begin
  TimerAct.Enabled := false;
  TimerAct.Free;
  RDataBuf:='';
  SComm.ClosePort;
  sleep(50);
  SComm.Free;
  inherited Destroy;
end;

//Open port and verify reliable connection
function TIOInterface.rbuflen: integer;
begin
  result:=length(RDataBuf);
end;

function TIOInterface.Conectar: Boolean;
begin
  error:=false;
  if OpenCom then verifycon else error:=true;
  TimerAct.Enabled := not error;
  result:=not error;
end; {Conectar}

// verify connection
function TIOInterface.verifycon: boolean;
var
  errorCount : Integer;
  S:Ansistring;
begin
  errorCount := 150;
  result:=false;
  S:='V';   //Command Get Version
  senddata(S);
  while (not Buffull) and  (errorCount > 0) do
  begin
    ResetTimeOut;
    S:=SComm.Read;    // Dummy read
    Enviar;
    sleep(10);
    Recibir;
    Application.ProcessMessages;
    Dec(errorCount);
  end;
  if BufFull then
  begin
    hardwareID:=extractdata;
    result:=true;
  end else
  begin
    error:=true;
    SComm.ClosePort;
  end;
end;

//-----------------Recepción-----------------------

//Get data and reconstruct data frame
procedure TIOInterface.Recibir;
var
  TE:Boolean;
  TmpStr:AnsiString;
begin
  TE:=TimerAct.Enabled;
  TimerAct.Enabled := False;
  TmpStr:=SComm.Read;
  if TmpStr<>'' then
  begin
    RDataBuf := RDataBuf + TmpStr;
    ResetTimeOut;
  end else if (COMTimeout>0) then dec(COMtimeout) else error:=true;
  while (rbuflen>FHSize) and (Copy(RDataBuf,1,FHSize) <> FrameHeader) do Delete(RDataBuf,1,1);
  TimerAct.Enabled := TE;
end;

function TIOInterface.BufFull:boolean;
begin
  result:= RDataBuf<>'';
end;

//Verify Checksum of Data Buffer, checksum data is at the end of the frame
function CheckSum:boolean;
var
  I:byte;
  chk:word;
begin
  //
  result:=true;
  exit;
  //
  chk:=0;
  for I := FHSize+2 to FHSize+DataFrameSize do chk:=chk+ord(RDataBuf[I]);
  result := (Lo(chk) = ord(RDataBuf[FHSize+DataFrameSize+1]));
end;

function TIOInterface.extractdata: ansistring;
var invalid:boolean;
begin
  result:=''; invalid:=false;
  if (rbuflen>FHSize+MinFrameSize) then
  begin
    DataFrameSize:=ord(RDataBuf[FHSize+1]);
    if ((DataFrameSize<=MaxFrameSize) or (DataFrameSize>=MinFrameSize)) then
    begin
      if rbuflen>=(FHSize+DataFrameSize+1) then
        if checksum then
        begin
          result:=Copy(RDataBuf,FHSize + 2,DataFrameSize-1);
          Delete(RDataBuf,1,FHSize+DataFrameSize+1);
        end else invalid:=true;
    end else invalid:=true;
    if invalid then
    begin
      Delete(RDataBuf,1,FHSize);            // Delete Frame Header
      Inc(PacketLost);
      while (rbuflen>FHSize) and (Copy(RDataBuf,1,FHSize) <> FrameHeader) do Delete(RDataBuf,1,1);  //Search for another Frame Header
    end;
  end;
end;

function TIOInterface.rawextract: ansistring;
begin
  result:=SComm.Read;
end;

//-----------------Envío------------------------

procedure TIOInterface.Enviar;
var I:integer; TE:Boolean;
begin
  TE:=TimerAct.Enabled;
  TimerAct.Enabled := False;
  while (SComm.OutCount <> 0) do sleep(5);
  if Length(SDataBuf)>0 then
  begin
    I:=SComm.Write(SDataBuf);
    if I>0 then Delete(SDataBuf,1,I);
  end;
  TimerAct.Enabled := TE;
end;

procedure TIOInterface.ResetTimeOut;
begin
  COMTimeout := trunc(5000 / 250);    //Five seconds, Timer.interval:=250
end;

procedure TIOInterface.senddata(s: ansistring);
var I,chk:byte;
begin
  chk:=0;
  for I := 1 to Length(s) do
    chk:=chk+ord(s[I]);
//  SDataBuf:=SDataBuf+FrameHeader+AnsiChar(chr(length(s)+1))+s+AnsiChar(chr(chk))+AnsiChar(chr(13));
  SDataBuf:=SDataBuf+FrameHeader+s+AnsiChar(chr(13));
end;

function TIOInterface.rawsend(s: ansistring):integer;
begin
  while (SComm.OutCount <> 0) do sleep(5);
  if Length(s)>0 then result:=SComm.Write(s) else result:=0;
end;

//---------------------------------------------------

function TIOInterface.OpenCom: Boolean;
begin
  SComm.Port := COM_Port;
  result:=SComm.OpenPort;
end;

procedure TIOInterface.CloseCom;
begin
  TimerAct.Enabled:=false;
  SComm.ClosePort;
  SDataBuf:='';
  RDataBuf:='';
end;

procedure TIOInterface.ProcessTimer(Sender: TObject);
begin
  Enviar;
  Recibir;
end;

procedure TIOInterface.Purge;
begin
  SComm.Purge;
  while SComm.Read<>'' do SComm.Read;
  SDataBuf:='';
  RDataBuf:='';
  ResetTimeOut;
end;

end.
