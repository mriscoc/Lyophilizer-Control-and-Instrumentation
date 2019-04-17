unit GREIC_CommOBJ;

//Version 3.5
//Modify FPortHandle from DWORD to HANDLE;

// Version 3.0
// Modify of Write to get the number of bytes written to the com buffer

// Version 2.0
// Added ReadBuf
// Added PurgeComm to Open

interface

uses
  Windows, SysUtils,  Controls;

type
  TGREIC_Comm = class(TObject)
  private
   FBaudRate : DWORD; {Baud Rate}
   FPortNumber: Integer; { port #, }
   FPortHandle: HANDLE;
   FPortInUse: Boolean;
   FByteSize: Byte;
   FParity: Byte;
   FStopBits: Byte;
   function GetInCount: Longint;
   function GetOutCount: Longint;
   procedure SetBaud(BaudRate: DWORD);
   procedure SetPort(FPortNumberToSet: Integer);
   procedure SetParity(FParityToSet: Byte);
   procedure SetStopBits(FStopBitsToSet: Byte);
   procedure SetByteSize(FByteSizeToSet: Byte);

  public
   constructor Create();
   destructor Destroy; override;
   function OpenPort: Boolean;
   procedure ClosePort;
   function Read: AnsiString;
   function ReadBuf(var BufStr:AnsiString; size:integer):integer;
   function Write(const sData: AnsiString): integer;
//   function Write(const sData: string): boolean;
   function WaitRead: boolean;
   function IsOpen: Boolean;
   function Purge:Boolean;

  published
   property BaudRate: DWORD read FBaudRate write SetBaud;
   property Port: Integer read FPortNumber write SetPort;
   property Parity: Byte read FParity write SetParity;
   property ByteSize: Byte read FByteSize write SetStopBits;
   property StopBits: Byte read FStopBits write SetByteSize;
   property InCount: longint read GetInCount;{ numero de caracteres recibidos }
   property OutCount: longint read GetOutCount;{ numero de caracteres a enviar }
   property InUse: boolean read FPortInUse;
end;

implementation

constructor TGREIC_Comm.Create();
 begin
  inherited Create();
  FBaudRate := CBR_9600;
  FPortHandle := INVALID_HANDLE_VALUE; { invalidate to start }
  FPortInUse := False;
  FPortNumber := 1;
  FByteSize := 8;
  FParity := NOPARITY;
  FStopBits:= ONESTOPBIT;
 end;

destructor TGREIC_Comm.Destroy;
 begin
  ClosePort;
  inherited Destroy;
 end;

{Abre el puert COMport, returns True si ok}
function TGREIC_Comm.OpenPort: boolean;
 var
  sCom: string;
  dcbPort: TDCB;
 begin
  if FPortHandle <> INVALID_HANDLE_VALUE then ClosePort;
  sCom := 'COM' + IntToStr(FPortNumber);
  FPortHandle := CreateFile(PChar(sCom), GENERIC_READ or GENERIC_WRITE, 0, nil,
                              OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, LongInt(0));
  if (FPortHandle <> INVALID_HANDLE_VALUE) then
   begin
    if GetCommState(FPortHandle, dcbPort) then
     begin
      dcbPort.BaudRate := FBaudRate;
      dcbPort.ByteSize := FByteSize;
      dcbPort.Parity := FParity;
      dcbPort.StopBits := FStopBits;
      dcbPort.Flags := 1;
      { flag bit fields:
      dcb_Binary, dcb_Parity, dcb_OutxCtsFlow, dcb_fOutxDsrFlow,
      dcb_fOutX, dcb_fInX, dcb_DtrFlow, dcb_RtsFlow
      }
     end;
    SetCommState(FPortHandle, dcbPort);
    PurgeComm(FPortHandle,PURGE_RXCLEAR);  //MARC Agregado
   end;
  Result := (FPortHandle <> INVALID_HANDLE_VALUE);
end;

function TGREIC_Comm.Purge: Boolean;
begin
  Result:=PurgeComm(FPortHandle,PURGE_RXCLEAR);  //MARC Agregado
end;

{Cierra el puerto COM port}
procedure TGREIC_Comm.ClosePort;
 begin
  if FPortHandle <> INVALID_HANDLE_VALUE then CloseHandle(FPortHandle);
  FPortHandle := INVALID_HANDLE_VALUE;
 end;

function TGREIC_Comm.GetInCount: LongInt;
 var
  statPort: TCOMSTAT;
  dwErrorCode: DWord;
 begin
  Result := 0;
  if FPortHandle <> INVALID_HANDLE_VALUE then
  begin
   ClearCommError(FPortHandle, dwErrorCode, @statPort);
   Result := statPort.cbInQue;
  end;
 end;

function TGREIC_Comm.GetOutCount: LongInt;
 var
  statPort: TCOMSTAT;
  dwErrorCode: DWord;
 begin
  Result := 0;
  if FPortHandle <> INVALID_HANDLE_VALUE then
  begin
   ClearCommError(FPortHandle, dwErrorCode, @statPort);
   Result := statPort.cbOutQue;
  end;
 end;

function TGREIC_Comm.IsOpen: boolean;
 begin
  Result := (FPortHandle <> INVALID_HANDLE_VALUE);
 end;

procedure TGREIC_Comm.SetBaud(BaudRate: DWORD);
 begin
  FBaudRate := BaudRate;
 end;

procedure TGREIC_Comm.SetPort(FPortNumberToSet: Integer);
 begin
  FPortNumber := FPortNumberToSet;
 end;

procedure TGREIC_Comm.SetParity(FParityToSet: Byte);
 begin
  FParity := FParityToSet;
 end;

procedure TGREIC_Comm.SetStopBits(FStopBitsToSet: Byte);
 begin
  FStopBits := FStopBitsToSet;
 end;

procedure TGREIC_Comm. SetByteSize(FByteSizeToSet: Byte);
 begin
  FByteSize := FByteSizeToSet;
 end;

//MARC Modificación

 function TGREIC_Comm.Write(const sData: AnsiString): Integer;
 var
  dwCharsWritten: DWord;
 begin
  dwCharsWritten := 0;
  Result := 0; { default to error return }
  if FPortHandle <> INVALID_HANDLE_VALUE then
   begin
    WriteFile(FPortHandle, PChar(sData)^, Length(sData), dwCharsWritten, nil);
    Result := dwCharsWritten;
   end;
 end;


// function TGREIC_Comm.Write(const sData: string): Boolean;
// var
//  dwCharsWritten: DWord;
// begin
//  dwCharsWritten := 0;
//  Result := False; { default to error return }
//  if FPortHandle <> INVALID_HANDLE_VALUE then
//   begin
//    WriteFile(FPortHandle, PChar(sData)^, Length(sData), dwCharsWritten, nil);
//    if dwCharsWritten = DWORD(Length(sData)) then
//    Result := True;
//   end;
// end;

function TGREIC_Comm.Read: AnsiString;
 var
  cbCharsAvailable, cbCharsRead: DWORD;
 begin
  Result := '';
  if FPortHandle = INVALID_HANDLE_VALUE then exit;

  cbCharsAvailable := GetInCount;
  if cbCharsAvailable > 0 then
   begin
    SetLength(Result, cbCharsAvailable);
    ReadFile(FPortHandle, PChar(Result)^, cbCharsAvailable, cbCharsRead, nil);
    SetLength(Result, cbCharsRead);
   end;
 end;

function TGREIC_Comm.ReadBuf(var BufStr:AnsiString; size:integer):integer;
var
  cbCharsAvailable, cbCharsRead: DWORD;
begin
  Result := 0;
  if FPortHandle = INVALID_HANDLE_VALUE then exit;
  cbCharsAvailable := GetInCount;
  if cbCharsAvailable > 0 then
  begin
    ReadFile(FPortHandle, BufStr[1], size, cbCharsRead, nil);
    Result:=cbCharsRead;
   end;
 end;

function TGREIC_Comm.WaitRead: Boolean;
 var
  lpEvtMask:DWORD;
 begin
  Result := True;
  if FPortHandle = INVALID_HANDLE_VALUE then exit;
  SetCommMask(FPortHandle, EV_RXCHAR);
  WaitCommEvent(FPortHandle, lpEvtMask, nil);
 end;

end.

