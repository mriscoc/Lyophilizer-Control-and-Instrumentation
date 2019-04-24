//----------------------------------------------
// Hardware interface Unit
// Original from MDA_DS v1.9
// Simplified for DataLog
// April, 2019
// (c) Miguel Risco-Castillo
//----------------------------------------------

unit HWU;

interface

Uses Dialogs, Forms, SysUtils, Math, Classes,
     IOInterfaceU, DataU;

Type

{ THW }

THW = Class(TIOInterface)
private
    vTempx : TAData;
    vState  : TAByte;
public
    function Actualizar:Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure GenSimData;
    property Temperatura: TAData read vTempx;
End;

Var
  HW : THW = nil;

implementation

function HexToInt(Valor:String): integer;
begin
  Result := StrToInt('$'+Valor);
end;

function THW.Actualizar:boolean;
var
  i:integer;
  Buf:ansistring;
  StrBuf:TStringList;
  FrameTyp:Char;
begin
  result:=true;
  if Simulate then GenSimData else
  begin
    Buf:=extractdata;
    if Buf<>'' then
    begin
      FrameTyp:=Buf[1];
      StrBuf:=TStringList.Create;
      StrBuf.Delimiter:=';';
      StrBuf.StrictDelimiter:=true;
      StrBuf.DelimitedText:=Copy(Buf,2,MaxInt);
      case FrameTyp of
        'C': begin
//;
        end;
        'D': for i := 0 to nChannels - 1 do
          begin
            vTempx[i]:=AOffset[i]+StrtoIntDef(StrBuf[i+1],0)/100;
            vTempx[i]:=Max(LoLimit,Min(UpLimit,vTempx[i]));
          end;
      end;
      if assigned(StrBuf) then FreeAndNil(StrBuf);
    end else result:=false;
  end;
end;

constructor THW.Create();
begin
  SetLength(vState,nChannels);
  inherited Create();
end;

//Destructor
destructor THW.Destroy;
begin
  inherited Destroy;
  SetLength(vState,0);
end; {Destroy}


procedure THW.GenSimData;
var
  i: integer;
const
  offset=0;
  range=100;
begin
  for i:=0 to nChannels-1 do
  begin
    vTempx[i]:=AOffset[i] + offset + (range)*(1/2-Cos(I/3+Time*500)/2);
  end;
end;

end.
