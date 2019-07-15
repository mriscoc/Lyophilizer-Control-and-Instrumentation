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
  FYmax: real;
  FYmin: real;
  vTempx : TAData;
  vState  : TAByte;
public
  function Actualizar:Boolean;
  constructor Create;
  destructor Destroy; override;
  procedure GenSimData;
  property Temperatura: TAData read vTempx;
  property Ymax:real read FYmax;
  property Ymin:real read FYmin;
End;

Var
  HW : THW = nil;

implementation

function HexToInt(Valor:String): integer;
begin
  Result := StrToInt('$'+Valor);
end;

function THW.Actualizar: Boolean;
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
      StrBuf.DelimitedText:=Copy(Buf,2,MaxInt); // Remove FrameTyp
      case FrameTyp of
        'C': begin
//;
        end;
        'D': for i := 1 to nChannels do
          begin
            if i<StrBuf.Count then vTempx[i]:=AOffset[i]+StrtoIntDef(StrBuf[i],0)*AScale[i];
            vTempx[i]:=Max(LoLimit,Min(UpLimit,vTempx[i]));
            FYMax:=Max(FYMax,vTempx[i]);
            FYMin:=Min(FYMin,vTempx[i]);
          end;
      end;
      if assigned(StrBuf) then FreeAndNil(StrBuf);
    end else result:=false;
  end;
end;

constructor THW.Create;
begin
  SetLength(vState,nChannels);
  FYmax:=MinFloat;
  FYmin:=MaxFloat;
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
begin
  for i:=1 to nChannels do
  begin
    vTempx[i]:=AOffset[i] + AScale[i]*(1/2-Cos(I/3+Time*500)/2);
    FYMax:=Max(FYMax,vTempx[i]);
    FYMin:=Min(FYMin,vTempx[i]);
  end;
end;

end.
