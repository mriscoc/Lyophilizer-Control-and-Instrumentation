//----------------------------------------------
// Hardware interface Unit
// Original from MDA_DS v1.9
// Adaptated to DataLog
// July, 2016
// (c) Miguel Risco-Castillo
//----------------------------------------------

unit HWU;

interface

Uses Dialogs, Forms, SysUtils, Math,
     IOInterfaceU, DataU;

Type

{ THW }

THW = Class(TIOInterface)
private
    vTempx : TAData;
    vTSet  : TAData;
    vTSet_H : TAData;
    vTiempoAct : TAWord;
    vTiempoSet : TAWord;
    vState  : TAByte;
public
    IDEq:word;          //  ID del Equipo Conectado
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

//function THW.Actualizar:boolean;
//var
//  i:integer;
//  StrBuf:String;
//  DataBuf:Array[0..91] of byte;
//begin
//  result:=true;
//  if Simulate then GenSimData else
//  begin
//    StrBuf:=extractdata;
//    if StrBuf<>'' then
//    begin
//      Move(StrBuf[1], DataBuf[0], 91);
//      if DataBuf[0]=ord('C') then
//      begin
//        IDEq:=Databuf[2] + $100 * Databuf[1];
//      end else
//      begin
//        IDEq:=Databuf[1] + $100 * Databuf[0];
//        for i := 0 to nChannels - 1 do
//        begin
//          vTempx[i]    := AOffset[i]+(Databuf[ 3+11*i] + $100 * Databuf[ 2+11*i])/IntPower(10,precision);
//          vTempx[i]    := Max(LoLimit,Min(UpLimit,vTempx[i]));
//          vTSet[i]     :=(Databuf[ 5+11*i] + $100 * Databuf[ 4+11*i])/IntPower(10,precision);
//          vTSet_H[i]   :=(Databuf[ 7+11*i] + $100 * Databuf[ 6+11*i])/IntPower(10,precision);
//          vTiempoAct[i]:=(Databuf[ 9+11*i] + $100 * Databuf[ 8+11*i]);
//          vTiempoSet[i]:=(Databuf[11+11*i] + $100 * Databuf[10+11*i]);
//          vState[i]    := Databuf[12+11*i];
//        end;
//      end;
//    end else result:=false;
//  end;
//end;

function THW.Actualizar:boolean;
var
  i:integer;
  StrBuf:String;
  DataBuf:Array[0..91] of byte;
begin
  result:=true;
  if Simulate then GenSimData else
  begin
    StrBuf:=extractdata;
    if StrBuf<>'' then
    begin
      Move(StrBuf[1], DataBuf[0], 91);

      ShowMessage(StrBuf);
      ShowMessage(DataBuf);

      if DataBuf[0]=ord('C') then
      begin
        IDEq:=Databuf[2] + $100 * Databuf[1];
      end else
      begin
        IDEq:=Databuf[1] + $100 * Databuf[0];
        for i := 0 to nChannels - 1 do
        begin
          vTempx[i]    := AOffset[i]+(Databuf[ 3+11*i] + $100 * Databuf[ 2+11*i])/IntPower(10,precision);
          vTempx[i]    := Max(LoLimit,Min(UpLimit,vTempx[i]));
          vTSet[i]     :=(Databuf[ 5+11*i] + $100 * Databuf[ 4+11*i])/IntPower(10,precision);
          vTSet_H[i]   :=(Databuf[ 7+11*i] + $100 * Databuf[ 6+11*i])/IntPower(10,precision);
          vTiempoAct[i]:=(Databuf[ 9+11*i] + $100 * Databuf[ 8+11*i]);
          vTiempoSet[i]:=(Databuf[11+11*i] + $100 * Databuf[10+11*i]);
          vState[i]    := Databuf[12+11*i];
        end;
      end;
    end else result:=false;
  end;
end;


constructor THW.Create();
begin
//  SetLength(vTempx,nChannels);
//  SetLength(vTSet,nChannels);
//  SetLength(vTSet_H,nChannels);
  SetLength(vTiempoSet,nChannels);
  SetLength(vTiempoAct,nChannels);
  SetLength(vState,nChannels);
  inherited Create();
end;

//Destructor
destructor THW.Destroy;
begin
  inherited Destroy;
//  SetLength(vTempx,0);
//  SetLength(vTSet,0);
//  SetLength(vTSet_H,0);
  SetLength(vTiempoSet,0);
  SetLength(vTiempoAct,0);
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
