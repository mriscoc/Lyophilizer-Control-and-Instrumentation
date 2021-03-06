unit DataU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics;

const
  HWchannels=4;     // Hardware max capacity, define the length of the com frame
  DefColor: Array [1..16] of TColor = ($00A1A55D, clBlue, clRed, $003153C4, clMaroon, $000996E7, clBlack, $0084A7C9, clDkGray, $00536508, $00846401, clTeal, clBlue, clFuchsia, clOlive, clPurple);
  MainTitle='Liofilizador -';
  {$IFDEF WINDOWS}
  OSSerialDev='COM';
  {$ENDIF}

type
  TAData=Array [1..HWchannels] of real;
  TAScOff=Array [1..HWchannels] of real;
  TALabels=Array [1..HWchannels] of string;
  TAWord = Array of word;
  TAByte = Array of byte;

var
  DBPath,DBName:String;
  nChannels:integer=4;
  AData: TAData;
  AOffset: TAScOff;        //  Compensación para la calibración
  AScale: TAScOff;         //  Factor de escala
  ALabel: TALabels;        //  Etiquetas
  StartTime: TDateTime;    //  Timestamp de Inicio de la toma de datos
  LoLimit,UpLimit:integer; //  Límites al valor de un dato
  Simulate:Boolean=false;  //  Modo simulación
  versionst:string='';     //  Version del programa
  COMPORT:integer=4;       //  # Puerto de Comunicacion
  COMLimit:integer=20;     //  Número Maximo de puertos COM en autobusqueda
  AutoCOM:boolean=true;    //  Autosearch device in COM ports
  ConfigDir:string='';     //  Configuration folder
//  function ScY(const dat,ch: integer): real;

implementation

Begin
  UpLimit:=5000;
  LoLimit:=-5000;
end.

