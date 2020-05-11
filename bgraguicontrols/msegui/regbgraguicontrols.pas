unit regbgraguicontrols;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}

interface

implementation
uses
 msedesignintf, bgbutton;

procedure register;
begin
 registercomponents('BGRAGUI Controls',[tbgbutton]);
end;

initialization
 register;
end.
