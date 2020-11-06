unit regbgraguicontrols;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}

interface

implementation
uses
 msedesignintf, bgbutton, bgraflashprogressbar;

procedure register;
begin
 registercomponents('BGRAGUI Controls',[tbgbutton,tbgraflashprogressbar]);
end;

initialization
 register;
end.
