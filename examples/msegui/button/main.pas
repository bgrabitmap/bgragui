unit main;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface
uses
 msetypes,mseglob,mseguiglob,mseguiintf,mseapplication,msestat,msemenus,msegui,
 msegraphics,msegraphutils,mseevent,mseclasses,msewidgets,mseforms,bgbutton;

type
 tmainfo = class(tmainform)
   tbgbutton1: tbgbutton;
 end;
var
 mainfo: tmainfo;
implementation
uses
 main_mfm;
end.
