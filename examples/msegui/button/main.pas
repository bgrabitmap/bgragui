unit main;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface
uses
 msetypes,mseglob,mseguiglob,mseguiintf,mseapplication,msestat,msemenus,msegui,
 msegraphics,msegraphutils,mseevent,mseclasses,msewidgets,mseforms,bgbutton,
 msesimplewidgets, bgbuttondrawer;

type
 tmainfo = class(tmainform)
   tbgbutton1: tbgbutton;
   tbgbutton2: tbgbutton;
   
   procedure onExecuteButton2(const sender: TObject);
 end;
var
 mainfo: tmainfo;
implementation
uses
 main_mfm;
procedure tmainfo.onExecuteButton2(const sender: TObject);
begin
  tbgbutton1.Style.State := bsDisabled;

end;

end.