unit main;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface
uses
 msetypes,mseglob,mseguiglob,mseguiintf,mseapplication,msestat,msemenus,msegui,
 msegraphics,msegraphutils,mseevent,mseclasses,msewidgets,mseforms,bgbutton,
 msesimplewidgets, bgbuttondrawer,BGRAFlashProgressBar, BGRABitmap,
  BGRABitmapTypes;

type
 tmainfo = class(tmainform)
   tbgbutton1: tbgbutton;
   tbgbutton2: tbgbutton;
   
   tpaintbox1: tpaintbox;
   TBGRAFlashProgressBar1: TBGRAFlashProgressBar;
   procedure onExecuteButton2(const sender: TObject);
   procedure onExecuteButton1(const sender: TObject);
   procedure PaintBoxPaint(const sender: twidget; const acanvas: tcanvas);
 end;
var
 mainfo: tmainfo;
implementation
uses
 main_mfm;
procedure tmainfo.onExecuteButton2(const sender: TObject);
begin
  tbgbutton1.Enabled := not tbgbutton1.Enabled;

end;

procedure tmainfo.onExecuteButton1(const sender: TObject);
begin
  showmessage('Hello World');
end;

procedure tmainfo.PaintBoxPaint(const sender: twidget; const acanvas: tcanvas);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(sender.width, sender.height, BGRAPixelTransparent);
  bmp.RoundRectAntialias(10, 10, bmp.width-10, bmp.height-10, 10, 10, BGRABlack, 2);
  bmp.Draw(acanvas, 0, 0, False);
  bmp.Free;
end;

end.
