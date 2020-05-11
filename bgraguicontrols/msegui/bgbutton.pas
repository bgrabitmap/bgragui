unit bgbutton;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}

interface
uses
 msesimplewidgets,mseevent,msegraphics,classes,mclasses,msegraphutils,mseact,
 mseguiglob, bgrabitmap, bgrabitmaptypes, bgbuttondrawer;

type
 tbgbutton = class(tbutton)
  private
    fbgra: TBGRABitmap;
    FStyle: TBGButtonDrawer;
  protected
   procedure paint(const canvas: tcanvas); override;
  public
    constructor create(aowner: tcomponent); override;
    destructor destroy; override;
  published
    property Style: TBGButtonDrawer read FStyle write FStyle;
 end;

implementation
uses
 mseshapes;

{ tmybutton }

constructor tbgbutton.create(aowner: tcomponent);
begin
  fbgra := TBGRABitmap.Create(width, height);
  fstyle := TBGButtonDrawer.Create;
  inherited;
end;

destructor tbgbutton.destroy;
begin
  fbgra.Free;
  fstyle.Free;
  inherited;
end;

procedure tbgbutton.paint(const canvas: tcanvas);
begin
  if (Width <> FBGRA.Width) or (Height <> FBGRA.Height) then
    FBGRA.SetSize(Width, Height);
  fstyle.Caption := caption;
  fstyle.Draw(fbgra);
  fbgra.Draw(canvas, 0, 0);
end;

end.
