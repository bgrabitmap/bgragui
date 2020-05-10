unit BGButtonDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type
  TBGButtonState = (bsNormal, bsHover, bsActive, bsDisabled);

  { TBGButtonDrawer }

  TBGButtonDrawer = class(TPersistent)
  private
    {$IFDEF DEBUG}
    FRedrawCount: integer;
    {$ENDIF}
    FCaption: string;
    FOnChange: TNotifyEvent;
    FState: TBGButtonState;
    procedure SetCaption(AValue: string);
    procedure SetState(AValue: TBGButtonState);
  protected
    procedure Change;
  public
    constructor Create;
    procedure Draw(ABitmap: TBGRABitmap);
  published
    property State: TBGButtonState read FState write SetState;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBGButtonDrawer }

procedure TBGButtonDrawer.SetState(AValue: TBGButtonState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Change;
end;

procedure TBGButtonDrawer.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TBGButtonDrawer.Create;
begin
  inherited Create;
  {$IFDEF DEBUG}
  FRedrawCount := 0;
  {$ENDIF}
end;

procedure TBGButtonDrawer.SetCaption(AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Change;
end;

procedure TBGButtonDrawer.Draw(ABitmap: TBGRABitmap);
var
  ts: TSize;
  Width, Height: integer;
begin
  {$IFDEF DEBUG}
  Inc(FRedrawCount);
  {$ENDIF}
  ts := ABitmap.TextSize(Caption);
  Width := ABitmap.Width;
  Height := ABitmap.Height;
  case FState of
    bsNormal:
    begin
      { Button Normal }
      ABitmap.GradientFill(0, 0, Width, Height, BGRA(107, 107, 107),
        BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
      ABitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
      ABitmap.SetHorizLine(1, 1, Width - 2, BGRA(130, 130, 130));
      ABitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end;
    bsHover:
    begin
      { Button Hovered }
      ABitmap.GradientFill(0, 0, Width, Height, BGRA(132, 132, 132),
        BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
      ABitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
      ABitmap.SetHorizLine(1, 1, Width - 2, BGRA(160, 160, 160));
      ABitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end;
    bsActive:
    begin
      { Button Down }
      ABitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      ABitmap.Rectangle(1, 1, Width - 1, Height - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      ABitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end;
    bsDisabled:
    begin
      { Button Disabled }
      ABitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      ABitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end;
  end;
  if FState <> bsDisabled then
  begin
    ABitmap.TextOut((Width - ts.cx) div 2, ((Height - ts.cy) div 2) -
      1, Caption, BGRA(47, 47, 47));
    ABitmap.TextOut((Width - ts.cx) div 2, (Height - ts.cy) div 2,
      Caption, BGRA(229, 229, 229));
  end
  else
    ABitmap.TextOut((Width - ts.cx) div 2, (Height - ts.cy) div 2,
      Caption, BGRA(170, 170, 170));
  {$IFDEF DEBUG}
  ABitmap.TextOut(0, 0, IntToStr(FRedrawCount), BGRA(255, 0, 0));
  {$ENDIF}
end;

end.
