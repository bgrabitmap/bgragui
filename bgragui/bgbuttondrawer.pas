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
    FRedrawCount: Integer;
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
begin
  {$IFDEF DEBUG}
  Inc(FRedrawCount);
  {$ENDIF}
  case FState of
    bsNormal:
    begin
      ABitmap.Fill(BGRA(0, 100, 0));
    end;
    bsHover:
    begin
      ABitmap.Fill(BGRA(50, 100, 50));
    end;
    bsActive:
    begin
      ABitmap.Fill(BGRA(75, 100, 75));
    end;
    bsDisabled:
    begin
      ABitmap.Fill(BGRA(100, 100, 100));
    end;
  end;
  ABitmap.TextRect(Rect(0, 0, ABitmap.Width, ABitmap.Height), FCaption, taCenter, tlCenter, BGRABlack);
  {$IFDEF DEBUG}
  ABitmap.TextOut(0, 0, IntToStr(FRedrawCount), BGRA(255, 0, 0));
  {$ENDIF}
end;

end.
