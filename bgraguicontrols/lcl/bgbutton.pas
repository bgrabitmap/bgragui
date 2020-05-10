unit BGButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGButtonDrawer, BGRABitmap, BGRABitmapTypes;

type


  { TBGButton }

  TBGButton = class(TGraphicControl)
  private
    FBGRA: TBGRABitmap;
    FModalResult: TModalResult;
    FStyle: TBGButtonDrawer;
    procedure SetStyle(AValue: TBGButtonDrawer);
    procedure StyleChange(Sender: TObject);
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove({%H-}x, {%H-}y: integer); virtual;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Style: TBGButtonDrawer read FStyle write SetStyle;
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icons/bgbutton_icon.lrs}
  RegisterComponents('BGRAGUI Controls', [TBGButton]);
end;

{ TBGButton }

procedure TBGButton.SetStyle(AValue: TBGButtonDrawer);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
end;

procedure TBGButton.StyleChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TBGButton.DoClick;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
end;

procedure TBGButton.DoMouseDown;
var
  NewState: TBGButtonState;
begin
  NewState := bsActive;

  if NewState <> FStyle.State then
    FStyle.State := NewState;
end;

procedure TBGButton.DoMouseUp;
var
  NewState: TBGButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := bsHover
  else
    NewState := bsNormal;

  if NewState <> FStyle.State then
    FStyle.State := NewState;
end;

procedure TBGButton.DoMouseEnter;
var
  NewState: TBGButtonState;
begin
  if Enabled then
    NewState := bsHover
  else
  begin
    FStyle.State := bsNormal;
    NewState := FStyle.State;
  end;

  if NewState <> FStyle.State then
    FStyle.State := NewState;
end;

procedure TBGButton.DoMouseLeave;
var
  NewState: TBGButtonState;
begin
  if Enabled then
    NewState := bsNormal
  else
  begin
    FStyle.State := bsNormal;
    NewState := FStyle.State;
  end;

  if NewState <> FStyle.State then
    FStyle.State := NewState;
end;

procedure TBGButton.DoMouseMove(x, y: integer);
begin
  inherited;
end;

procedure TBGButton.Paint;
begin
  inherited Paint;
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height) then
    FBGRA.SetSize(ClientWidth, ClientHeight);
  FStyle.Draw(FBGRA);
  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBGButton.Click;
begin
  DoClick;
  inherited Click;
end;

procedure TBGButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    DoMouseDown;
end;

procedure TBGButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp;
end;

procedure TBGButton.MouseEnter;
begin
  inherited MouseEnter;
  DoMouseEnter;
end;

procedure TBGButton.MouseLeave;
begin
  inherited MouseLeave;
  DoMouseLeave;
end;

procedure TBGButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  DoMouseMove(X, Y);
end;

procedure TBGButton.SetEnabled(Value: boolean);
begin
  if Value then
    FStyle.State := bsNormal
  else
    FStyle.State := bsDisabled;
  inherited SetEnabled(Value);
end;

procedure TBGButton.TextChanged;
begin
  inherited TextChanged;
  FStyle.Caption := Caption;
end;

constructor TBGButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create(Width, Height);
  FStyle := TBGButtonDrawer.Create;
  FStyle.OnChange:=@StyleChange;
end;

destructor TBGButton.Destroy;
begin
  FBGRA.Free;
  FStyle.Free;
  inherited Destroy;
end;

end.
