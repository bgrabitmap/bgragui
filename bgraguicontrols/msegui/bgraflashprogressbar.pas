unit BGRAFlashProgressBar;

interface

uses
  msesimplewidgets,mseevent,msegraphics,classes, sysutils, mclasses,msegraphutils,mseact,
  mseguiglob, msewidgets, bgrabitmap, bgrabitmaptypes, bgragraphics, Math;

type

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(tsimplewidget)
  private
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FBackgroundColor: colorty;
    FMaxValue: integer;
    FMinValue: integer;
    FValue:    integer;
    FBmp:      TBGRABitmap;
    FRandSeed: integer;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    procedure SetFBackgroundRandomize(AValue: boolean);
    procedure SetFBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetFBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetFBackgroundColor(AValue: colorty);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
    { Private declarations }
  protected
    procedure paint(const canvas: tcanvas); override;
  public
    { Public declarations }
    constructor create(aowner: tcomponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property MinValue: integer Read FMinValue Write SetMinValue;
    property MaxValue: integer Read FMaxValue Write SetMaxValue;
    property Value: integer Read FValue Write SetValue;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnredraw write FOnRedraw;
    property BackgroundColor: colorty read FBackgroundColor write SetFBackgroundColor;
    property BackgroundRandomizeMinIntensity: word read FBackgroundRandomizeMinIntensity write SetFBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word read FBackgroundRandomizeMaxIntensity write SetFBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read FBackgroundRandomize write SetFBackgroundRandomize;
  end;

implementation

uses BGRAGradients;

{ TBGRAFlashProgressBar }

procedure TBGRAFlashProgressBar.SetMinValue(const AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetValue(const AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.paint(const canvas: tcanvas);
var
  content: TRect;
  xpos, y, tx, ty: integer;
  bgColor: TBGRAPixel;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := Color;

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    //InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  tx := ClientWidth;
  ty := ClientHeight;
  if Assigned(FBmp) and ((FBmp.Width <> tx) or (FBmp.Height <> ty)) then
    FreeAndNil(FBmp);

  if not Assigned(FBmp) then
    FBmp := TBGRABitmap.Create(tx, ty)
  else
    FBmp.FillTransparent;

  FBmp.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), BackgroundColor, dmSet);
  if (tx > 2) and (ty > 2) then
    FBmp.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    if BackgroundRandomize then
    for y := content.Top to content.Bottom - 1 do
    begin
      bgColor := BackgroundColor;
      bgColor.Intensity := RandomRange(BackgroundRandomizeMinIntensity, BackgroundRandomizeMaxIntensity);
      FBmp.HorizLine(content.Left, y, content.Right - 1, bgColor, dmSet);
    end;
    if tx >= 6 then
      FBmp.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if FMaxValue > FMinValue then
    begin
      xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          FBmp.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          FBmp.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
  if Assigned(OnRedraw) then
    OnRedraw(Self, FBmp, {%H-}xpos);
  FBmp.Draw(Canvas, 0, 0, False);
end;

constructor TBGRAFlashProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FBmp := nil;
  randomize;
  FRandSeed := randseed;
  //Color := BGRA(102, 163, 226);
  //BackgroundColor := BGRA(47,47,47);
  BackgroundRandomize := True;
  BackgroundRandomizeMinIntensity := 4000;
  BackgroundRandomizeMaxIntensity := 5000;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBmp);
  inherited Destroy;
end;

procedure TBGRAFlashProgressBar.SetMaxValue(const AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundColor(AValue: colorty);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomize(AValue: boolean);
begin
  if FBackgroundRandomize=AValue then Exit;
  FBackgroundRandomize:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomizeMaxIntensity(AValue: word
  );
begin
  if FBackgroundRandomizeMaxIntensity=AValue then Exit;
  FBackgroundRandomizeMaxIntensity:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomizeMinIntensity(AValue: word
  );
begin
  if FBackgroundRandomizeMinIntensity=AValue then Exit;
  FBackgroundRandomizeMinIntensity:=AValue;
  Invalidate;
end;

end.
