unit BGRAFlashProgressBar;

interface

uses
  msesimplewidgets, mseevent, msegraphics, classes, sysutils, mclasses, msegraphutils, mseact,
  mseguiglob, msewidgets, bgrabitmap, bgragraphics, bgradrawerflashprogressbar;

type
  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TPaintBox)
  private
    FBGRA: TBGRABitmap;
    FDrawer: TBGRADrawerFlashProgressBar;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    function GetBackgroundColor: TColor;
    function GetBackgroundRandomize: boolean;
    function GetBackgroundRandomizeMaxIntensity: word;
    function GetBackgroundRandomizeMinIntensity: word;
    function GetBarColor: TColor;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
    procedure OnChangeDrawer(Sender: TObject);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
  protected
    procedure Paint(const Canvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MinValue: integer Read GetMinValue Write SetMinValue;
    property MaxValue: integer Read GetMaxValue Write SetMaxValue;
    property Value: integer Read GetValue Write SetValue;
    property BarColor: TColor read GetBarColor write SetBarColor;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word read GetBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word read GetBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read GetBackgroundRandomize write SetBackgroundRandomize;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnredraw write FOnRedraw;
  end;

implementation

uses BGRABitmapTypes;

procedure TBGRAFlashProgressBar.SetMinValue(const AValue: integer);
begin
  FDrawer.MinValue := AValue;
end;

procedure TBGRAFlashProgressBar.SetValue(const AValue: integer);
begin
  FDrawer.Value := AValue;
end;

procedure TBGRAFlashProgressBar.Paint(const Canvas: TCanvas);
begin
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height) then
    FBGRA.SetSize(ClientWidth, ClientHeight);
  FDrawer.Draw(FBGRA);
  if Assigned(OnRedraw) then
    OnRedraw(Self, FBGRA, {%H-}FDrawer.XPosition);
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TBGRAFlashProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Bitmap and Drawer
  FBGRA := TBGRABitmap.Create(Width, Height);
  FDrawer := TBGRADrawerFlashProgressBar.Create;
  FDrawer.OnChange := @OnChangeDrawer;
  // Functionality
  MinValue := 0;
  MaxValue := 100;
  Value := 30;
  // Functionality and Style
  Randomize;
  FDrawer.RandSeed := RandSeed;
  // Style
  Color := cl_transparent;
  BarColor := BGRA(102, 163, 226);
  BackgroundColor := BGRA(47,47,47);
  BackgroundRandomize := True;
  BackgroundRandomizeMinIntensity := 4000;
  BackgroundRandomizeMaxIntensity := 5000;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBGRA);
  FDrawer.Free;
  inherited Destroy;
end;

procedure TBGRAFlashProgressBar.SetMaxValue(const AValue: integer);
begin
  FDrawer.MaxValue := AValue;
end;

procedure TBGRAFlashProgressBar.OnChangeDrawer(Sender: TObject);
begin
  Invalidate;
end;

function TBGRAFlashProgressBar.GetBackgroundColor: TColor;
begin
  Result := FDrawer.BackgroundColor;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomize: boolean;
begin
  Result := FDrawer.BackgroundRandomize;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomizeMaxIntensity: word;
begin
  Result := FDrawer.BackgroundRandomizeMaxIntensity;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomizeMinIntensity: word;
begin
  Result := FDrawer.BackgroundRandomizeMinIntensity;
end;

function TBGRAFlashProgressBar.GetBarColor: TColor;
begin
  Result := FDrawer.BarColor;
end;

function TBGRAFlashProgressBar.GetMaxValue: integer;
begin
  Result := FDrawer.MaxValue;
end;

function TBGRAFlashProgressBar.GetMinValue: integer;
begin
  Result := FDrawer.MinValue;
end;

function TBGRAFlashProgressBar.GetValue: integer;
begin
  Result := FDrawer.Value;
end;

procedure TBGRAFlashProgressBar.SetBackgroundColor(AValue: TColor);
begin
  FDrawer.BackgroundColor := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomize(AValue: boolean);
begin
  FDrawer.BackgroundRandomize := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMaxIntensity(AValue: word
  );
begin
  FDrawer.BackgroundRandomizeMaxIntensity := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMinIntensity(AValue: word
  );
begin
  FDrawer.BackgroundRandomizeMinIntensity := AValue;
end;

procedure TBGRAFlashProgressBar.SetBarColor(AValue: TColor);
begin
  FDrawer.BarColor := AValue;
end;

end.
