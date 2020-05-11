unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BGButton1: TBGButton;
    BGButton2: TBGButton;
    procedure BGButton1Click(Sender: TObject);
    procedure BGButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.BGButton1Click(Sender: TObject);
begin
  ShowMessage('Hello from BGButton!');
end;

procedure TfrmMain.BGButton2Click(Sender: TObject);
begin
  BGButton1.Enabled := not BGButton1.Enabled;
end;

end.

