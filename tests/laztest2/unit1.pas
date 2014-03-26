unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, hd_defs, hd_main, hd_form,hd_imgfmt_bmp, hd_button;

type

  { TpgfForm1 }

  TpgfForm1 = class(TpgfForm)
    wgButton1: TwgButton;
    wgButton2: TwgButton;
    procedure wgButton1Click(Sender: TObject);
    procedure wgButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  pgfForm1: TpgfForm1;

implementation
uses unit2;
{$R *.lfm}

{ TpgfForm1 }

procedure TpgfForm1.wgButton2Click(Sender: TObject);
begin
  pgfForm2.visible := true;
  pgfForm2.Show;
end;

procedure TpgfForm1.wgButton1Click(Sender: TObject);
begin
  close;
end;

end.

