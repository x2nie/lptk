unit Unit2;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, hd_defs, hd_main, hd_form, hd_button, hd_progressbar;

type

  { TpgfForm2 }

  TpgfForm2 = class(TpgfForm)
    wgButton1: TwgButton;
    wgProgressbar1: TwgProgressbar;
    procedure wgButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  pgfForm2: TpgfForm2;

implementation

{$R *.lfm}

{ TpgfForm2 }

procedure TpgfForm2.wgButton1Click(Sender: TObject);
begin
  self.Close;
end;

end.

