unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hd_base, hd_main, hd_form, hd_button;

type

  { TpgfForm2 }

  TpgfForm2 = class(TpgfForm)
    wgButton1: TwgButton;
    wgButton2: TwgButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  pgfForm2: TpgfForm2;

implementation

{$R *.lfm}

end.

