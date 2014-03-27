unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hd_base, hd_main, hd_form, wgbutton;

type

  { TpgfForm1 }

  TpgfForm1 = class(TpgfForm)
    wgButton1: TwgButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  pgfForm1: TpgfForm1;

implementation

{$R *.lfm}

end.

