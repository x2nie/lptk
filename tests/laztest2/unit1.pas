unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, hd_defs, hd_main, hd_form, hd_button;

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

