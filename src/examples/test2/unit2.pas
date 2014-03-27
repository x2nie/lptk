unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, hd_base, hd_main, hd_form, hd_button, wgbutton, Menus;

type

  { TpgfForm1 }

  TpgfForm1 = class(TpgfForm)
    PopupMenu1: TPopupMenu;
    wgButton1: TwgButton;
    wgButton2: TwgButton;
    wgButton3: TwgButton;
    wgButton4: TwgButton;
    wgButton5: TwgButton;
    wgButton6: TwgButton;
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

