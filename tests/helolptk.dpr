program helolptk;

{$apptype GUI}

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

uses
  SysUtils, Classes, gfxbase, schar16, gfxform, wglabel, wgbutton;

type
  TfrmMain = class(TGfxForm)
  public
    btnClose : TwgButton;
    lbLabel1 : TwgLabel;

    procedure AfterCreate; override;

    procedure CloseClick(sender : TObject);
  end;

procedure TfrmMain.AfterCreate;
begin
  SetDimensions(338,154,153,81);
  WindowTitle8 := 'Hello LPTK ';

  lbLabel1 := CreateLabel(self, 24,12, 'Hello from LPTK');

  btnClose := CreateButton(self, 28, 40, 96, 'Close', nil);
  btnClose.OnClick := CloseClick;
  btnClose.ImageName := 'stdimg.close';
end;                                

procedure TfrmMain.CloseClick(sender: TObject);
begin
  Close;
end;

var
  mfrm : TfrmMain;
begin
  gfxOpenDisplay('');
  mfrm := TfrmMain.Create(nil);
  mfrm.Show;
  gfxDoMessageLoop;
  gfxCloseDisplay;
end.
