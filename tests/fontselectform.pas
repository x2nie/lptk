program fontselect;

{$APPTYPE GUI}

{$IFDEF FPC}
    {$mode delphi}
    {$H+}
{$ENDIF}

uses SysUtils, Classes, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  wgedit, gfxwidget, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, gfxdialogs, wgcheckbox
  ;

type

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnSelectFont : TwgButton;
    lbFontInfo : TwgLabel;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;

    procedure FontSelectClick(sender : TObject);
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(300,100,419,56);
  WindowTitle8 := 'LPTK Font selection demo';

  btnSelectFont := TwgButton.Create(self);
  with btnSelectFont do
  begin
    SetDimensions(12,16,101,24);
    Text := u8('Select font...');
    OnClick := FontSelectClick;
  end;

  lbFontInfo := TwgLabel.Create(self);
  with lbFontInfo do
  begin
    SetDimensions(120,20,284,16);
    Text := u8('lbFontInfo');
  end;

  {@VFD_BODY_END: frmMain}
end;



procedure TfrmMain.FontSelectClick(sender: TObject);
var
  s : string;
begin
  s := str16to8(lbFontInfo.Text);
  if SelectFontDialog(s) then lbFontInfo.Text := u8(s);
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
