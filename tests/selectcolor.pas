program selectcolor;

{$apptype GUI}

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid,
  wgdbgrid, gfxdialogs, wgcheckbox;

type

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnSelectColor : TWGBUTTON;
    lbColorInfo : TWGLABEL;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;

    procedure SelectClick(sender : TObject);
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(300,100,419,56);
  WindowTitle8 := 'LPTK color selection demo';

  btnSelectColor := TWGBUTTON.Create(self);
  with btnSelectColor do
  begin
    SetDimensions(12,16,101,24);
    Text := u8('Select color...');
    OnClick := SelectClick;
  end;

  lbColorInfo := TWGLABEL.Create(self);
  with lbColorInfo do
  begin
    SetDimensions(120,20,284,16);
    Text := u8('Color Info');
  end;

  {@VFD_BODY_END: frmMain}
end;



procedure TfrmMain.SelectClick(sender: TObject);
var
  c : TGfxColor;
begin
  c := lbColorInfo.Color;
  if SelectColorDialog(c) then lbColorInfo.Color := c;
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
