program styleeditor;

{$IFDEF FPC}
    {$mode delphi}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses SysUtils, Classes, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  wgedit, gfxwidget, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, gfxdialogs, wgcheckbox,
  wgcustomgrid
  ;

type

  TwgStyleGrid = class(TwgCustomGrid)
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnOpen : TwgButton;
    btnSave : TwgButton;
    grid : TwgStyleGrid;
    btnClose : TwgButton;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(300,100,411,311);
  WindowTitle8 := 'frmMain';

  btnOpen := TwgButton.Create(self);
  with btnOpen do
  begin
    SetDimensions(4,4,80,24);
    Text := u8('Open...');
    ImageName := 'stdimg.open';
  end;

  btnSave := TwgButton.Create(self);
  with btnSave do
  begin
    SetDimensions(92,4,80,24); 
    Text := u8('Save');
    ImageName := 'stdimg.save';
  end;

  grid := TwgStyleGrid.Create(self);
  with grid do
  begin
    SetDimensions(4,36,400,268);
  end;

  btnClose := TwgButton.Create(self);
  with btnClose do
  begin
    SetDimensions(324,4,80,24);
    Text := u8('Close');
    ImageName := 'stdimg.exit';
  end;

  {@VFD_BODY_END: frmMain}
end;

var
  mfrm : TfrmMain;
{ TwgStyleGrid }

constructor TwgStyleGrid.Create(AOwner: TComponent);
begin
  inherited;
  AddColumn8('Settings Name',120);
  AddColumn8('Type',60);
  AddColumn8('Use',30);
  AddColumn8('Set',30);
  AddColumn8('Value',60);
  AddColumn8('Sample',80);
end;

begin
  gfxOpenDisplay('');
  mfrm := TfrmMain.Create(nil);
  mfrm.Show;
  gfxDoMessageLoop;
  gfxCloseDisplay;
end.
