program dirtreetest;

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys, messagequeue, schar16, gfxbase, gfxstyle, gfxform,
wgfilegrid;

type

  TMainForm = class(TGfxForm)

  public
    FileGrid: TwgFileGrid;
    procedure AfterCreate; override;
  end;

var
  MainForm: TGfxForm;

procedure TMainForm.AfterCreate;
begin
  inherited;
  SetDimensions(500, 10, 400, 400);
  WindowTitle8 := 'FileGrid-Test';
  FileGrid := TwgFileGrid.create(self);
  FileGrid.Directory := 'C:\';
  FileGrid.Options := [fgDirectories, fgFiles, fgDirectoriesFirst];
  FileGrid.setDimensions(10, 10, 300, 300);
  FileGrid.DirectoryColor := $FF0000;
  FileGrid.anchors := [anLeft, anBottom, anRight, anTop];
end;

begin

  GfxOpenDisplay('');

  MainForm := TMainForm.Create(nil);

  MainForm.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;

end.

