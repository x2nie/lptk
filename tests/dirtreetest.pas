program dirtreetest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys,messagequeue, schar16, gfxbase, gfxstyle, gfxform, wgdirtree;

type
    TMainForm = class(TGfxForm)
      public
	DirTree : TwgDirTree;
	procedure AfterCreate; override;
    end;

var
    MainForm : TGfxForm;

procedure TMainForm.AfterCreate;
begin
    inherited;
    SetDimensions(500,10,400,400);
    WindowTitle8 := 'DirTree-Test';
    DirTree := TwgDirTree.Create(self);
    DirTree.top := 100;
    DirTree.left := 10;
    DirTree.height := 200;
    DirTree.width := 200;
    DirTree.anchors := [anLeft,anBottom, anRight, anTop];
//    DirTree.ReadDirectories(DirTree.RootNode);
end;


begin
    GfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    GfxDoMessageLoop;    
    GfxCloseDisplay;    
end.

