program dirtreetest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys,messagequeue, schar16, gfxbase, gfxstyle, gfxform, wgdirtree, gfximagelist;

type
    TMainForm = class(TGfxForm)
      public
        ImageList : TgfxImageList;
	DirTree : TwgDirTree;
	procedure AfterCreate; override;
    end;

var
    MainForm : TGfxForm;

procedure TMainForm.AfterCreate;
var
   ImageItem : TgfxImageItem;
begin
    inherited;
    ImageList := TgfxImageList.Create;
    ImageItem := TgfxImageItem.Create;
    ImageItem.LoadFromFile('../stdimg/stdimg_folder16x16.bmp');
    ImageList.Item[0] := ImageItem;
    SetDimensions(500,10,400,400);
    WindowTitle8 := 'DirTree-Test';
    DirTree := TwgDirTree.Create(self);
    DirTree.ImageList := ImageList;
    DirTree.ShowImages := true;
    DirTree.DirectoryIndex := 0;   // Index of the Folder-Image in the ImageList
    DirTree.top := 100;
    DirTree.left := 10;
    DirTree.height := 200;
    DirTree.width := 200;
    DirTree.anchors := [anLeft,anBottom, anRight, anTop];
end;


begin
    GfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    GfxDoMessageLoop;    
    GfxCloseDisplay;    
end.

