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
        Image : TgfxImage;
        ImageList : TgfxImageList;
	DirTree : TwgDirTree;
	procedure RePaint; override;
	procedure AfterCreate; override;
    end;

var
    MainForm : TGfxForm;

procedure TMainForm.RePaint;
begin
    inherited RePaint;
    Canvas.DrawImage(10,10,image);
end;

procedure TMainForm.AfterCreate;
var
   ImageItem : TgfxImageItem;
begin
    inherited;
    ImageList := TgfxImageList.Create;
    ImageItem := TgfxImageItem.Create;
    ImageItem.Image := TgfxImage.Create;
    try
	ImageItem.Image.LoadFromFile('../stdimg/stdimg_folder16x16.bmp');
    except
             on e : exception do writeln(e.message);
    end;
    ImageList.Item[0] := ImageItem;
    SetDimensions(10,10,220,220);
    WindowTitle8 := 'DirTree-Test';
    DirTree := TwgDirTree.Create(self);
    DirTree.ImageList := ImageList;
    DirTree.ShowImages := true;
    DirTree.DirectoryIndex := 0;   // Index of the Folder-Image in the ImageList
    DirTree.top := 10;
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

