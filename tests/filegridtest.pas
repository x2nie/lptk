program filegridtest;

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys, messagequeue, schar16, gfxbase, gfxstyle, gfxform, wgfilegrid, gfximagelist, wgbutton;

type

  TMainForm = class(TGfxForm)
  public
    ImageList : TgfxImageList;
    FileGrid: TwgFileGrid;
    Button : TwgButton;
    procedure AfterCreate; override;
    procedure ButtonClick(ASender : TObject);
    procedure GetFileGridImage(ASender : TObject; AFileData : TwgFileData; var AImageIndex : integer);
  end;

var
  MainForm: TGfxForm;

procedure TMainForm.GetFileGridImage(ASender : TObject; AFileData : TwgFileData; var AImageIndex : integer);
begin
     if AFileData.FileType = ftDirectory then
        AImageIndex := 0
     else
         AImageIndex := 1;
end;

procedure TMainForm.ButtonClick(ASender : TObject);
begin
     if fgDetail in FileGrid.Options then
     begin
          FileGrid.Options := [fgDirectories, fgFiles, fgDirectoriesFirst];
          Button.Text := Str8To16('Detail');
     end
     else
     begin
          Button.Text := Str8To16('List');
          FileGrid.Options := [fgDirectories, fgFiles, fgDirectoriesFirst, fgDetail];
     end;
end;

procedure TMainForm.AfterCreate;
var
   AImageItem : TgfxImageItem;
begin
  inherited;
  SetDimensions(500, 10, 400, 400);
  WindowTitle8 := 'FileGrid-Test';
  ImageList := TgfxImageList.Create;
  AImageItem := TgfxImageItem.Create;
  AImageItem.LoadFromFile('../stdimg/stdimg_folder16x16.bmp');
  ImageList.Item[0] := AImageItem;
  AImageItem := TgfxImageItem.Create;
  AImageItem.LoadFromFile('../stdimg/stdimg_file16x16.bmp');
  ImageList.Item[1] := AImageItem;
  Button := TwgButton.Create(Self);
  Button.SetDimensions(10,10,80,Button.Font.Height+5);
  Button.Text := Str8To16('List');
  Button.OnClick := {$IFDEF fpc}@{$ENDIF}ButtonClick;
  FileGrid := TwgFileGrid.create(self);
  FileGrid.OnImageIndex := {$IFDEF fpc}@{$ENDIF}GetFileGridImage;
  FileGrid.ImageList := ImageList;
  FileGrid.ShowImages := True;
  FileGrid.Directory := '/';
  FileGrid.Options := [fgDirectories, fgFiles, fgDirectoriesFirst, fgDetail];
  FileGrid.setDimensions(10, 80, 300, 300);
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

