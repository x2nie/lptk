// TwgFileDialog - a simple FileOpen/Save-Dialog
// File maintainer: Erik Grohnwaldt (Erik@Grohnwaldt.de)

{$IFDEF FPC}
    {$mode objfpc}
    {$h+}
{$ENDIF}
unit
    wgfiledialog;
    
interface

uses
    gfxbase, wgfilegrid, wgdirtree, wgedit, gfxform, wgbutton, gfxstyle, classes,
    gfximagelist, schar16;

type

  TwgFileDialog = class(TgfxForm)
	private
	    FFileGrid : TwgFileGrid;
            FOKBtn : TwgButton;
            FCancelBtn : TwgButton;
	    FDirTree : TwgDirTreePopup;
            FDetailBtn : TwgButton;
            FFileName : TwgEdit;
            FFileExists : Boolean;
            FImageList : TgfxImageList;
	protected
	    procedure AfterCreate; override;
      procedure TreeDirectoryChange(Sender : TObject);
      procedure GridDirectoryChange(ASender : TObject; AFileName : String);
      procedure GridFocusChange(ASender : TObject; ARow, ACol : Integer);
            procedure CancelClick(ASender : TObject);
            procedure DetailClick(ASender : TObject);
            procedure FilenameChange(ASender : TObject);
            procedure OkClick(ASender : TObject);
            procedure FileGridImageIndex(Sender : TObject; AFileData : TwgFileData; var AImageIndex : integer);
	    procedure GridDoubleClick(ASender : TObject; AX, AY : TgfxCoord; var AButton : Word; var ShiftState : Word);
            function GetFileName : String;
            procedure SetFileName(AValue : String);
            procedure SetDirectory(AValue : String);
            function GetDirectory : String;
            function GetFullFilename : String;
            procedure SetFullFilename(aValue : String);
	public
	    function Execute : Boolean;
            constructor Create(AOwner : TComponent); override;
            property FileExists : Boolean read FFileExists write FFileExists;
            property FileName : String read GetFileName write SetFileName;
            property Directory : String read GetDirectory write SetDirectory;
            property FullFilename : String read GetFullFilename write SetFullFilename;
   end;

implementation

uses
    sysutils, gfxbmpimage;

var
  initialized : boolean;

procedure InitImages;
begin
  if initialized then Exit;

  // GfxLibAddMaskedBMP('btn.lens',@stdimg_lens16x16,sizeof(stdimg_lens16x16),0,0);

  initialized := true;
end;

procedure TwgFileDialog.SetFullFilename(aValue : String);
begin
  Directory := ExtractFilePath(aValue);
  Filename := ExtractFilename(aValue);
end;

function TwgFileDialog.GetFullFilename : String;
begin
  result := Directory + Filename;
end;

function TwgFileDialog.GetDirectory : String;
begin
  result := fDirTree.ActiveDirectory;
end;

procedure TwgFileDialog.SetDirectory( aValue : String );
begin
  if DirectoryExists(aValue) then
  begin
    fFileGrid.Directory := aValue;
    fDirTree.ActiveDirectory := aValue;
  end;
end;

procedure TwgFileDialog.GridDoubleClick(ASender : TObject; AX, AY : TgfxCoord; var AButton : Word; var ShiftState : Word);
begin
    OkClick(ASender);
end;

function TwgFileDialog.GetFileName : String;
begin
     result := FFileName.Text8;
end;

procedure TwgFileDialog.SetFileName(AValue : String);
begin
     FFileName.Text8 := AValue;
     FFileGrid.LocateFile(AValue);
end;

procedure TwgFileDialog.FileGridImageIndex(Sender : TObject; AFileData : TwgFileData; var AImageIndex : integer);
begin
     if AFileData.FileType = ftDirectory then AImageIndex := 1
     else AImageIndex := 0;
end;

constructor TwgFileDialog.Create(AOwner : TComponent);
begin
     inherited Create(nil);
end;

procedure TwgFileDialog.OkClick(ASender : TObject);
var
   AClose : Boolean;
begin
     AClose := True;
     if FFileExists and (not sysutils.FileExists(FFileName.Text8)) then AClose := False;
     if AClose then
     begin
          ModalResult := 1;
          Close;
     end;
end;

procedure TwgFileDialog.GridFocusChange(ASender : TObject; ARow, ACol : Integer);
begin
     FFileName.Text8 := ExtractFileName(FFileGrid.FileName);
end;

procedure TwgFileDialog.FileNameChange(ASender : TObject);
begin
     FFileGrid.LocateFile(FFileName.Text8);
end;

procedure TwgFileDialog.GridDirectoryChange(ASender : TObject; AFileName : String);
begin
     FDirTree.ActiveDirectory := AFileName;
end;

procedure TwgFileDialog.TreeDirectoryChange(Sender : TObject);
begin
     FFileGrid.Directory := FDirTree.ActiveDirectory;
end;

function TwgFileDialog.Execute : Boolean;
begin
    result := ShowModal = 1;
end;

procedure TwgFileDialog.CancelClick(ASender : TObject);
begin
     ModalResult := 0;
     Close;
end;

procedure TwgFileDialog.DetailClick(ASender : TObject);
begin
   if FDetailBtn.Down then
      FFileGrid.Options := FFileGrid.Options + [fgDetail]
   else
     FFileGrid.Options := FFileGrid.Options - [fgDetail];
end;

procedure TwgFileDialog.AfterCreate;
var
   Image : TgfxImageItem;
begin
    inherited AfterCreate;
    
    InitImages;
    
    Resizeable := False;
    SetDimensions(0,0,400,310);
    FFileExists := False;
    FOKBtn := CreateButton(Self,10,280,115,'OK',{$IFDEF FPC}@{$ENDIF}OkClick);
    FOkBtn.ImageName := 'stdimg.ok';

    FCancelBtn := CreateButton(Self,125,280,115,'Cancel',{$IFDEF FPC}@{$ENDIF}CancelClick);
    FCancelBtn.ImageName := 'stdimg.cancel';

    FDetailBtn := CreateButton(Self,245,10,32,'',{$IFDEF FPC}@{$ENDIF}DetailClick);
    FDetailBtn.Width := FDetailBtn.Height;
    FDetailBtn.ImageName := 'stdimg.find';

    FDetailBtn.AllowDown := True;
    FFileName := TwgEdit.Create(Self);
    FFileName.SetDimensions(10,250,380,FFileName.Font.Height + 4);

    FImageList := TgfxImageList.Create;
    FImageList.AddImage(GfxLibGetImage('stdimg.document'),0);
    FImageList.AddImage(GfxLibGetImage('stdimg.folder'),1);
    FImageList.AddImage(GfxLibGetImage('stdimg.ok'),2);
    FImageList.AddImage(GfxLibGetImage('stdimg.cancel'),3);
    FImageList.AddImage(GfxLibGetImage('stdimg.find'),4);

    FDirTree := TwgDirTreePopup.Create(Self);
    FDirTree.SetDimensions(10,10,230,FDetailBtn.Height);
    FDirTree.ImageList := FImageList;
    FDirTree.ImageIndex := 1;
    FDirTree.ShowImages := True;

    FFileGrid := TwgFileGrid.Create(Self);
    FFileGrid.Directory := FDirTree.ActiveDirectory;
    FFileGrid.Options := [fgFiles,fgDirectories, fgDirectoriesFirst];
    FFileGrid.SetDimensions(10,FDirTree.Font.Height + 24, 380,200);
    FFileGrid.onDirectoryChange := {$IFDEF fpc}@{$ENDIF}GridDirectoryChange;
    FFileGrid.onFocusChange := {$IFDEF fpc}@{$ENDIF}GridFocusChange;
    FFileGrid.onImageIndex := {$IFDEF fpc}@{$ENDIF}FileGridImageIndex;
    FFileGrid.ImageList := FImageList;
    FFileGrid.ShowImages := True;
    FFileGrid.OnDoubleClick := {$IFDEF FPC}@{$ENDIF}GridDoubleClick;
    FDirTree.onChange := {$IFDEF fpc}@{$ENDIF}TreeDirectoryChange;
    FFileName.onChange := {$IFDEF fpc}@{$ENDIF}FileNameChange;
end;

initialization
begin
  initialized := false;
end;

end.
