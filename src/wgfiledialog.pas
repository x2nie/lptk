{$IFDEF FPC}
    {$mode objfpc}
    {$h+}
{$ENDIF}
unit
    wgfiledialog;
    
interface

uses
    gfxbase, wgfilegrid, wgdirtree, wgedit, gfxform, wgbutton, gfxstyle, wgchoicelist, classes, schar16;

type
    TwgFileDialog = class(TgfxForm)
	private
	    FFileGrid : TwgFileGrid;
            FOKBtn : TwgButton;
            FCancelBtn : TwgButton;
	    FDirTree : TwgDirTreePopup;	    
            FDetailBtn : TwgButton;
            FFileName : TwgEdit;
            FChoiceList : TwgChoiceList;
	protected
	    procedure AfterCreate; override;
            procedure TreeDirectoryChange(Sender : TObject);
            procedure GridDirectoryChange(ASender : TObject; AFileName : String);
            procedure GridFocusChange(ASender : TObject; ARow, ACol : Integer);
            procedure CancelClick(ASender : TObject);
            procedure DetailClick(ASender : TObject);
            procedure FilenameChange(ASender : TObject);
	public
	    function Execute : Boolean;
            constructor Create(AOwner : TComponent); override;
    end;

implementation

uses
    sysutils;

constructor TwgFileDialog.Create(AOwner : TComponent);
begin
     inherited Create(nil);
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
begin
    inherited AfterCreate;
    Resizeable := False;
    SetDimensions(0,0,400,310);

    FOKBtn := CreateButton(Self,10,280,115,'OK',nil);
    FCancelBtn := CreateButton(Self,125,280,115,'Cancel',{$IFDEF FPC}@{$ENDIF}CancelClick);
    FDetailBtn := CreateButton(Self,245,10,50,'Detail',{$IFDEF FPC}@{$ENDIF}DetailClick);
    FDetailBtn.AllowDown := True;
    FFileName := TwgEdit.Create(Self);
    FFileName.SetDimensions(10,250,380,FFileName.Font.Height + 4);

    FDirTree := TwgDirTreePopup.Create(Self);
    FDirTree.SetDimensions(10,10,230,FDetailBtn.Height);

    FFileGrid := TwgFileGrid.Create(Self);
    FFileGrid.Directory := FDirTree.ActiveDirectory;
    FFileGrid.Options := [fgFiles,fgDirectories, fgDirectoriesFirst];
    FFileGrid.SetDimensions(10,FDirTree.Font.Height + 24, 380,200);
    FFileGrid.onDirectoryChange := {$IFDEF fpc}@{$ENDIF}GridDirectoryChange;
    FFileGrid.onFocusChange := {$IFDEF fpc}@{$ENDIF}GridFocusChange;
    FDirTree.onChange := {$IFDEF fpc}@{$ENDIF}TreeDirectoryChange;
    FFileName.onChange := {$IFDEF fpc}@{$ENDIF}FileNameChange;
end;

end.
