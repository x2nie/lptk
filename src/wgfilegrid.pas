{ wgfilegrid.pas: FileGrid widget
  File maintainer: Erik@Grohnwaldt.de

  History: }
// $Log$
// Revision 1.2  2003/12/11 11:58:37  aegluke
// linux changes
//
// Revision 1.1  2003/12/10 19:09:10  aegluke
// Initial release, only for presentation
//

unit wgfilegrid;

interface

uses
  gfxbase, wgcustomgrid, classes, schar16, sysutils;

const ftDirectory = faDirectory;

type
  TfgOptions = set of (fgDirectories, fgFiles, fgDetail);
  // Display-Options: fgDirectories shows directories in the grid
  //                  fgFiles shows files in the grid
  //                  fgDetail: detail-informations for files
  //                    - if fgDetail is set, every row shows only one file with its informations

  TwgFileData = class
    public
    // contains data for one file shown in detail-view
    FileName : String;
    FileSize : Integer;
    FileDate : TDateTime;
    FileType : Integer;
  end;

  TwgFileGrid = class(TwgCustomGrid)
    private
      FDirectory : String;
      FOptions : TfgOptions;
      FFiles : TList;
    protected
      procedure ReadDirectory;
      procedure SetDirectory(aValue : String);
      procedure SetOptions(aValue : TfgOptions);

      // hide properties
      property RowCount;
      property ColumnCount;
      property Columns;
      procedure ReCalcGrid;
    public
      procedure DrawCell(aRow, aCol : integer; aRect : TGfxRect; aFlags : integer); override;
      property Directory : String read FDirectory write SetDirectory;
      property Options : TfgOptions read FOptions write SetOptions;
      constructor Create(aOwner : TComponent); override;
  end;

implementation

uses
  gfxstyle;

procedure TwgFileGrid.RecalcGrid;
begin
  if fgDetail in FOptions then
  begin
    RowCount := FFiles.Count;
    ColumnCount := 1;
    AddColumn8('File',100);
    AddColumn8('Size',72);
    AddColumn8('Date',72);
    AddColumn8('Type',72);
    Columns[0].Alignment := alLeft;
    Columns[1].Alignment := alRight;
    Columns[2].Alignment := alLeft;
    Columns[3].Alignment := alLeft;
  end;
end;

procedure TwgFileGrid.DrawCell(aRow, aCol: integer; aRect: TGfxRect; aFlags: integer);
var
  s : string16;
  i : integer;
begin
  if fgDetail in FOptions then
  begin
    case aCol of
      1 : begin
        s := Str8To16(TwgFileData(FFiles[aRow-1]).FileName);
        canvas.DrawString16(aRect.left+1, aRect.top+1, s);
      end;
      2 : begin
        s := Str8To16(IntToStr(TwgFileData(FFiles[aRow-1]).FileSize));
        canvas.DrawString16(aRect.right - guistyle.GridFont.TextWidth16(s) - 1, aRect.top+1, s);
      end;
      3 : begin
        s := Str8To16(DateToStr(TwgFileData(FFiles[aRow-1]).FileDate));
        canvas.DrawString16(aRect.right - guistyle.GridFont.TextWidth16(s) - 1, aRect.top+1, s);
      end;
      4 : begin
        case TwgFileData(FFiles[aRow-1]).FileType of
          ftDirectory : begin
            s := Str8To16('Directory');
            canvas.DrawString16(aRect.left+1, aRect.top+1, s);
          end;
        end;    
      end;
    end;
  end
  else
  begin
  end;
end;

constructor TwgFileGrid.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FFiles := TList.Create;
  ColumnCount := 1;
  FOptions := [fgDetail];
end;

procedure TwgFileGrid.SetOptions(aValue : TfgOptions);
begin
  if aValue <> FOptions then
  begin
    FOptions := aValue;
    ReadDirectory;
    RePaint;
  end;
end;

procedure TwgFileGrid.ReadDirectory;
var
  rec : TSearchRec;
  FileAttr : integer;
  Container : TwgFileData;
  SearchStr : String;
begin
  FFiles.Clear;
  {$ifdef win32}
          SearchStr := FDirectory + '\*';
  {$else}
         SearchStr := FDirectory + '/*';
  {$endif}
  FileAttr := faAnyFile;
  if FindFirst(SearchStr, FileAttr, rec) = 0 then
  begin
    repeat
      Container := TwgFileData.Create;
      Container.FileName := rec.Name;
      Container.FileSize := rec.Size;
      Container.FileDate := FileDateToDateTime(rec.Time);
      if (faDirectory and rec.Attr) = faDirectory then
        Container.FileType := ftDirectory;
      FFiles.Add(Container);
    until FindNext(rec) <> 0;
  end;
  RecalcGrid;
end;

procedure TwgFileGrid.SetDirectory(aValue : String);
begin
  if DirectoryExists(aValue) and (aValue <> FDirectory) then
  begin
    FDirectory := aValue;
    ReadDirectory;
    RePaint;
  end;
end;

end.
