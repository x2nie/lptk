{ wgfilegrid.pas: FileGrid widget
  File maintainer: Erik@Grohnwaldt.de

  History: }
// $Log$
// Revision 1.11  2004/01/24 18:35:51  aegluke
// wgfiledialog-changes
//
// Revision 1.10  2004/01/23 12:29:11  aegluke
// LocateFile added
//
// Revision 1.9  2004/01/03 10:28:05  aegluke
// Fixed ScrollBar-Bug
//
// Revision 1.8  2004/01/02 20:43:08  aegluke
// ImageList-Support, Detailed-View-Bugfix
//
// Revision 1.7  2003/12/23 18:27:47  aegluke
// fgDetail-Bugfix in HandleKeyPress
//
// Revision 1.6  2003/12/23 08:24:23  aegluke
// avoid artifacts on directory change
//
// Revision 1.5  2003/12/22 13:07:32  aegluke
// Linux changes
//
// Revision 1.4  2003/12/21 19:16:07  aegluke
// SetFileName
// onFileDoubleClick change onFileChose
// fgDetail-Changes
//
// Revision 1.3  2003/12/20 15:13:01  aegluke
// wgFileGrid-Changes
//
// Revision 1.2  2003/12/11 11:58:37  aegluke
// linux changes
//
// Revision 1.1  2003/12/10 19:09:10  aegluke
// Initial release, only for presentation
//

{$IFDEF FPC}
    {$mode objfpc}
    {$h+}
{$ENDIF}
unit wgfilegrid;
{//$DEFINE DEBUG}
interface

uses
  gfxbase, wgcustomgrid, classes, schar16, gfximagelist, sysutils{$IFDEF win32},windows{$ENDIF};

const
  ftDirectory = 1;
  ftFile = 2;

const
  CDirectoryStr = 'Directory';
  CFileStr = 'File';
  CDateStr = 'Date';
  CSizeStr = 'Size';

type
  TwgFileData = class;
  
  TfgOptions = set of (fgDirectories, fgFiles, fgDetail, fgDirectoriesFirst);
  // Display-Options: fgDirectories shows directories in the grid
  //                  fgFiles shows files in the grid
  //                  fgDetail: detail-informations for files
  //                    - if fgDetail is set, every row shows only one file with its informations
  //                  fgDirectoriesFirst: Directories will showed first in the list

  TwgFileGridChange = procedure(Sender : TObject; Filename : String) of object;
  TwgFileGridImageIndex = procedure(Sender : TObject; AFileData : TwgFileData; var AImageIndex : integer) of Object;
  // returns the image-number -1 for image not set

  TwgFileData = class
  public
    // contains data for one file shown in detail-view
    FileName: string;
    FileSize: Integer;
    FileDate: TDateTime;
    FileType: Integer;
  end;

  TwgFileGrid = class(TwgCustomGrid)
  private
    FDirectory: string;
    FOptions: TfgOptions;
    FFiles: TList;
    FDirectoryColor : TgfxColor;
    FFileColor : TgfxColor;
    FImageList : TgfxImageList;
    FShowImages : Boolean;
    FSort : Boolean;
  protected

    procedure ReadDirectory;
    procedure SetDirectory(aValue: string);
    procedure SetOptions(aValue: TfgOptions);
    procedure HandleDoubleClick(x, y: integer; btnstate, shiftstate: word); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    // hide properties
    property RowCount;
    property ColumnCount;
    property Columns;
    procedure ReCalcGrid;
    procedure FollowFocus; override;
    procedure SetFileColor(AValue : TgfxColor);
    procedure SetDirectoryColor(AValue : TgfxColor);
    procedure SetFileName(AValue : String);
    function GetFileName : String;
    procedure DoDirectoryChange;
    procedure DoFileChose;
    procedure SetImageList(AValue : TgfxImageList);
    procedure SetShowImages(AValue : Boolean);
    procedure DoImageIndex(AFileData : TwgFileData; var AImageIndex : integer);
    procedure SetSort(AValue : Boolean);
    procedure OrderFiles;
    {$IFDEF win32}
    procedure ReadDriveNames;
    {$endif}
  public
    procedure HandleResize(DWidth, DHeight : Integer); override;
    procedure DoShow; override;
    procedure DrawCell(aRow, aCol: integer; aRect: TGfxRect; aFlags: integer); override;
    procedure LocateFile(AFileName : String);
    
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    
    property Directory: string read FDirectory write SetDirectory;
    property Options: TfgOptions read FOptions write SetOptions;
    property DirectoryColor : TgfxColor read FDirectoryColor write SetDirectoryColor;
    property FileColor : TgfxColor read FFileColor write SetFileColor;
    property FileName : String read GetFileName write SetFileName;
    property ImageList : TgfxImageList read FImageList write SetImageList;
    property ShowImages : boolean read FShowImages write SetShowImages;
    property Sort : Boolean read FSort write SetSort;
  public
    onDirectoryChange : TwgFileGridChange;
    onFileChose : TwgFileGridChange;
    onImageIndex : TwgFileGridImageIndex;
  end;

implementation

uses
  gfxstyle;

const
{$IFDEF win32}
  DirSeparator = '\';
{$ELSE}
  DirSeparator = '/';
{$ENDIF}

procedure TwgFileGrid.OrderFiles;
var
   ADirIndex : Integer;
   ACounter : Integer;
   AChanged : Boolean;
   ATmp : Pointer;
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.OrderFiles');
     {$ENDIF}
     AChanged := True;
     ADirIndex := 0;
     if fgDirectoriesFirst in Options then
     begin
          for ACounter := 0 to FFiles.Count - 1 do
              if TwgFileData(FFiles[ACounter]).FileType = ftDirectory then
                 ADirIndex := ACounter;
          while AChanged do
          begin
               AChanged := False;
               for ACounter := 1 to ADirIndex do
               begin
                    if CompareText(TwgFileData(FFiles[ACounter-1]).FileName,TwgFileData(FFiles[ACounter]).FileName) > 0 then
                    begin
                         ATmp := FFiles[ACounter];
                         FFiles[ACounter] := FFiles[ACounter-1];
                         FFiles[ACounter-1] := ATmp;
                         AChanged := True;
                    end;
               end;
          end;
          ADirIndex := ADirIndex + 2;
     end;
     AChanged := True;
     while AChanged do
     begin
       AChanged := False;
       for ACounter := ADirIndex to FFiles.Count - 1 do
       begin
          if CompareText(TwgFileData(FFiles[ACounter-1]).FileName,TwgFileData(FFiles[ACounter]).FileName) > 0 then
          begin
               ATmp := FFiles[ACounter];
               FFiles[ACounter] := FFiles[ACounter-1];
               FFiles[ACounter-1] := ATmp;
               AChanged := True;
          end;
       end;
     end;
end;

procedure TwgFileGrid.SetSort(AValue : Boolean);
begin
     if AValue <> FSort then
     begin
          FSort := AValue;
          ReadDirectory;
     end;
end;

destructor TwgFileGrid.Destroy;
var
   ACounter : Integer;
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.Destroy');
     {$ENDIF}
     for ACounter := 0 to FFiles.Count - 1 do
     begin
          TwgFileData(FFiles[ACounter]).Free;
     end;
     inherited Destroy;
end;

procedure TwgFileGrid.LocateFile(AFileName : String);
var
   AValue : Integer;
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.LocateFile');
     {$ENDIF}
     for AValue := 0 to FFiles.Count - 1 do
     begin
          if TwgFileData(FFiles[AValue]).FileName = AFileName then
          begin
               FileName := AFileName;
               Break;
          end;
     end;
end;

procedure TwgFileGrid.DoImageIndex(AFileData : TwgFileData; var AImageIndex : integer);
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.DoImageIndex');
     {$ENDIF}
     if Assigned(onImageIndex) then
        onImageIndex(Self,AFileData, AImageIndex);
end;

procedure TwgFileGrid.SetShowImages(AValue : Boolean);
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.SetShowImages');
     {$ENDIF}
     if AValue <> FShowImages then
     begin
          FShowImages := AValue;
          if FImageList <> nil then RePaint;
     end;
end;

procedure TwgFileGrid.SetImageList(AValue : TgfxImageList);
begin
     {$IFDEF DEBUG}
     writeln('TwgFileGrid.SetImageList');
     {$ENDIF}
     if FImageList <> AValue then
     begin
          FImageList := AValue;
          if FShowImages then RePaint;
     end;
end;

{$IFDEF win32}
procedure TwgFileGrid.ReadDriveNames;
var
  ADrive : String;
  ACounter : Integer;
  ANumber : Integer;
  Container : TwgFileData;
begin
  FFiles.Clear;
  for ACounter := 0 to 25 do
  begin
    ADrive := Chr(Ord('A')+ACounter) + ':\';
    ANumber := Windows.GetDriveType(PChar(ADrive));
    if ANumber <> 1 then
    begin
      Container := TwgFileData.Create;
      Container.FileName := Chr(Ord('A')+ACounter) + ':';
      Container.FileSize := 0;
      Container.FileDate := Now;
      Container.FileType := ftDirectory;
      FFiles.Add(Container);
    end;
  end;
end;
{$ENDIF}

procedure TwgFileGrid.DoFileChose;
begin
  if Assigned(onFileChose) then
    onFileChose(self,FileName);
end;

procedure TwgFileGrid.DoDirectoryChange;
begin
  if Assigned(onDirectoryChange) then
    onDirectoryChange(self,Directory);
end;

procedure TwgFileGrid.SetFileName(AValue : String);
var
//  AFilePath : String;
  ACounter : integer;
  AFileName : String;
  AFilePos : Integer;
  AFilePath : String;
begin
  AFilePath := ExtractFilePath(AValue);
  if FileExists(AValue) then
  begin
    if AFilePath <> '' then
       Directory := AFilePath;
    AFileName := ExtractFileName(AValue);
    AFilePos := -1;
    for ACounter := 0 to FFiles.Count - 1 do
    begin
      {$IFDEF WIN32}
      if UpperCase(TwgFileData(FFiles[ACounter]).FileName) = UpperCase(AFileName) then
      {$ELSE}
      if TwgFileData(FFiles[ACounter]).FileName = AFileName then
      {$ENDIF}
      begin
        AFilePos := ACounter;
        Break;
      end;
    end;
    if AFilePos > -1 then
    begin
      if fgDetail in Options then
      begin
        FocusRow := AFilePos + 1;
      end
      else
      begin
        FocusRow := (AFilePos) MOD VisibleLines + 1;
        FocusCol := ((AFilePos) DIV VisibleLines) + 1;
        RePaint;
      end;
    end;
  end;
end;

function TwgFileGrid.GetFileName : String;
var
  AFilesPos : integer;
begin
  if fgDetail in Options then
  begin
    result := Directory + TwgFileData(FFiles[FocusRow-1]).FileName;
  end
  else
  begin
    AFilesPos := (FocusCol - 1) * VisibleLines + FocusRow - 1;
    result := Directory + TwgFileData(FFiles[AFilesPos]).FileName;
  end;
end;

procedure TwgFileGrid.SetFileColor(AValue : TgfxColor);
begin
     if AValue <> FFileColor then
     begin
      FFileColor := AValue;
      RePaint;
     end;
end;

procedure TwgFileGrid.SetDirectoryColor(AValue : TgfxColor);
begin
  if AValue <> FDirectoryColor then
  begin
    FDirectoryColor := AValue;
    RePaint;
  end;
end;

procedure TwgFileGrid.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  AFilesPos : Integer;
  AVisibleLines : Integer;
begin
  consumed := false;
  case KeyCode of
      KEY_ENTER : begin
        if not (fgDetail in Options) then
          AFilesPos := (FocusCol - 1) * VisibleLines + FocusRow - 1
        else
          AFilesPos := FocusRow - 1;
        if TwgFileData(FFiles[AFilesPos]).FileType = ftDirectory then
        begin
          {$IFDEF win32}
          if (Length(Directory) = 3) and (TwgFileData(FFiles[AFilesPos]).FileName = '..') then
            Directory := ''
          else
            if Length(Directory) >= 3 then
              Directory := Directory + DirSeparator + TwgFileData(FFiles[AFilesPos]).FileName
            else
              Directory := TwgFileData(FFiles[AFilesPos]).FileName + DirSeparator;
          {$ELSE}
            Directory := Directory + DirSeparator + TwgFileData(FFiles[AFilesPos]).FileName;
          {$ENDIF}
          DoDirectoryChange;
          Consumed := true;
        end
        else
          DoFileChose;
      end;
      KEY_UP : begin
        if not (fgDetail in Options) then
        begin
             if (FocusRow = 1) and (FocusCol > 1) then
             begin
                  FocusCol := FocusCol - 1;
                  FocusRow := RowCount;
                  Consumed := true;
             end
             else
             begin
                  if (FocusRow = 1) and (FocusCol = 1) then
                  begin
                       AVisibleLines := VisibleLines;
                       FocusRow := (FFiles.Count) MOD AVisibleLines;
                       FocusCol := ((FFiles.Count) DIV AVisibleLines) + 1;
                       RePaint;
                       Consumed := true;
                  end;
             end;
        end;
      end;
      KEY_DOWN : begin
        if not (fgDetail in Options) then
        begin
             if FocusRow = VisibleLines then
             begin
                  FocusCol := FocusCol + 1;
                  FocusRow := 1;
                  consumed := true;
             end
             else
             begin
                  AFilesPos := (FocusCol - 1) * VisibleLines + FocusRow - 1;
                  if AFilesPos = FFiles.Count - 1 then
                  begin
                       FocusCol := 1;
                       FocusRow := 1;
                       consumed := true;
                  end;
             end;
        end;
      end;
  end;
  if not consumed then inherited HandleKeyPress(KeyCode, Shiftstate, Consumed);
end;

procedure TwgFileGrid.FollowFocus;
var
  AFilesPos : Integer;
  AVisibleLines : Integer;
begin
  if not (fgDetail in Options) then
  begin
       AVisibleLines := VisibleLines;
       AFilesPos := (FocusCol - 1) * AVisibleLines + FocusRow - 1;
       if AFilesPos > FFiles.Count - 1 then
       begin
            FFocusRow := (FFiles.Count) MOD AVisibleLines;
            FFocusCol := ((FFiles.Count) DIV AVisibleLines) + 1;
       end;
  end;
  inherited FollowFocus;
end;

procedure TwgFileGrid.HandleResize(DWidth, DHeight : Integer);
begin
  inherited HandleResize(DWidth, DHeight);
  ReCalcGrid;
end;

procedure TwgFileGrid.DoShow;
begin
  ReCalcGrid;
  inherited DoShow;
end;

procedure TwgFileGrid.HandleDoubleClick(x, y : integer; btnstate, shiftstate : word);
var
  AFilesPos : Integer;
begin
   if not (fgDetail in Options) then
     AFilesPos := (FocusCol - 1) * VisibleLines + FocusRow - 1
   else
     AFilesPos := FocusRow - 1;
   if TwgFileData(FFiles[AFilesPos]).FileType = ftDirectory then
   begin
          {$IFDEF win32}
          if (Length(Directory) = 3) and (TwgFileData(FFiles[AFilesPos]).FileName = '..') then
            Directory := ''
          else
            if Length(Directory) >= 3 then
              Directory := Directory + DirSeparator + TwgFileData(FFiles[AFilesPos]).FileName
            else
              Directory := TwgFileData(FFiles[AFilesPos]).FileName + DirSeparator;
          {$ELSE}
            Directory := Directory + DirSeparator + TwgFileData(FFiles[AFilesPos]).FileName;
          {$ENDIF}
          DoDirectoryChange;
  end
  else
    DoFileChose;
end;

procedure TwgFileGrid.RecalcGrid;
var
  AColumnWidth : integer;
  ARowCount  : integer;
  AColumnCount : integer;
  AVisibleLines : integer;
  AWidth : integer;
  AFilePos : integer;
  AImageIndex : integer;
begin
  if fgDetail in FOptions then
  begin
    DrawGrid := true;
    RowCount := FFiles.Count;
    ColumnCount := 0;
    RowSelect := true;
    HeadersOn := True;
    AddColumn8(CFileStr, 100);
    AddColumn8(CSizeStr, 72);
    AddColumn8(CDateStr, 72);
    AddColumn8('', 72);
    Columns[0].Alignment := alLeft;
    Columns[1].Alignment := alRight;
    Columns[2].Alignment := alLeft;
    Columns[3].Alignment := alLeft;
  end
  else
  begin
    DrawGrid := false;
    RowSelect := false;
    HeadersOn := false;
    if VisibleLines > 0 then
      begin
      if FFiles.Count < VisibleLines then
      begin
        RowCount := FFiles.Count;
        ColumnCount := 1;
      end
      else
      begin
        ColumnCount := (FFiles.Count div VisibleLines) + 1;
        RowCount := VisibleLines;
      end;
    end;
    AVisibleLines := VisibleLines;
    // resize the columns to show the whole filenames
    for AColumnCount := 1 to ColumnCount do
    begin
      AColumnWidth := 0;
      for ARowCount := 1 to RowCount do
      begin
        AFilePos := (AColumnCount - 1)* AVisibleLines + ARowCount - 1;
        if AFilePos < FFiles.Count then
        begin
          AWidth := guistyle.GridFont.TextWidth16(Str8To16(TwgFileData(FFiles[AFilePos]).FileName))+6;
          AImageIndex := -1;
          if ShowImages and (ImageList <> nil) then
          begin
               DoImageIndex(TwgFileData(FFiles[AFilePos]),AImageIndex);
               if AImageIndex > -1 then
                  AWidth := AWidth + ImageList.Item[AImageIndex].Image.Width + 2;
          end;
          if AWidth > AColumnWidth then AColumnWidth := AWidth;
        end;
      end;
      ColumnWidth[AColumnCount] := AColumnWidth;
    end;
  end;
end;

procedure TwgFileGrid.DrawCell(aRow, aCol: integer; aRect: TGfxRect; aFlags: integer);
var
  s: string16;
  FilesPos : Integer;
  AImageIndex : Integer;
  
  procedure setDrawColor(AFile : TwgFileData; ASelected : Boolean);
  begin
    if ASelected then
    begin
      if FFocused then
      begin
        canvas.SetTextColor(clSelectionText);
      end
      else
      begin
        canvas.SetTextColor(clInactiveSelText);
      end;
    end
    else
    begin
      Canvas.SetTextColor(clText1);
      if (AFile.FileType = ftDirectory) and (DirectoryColor <> clUnset) then
        canvas.SetTextColor(DirectoryColor);
      if (AFile.FileType = ftFile) and (FileColor <> clUnset) then
        canvas.SetTextColor(FileColor);
    end;
  end;
begin
  if fgDetail in FOptions then
  begin
    SetDrawColor(TwgFileData(FFiles[aRow-1]),ARow = FocusRow);
    case ACol of
      1:
        begin
          s := Str8To16(TwgFileData(FFiles[aRow - 1]).FileName);
          canvas.DrawString16(aRect.left + 1, aRect.top + 1, s);
        end;
      2:
        begin
          s := Str8To16(IntToStr(TwgFileData(FFiles[aRow - 1]).FileSize));
          canvas.DrawString16(aRect.right - guistyle.GridFont.TextWidth16(s) - 1,
            aRect.top + 1, s);
        end;
      3:
        begin
          s := Str8To16(DateToStr(TwgFileData(FFiles[aRow - 1]).FileDate));
          canvas.DrawString16(aRect.right - guistyle.GridFont.TextWidth16(s) - 1,
            aRect.top + 1, s);
        end;
      4:
        begin
          case TwgFileData(FFiles[aRow - 1]).FileType of
            ftFile:
              begin
                s := Str8To16(CFileStr);
                canvas.DrawString16(aRect.left + 1, aRect.top + 1, s);
              end;
            ftDirectory:
              begin
                s := Str8To16(CDirectoryStr);
                canvas.DrawString16(aRect.left + 1, aRect.top + 1, s);
              end;
          end;
        end;
    end;
  end
  else
  begin
      FilesPos := (ACol - 1)* VisibleLines + ARow - 1;
      if FilesPos < FFiles.Count then
      begin
        s := Str8To16(TwgFileData(FFiles[FilesPos]).FileName);
        SetDrawColor(TwgFileData(FFiles[FilesPos]),(ARow = FocusRow) and (ACol = FocusCol));
        AImageIndex := -1;
        if ShowImages and (ImageList <> nil) then
        begin
             DoImageIndex(TwgFileData(FFiles[FilesPos]),AImageIndex);
             if AImageIndex > -1 then
             begin
                Canvas.DrawImagePart(aRect.Left + 1,aRect.Height div 2 + aRect.Top - 8,ImageList.Item[AImageIndex].Image,0,0,16,16);
                Canvas.DrawString16(aRect.left + 18, aRect.top + 1, s);
             end
             else
                 Canvas.DrawString16(aRect.left + 1, aRect.top + 1, s);
        end
        else
            Canvas.DrawString16(aRect.left + 1, aRect.top + 1, s);
      end;
  end;
end;

constructor TwgFileGrid.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFiles := TList.Create;
  ColumnCount := 1;
  FSort := False;
  FOptions := [fgDetail, fgDirectoriesFirst];
  FFileColor := clUnset;
  FShowImages := false;
  FImageList := nil;
  FDirectoryColor := clUnset;
  RowSelect := true;
end;

procedure TwgFileGrid.SetOptions(aValue: TfgOptions);
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
  Container: TwgFileData;

  procedure ReadAll;
  var
    rec: TSearchRec;
    FileAttr: Integer;
    Container: TwgFileData;
    SearchStr: string;
  begin
    SearchStr := Directory + DirSeparator + '*';
    FileAttr := faAnyFile;
    if FindFirst(SearchStr, FileAttr, rec) = 0 then
    begin
      repeat
        if (rec.Name <> '.') and (rec.Name <> '..') and ((faDirectory and
          rec.Attr) <> faDirectory) then
        begin
          Container := TwgFileData.Create;
          Container.FileName := rec.Name;
          Container.FileSize := rec.Size;
 //         Container.FileDate := FileDateToDateTime(rec.Time);
          if (faDirectory and rec.Attr) = faDirectory then
            Container.FileType := ftDirectory
          else
            Container.FileType := ftFile;
          FFiles.Add(Container);
        end;
      until FindNext(rec) <> 0;
    end;
  end;

  procedure ReadFiles;
  var
    rec: TSearchRec;
    FileAttr: Integer;
    Container: TwgFileData;
    SearchStr: string;
  begin
    SearchStr := Directory + DirSeparator + '*';
    FileAttr := faAnyFile;
    if FindFirst(SearchStr, FileAttr, rec) = 0 then
    begin
      repeat
        if (rec.Name <> '.') and (rec.Name <> '..') and ((faDirectory and
          rec.Attr) <> faDirectory) then
        begin
          Container := TwgFileData.Create;
          Container.FileName := rec.Name;
          Container.FileSize := rec.Size;
 //         Container.FileDate := FileDateToDateTime(rec.Time);
          if (faDirectory and rec.Attr) = faDirectory then
            Container.FileType := ftDirectory
          else
            Container.FileType := ftFile;
          FFiles.Add(Container);
        end;
      until FindNext(rec) <> 0;
    end;
  end;

  procedure ReadDirectories;
  var
    rec: TSearchRec;
    FileAttr: Integer;
    Container: TwgFileData;
    SearchStr: string;
  begin
    SearchStr := Directory + DirSeparator + '*';
    FileAttr := faAnyFile;
    if FindFirst(SearchStr, FileAttr, rec) = 0 then
    begin
      repeat
        if (rec.Name <> '.') and (rec.Name <> '..') and ((faDirectory and
          rec.Attr) = faDirectory) then
        begin
          Container := TwgFileData.Create;
          Container.FileName := rec.Name;
          Container.FileSize := rec.Size;
{          Container.FileDate := FileDateToDateTime(rec.Time);     }
          if (faDirectory and rec.Attr) = faDirectory then
            Container.FileType := ftDirectory
          else
            Container.FileType := ftFile;
          FFiles.Add(Container);
        end;
      until FindNext(rec) <> 0;
    end;
  end;

begin
  FFiles.Clear;
  Container := TwgFileData.Create;
  Container.FileName := '..';
  Container.FileSize := 0;
  Container.FileDate := Now;
  Container.FileType := ftDirectory;
  if fgDirectoriesFirst in Options then
  begin
       if fgDirectories in Options then
          ReadDirectories;
       if fgFiles in Options then
          ReadFiles;
  end
  else
  begin
    if (fgDirectories in Options) and (fgFiles in Options) then
    begin
       ReadAll;
    end
    else
    begin
        if fgDirectories in Options then
           ReadDirectories;
        if fgFiles in Options then
           ReadFiles;
    end;
  end;
  OrderFiles;
  FFiles.Insert(0,Container);
  RecalcGrid;
end;

procedure TwgFileGrid.SetDirectory(aValue: string);
begin
  {$IFDEF DEBUG}
  Writeln('SetDirectory: ',AValue);
  {$ENDIF}
  if not DirectoryExists(AValue) then exit;
  if AValue <> '' then
  begin
    if AValue[Length(aValue)] = DirSeparator then
    begin
      Delete(AValue,Length(AValue),1);
    end;
  end;
  {$IFDEF win32}
    if AValue = '' then
    begin
      ReadDriveNames;
      FDirectory := AValue;
      if Windowed then canvas.Clear(BackgroundColor);
      RePaint;
      DoDirectoryChange;
    end
    else
  {$ENDIF}
    begin
      while pos(DirSeparator+DirSeparator,AValue) <> 0 do
        Delete(AValue,pos(DirSeparator+DirSeparator,AValue),1);	
      AValue := ExpandFileName(AValue+DirSeparator);
      while pos(DirSeparator+DirSeparator,AValue) <> 0 do
        Delete(AValue,1,1);
      if DirectoryExists(AValue) and (AValue <> FDirectory) then
      begin
        if Windowed then canvas.Clear(BackgroundColor);
        FDirectory := aValue;
        ReadDirectory;
        FocusCol := 0;
        FocusRow := 0;
        RePaint;
        DoDirectoryChange;
      end;
    end;
end;

end.

