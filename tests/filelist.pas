program filelist;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, wgcustomgrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox, wgbevel,
{$ifdef Win32}
  windows
{$else}
  libc, linux
{$endif}
  ;

{$ifndef FPC}
const
  DirectorySeparator = '\';
{$endif}

function GetGroupName(gid : integer) : string;
{$ifdef Win32}
begin
  result := IntToStr(gid);
end;
{$else}
var
  p : PGroup;
begin
  p := getgrgid(gid);
  if p <> nil then result := p^.gr_name;
end;
{$endif}

function GetUserName(uid : integer) : string;
{$ifdef Win32}
begin
  result := IntToStr(uid);
end;
{$else}
var
  p : PPasswd;
begin
  p := getpwuid(uid);
  if p <> nil then result := p^.pw_name else result := '';
end;
{$endif}

type

  TFileEntryType = (etFile,etDir);
  TFileListSortOrder = (soNone,soFileName,soCSFileName,soFileExt,soSize,soTime);

  TFileEntry = class
  public
    Name : string;
    NameExt : string;
    Size : int64;
    etype : TFileEntryType;
    islink : boolean;
{$ifdef Win32}
    attributes : longword;
{$else}
    mode : integer;
{$endif}
    modtime : TDateTime;
    ownerid : integer;
    groupid : integer;
    linktarget : string;
    
    constructor Create;
  end;
  
  TFileList = class
  private
    FEntries : TList;
    FDirectoryName : string;

    function GetEntry(i : integer): TFileEntry;
  public

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count : integer;

    property Entry[i : integer] : TFileEntry read GetEntry;
    property DirectoryName : string read FDirectoryName;

    function ReadDirectory(const fmask : string; ShowHidden : boolean) : integer;

    procedure Sort(order : TFileListSortOrder);
  end;

  TwgFileGrid = class(TwgCustomGrid)
  public
    flist : TFileList;
    
    FixedFont : TGfxFont;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function GetRowCount : integer; override;
    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;

    function CurrentEntry : TFileEntry;
  end;

  TfrmFileDialog = class(TGfxForm)
  private
    FFilterList : TStringList;
    FFilter: string;
    procedure SetFilter(const Value: string);
    function GetShowHidden: boolean;
    procedure SetShowHidden(const Value: boolean);
  public
    {@VFD_HEAD_BEGIN: frmFileDialog}
    chlDir : TwgChoiceList;
    grid : TwgFileGrid;
    btnUpDir : TwgButton;
    btnDirNew : TwgButton;
    btnShowHidden : TwgButton;
    panel1 : TwgBevel;
    lbFileInfo : TwgLabel;
    edFilename : TwgEdit;
    chlFilter : TwgChoiceList;
    lb1 : TwgLabel;
    lb2 : TwgLabel;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: frmFileDialog}

    procedure AfterCreate; override;
    destructor Destroy; override;

    procedure ListChange(Sender : TObject; row : integer);
    procedure DirChange(Sender : TObject);
    procedure FilterChange(Sender : TObject);
    procedure GridDblClick(Sender : TObject; x,y : integer; var btnstate, shiftstate : word);

    procedure UpDirClick(sender : TObject);

    procedure CancelClick(sender : TObject);

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure SetCurrentDirectory(const dir : string);

    function SelectFile(const fname : string) : boolean;

    procedure ProcessFilterString;

    function GetFileFilter : string;

  public
    FileName : string;

    property Filter : string read FFilter write SetFilter;

    function RunOpenFile : boolean;

    property ShowHidden : boolean read GetShowHidden write SetShowHidden;


  end;

{ TfrmFileDialog }

procedure TfrmFileDialog.SetFilter(const Value: string);
begin
  FFilter := Value;
  ProcessFilterString;  
end;

procedure TfrmFileDialog.UpDirClick(sender: TObject);
begin
  SetCurrentDirectory('..');
end;

function TfrmFileDialog.SelectFile(const fname: string): boolean;
var
  n : integer;
begin
  for n:=1 to grid.flist.Count do
  begin
    if grid.flist.Entry[n].Name = fname then
    begin
      //Writeln('selection: ',n);
      grid.FocusRow := n;
      result := true;
      exit;
    end;
  end;
  result := false;
end;


function TfrmFileDialog.RunOpenFile: boolean;
var
  sdir : string;
begin
  //ProcessFilterString;
  sdir := ExtractFileDir(FileName);
  if sdir = '' then sdir := '.';
  SetCurrentDirectory(sdir);
  FileName := ExtractFileName(FileName);
  if not SelectFile(FileName) then edFilename.Text8 := FileName;
  if ShowModal > 0 then
  begin
    Writeln('selected: ');
    result := true;
  end
  else result := false;
end;

procedure TfrmFileDialog.ProcessFilterString;
var
  p : integer;
  s, fs, fm : string;
begin
  s := FFilter;
  FFilterList.Clear;
  chlFilter.Items.Clear;

  repeat
    fs := ''; fm := '';
    p := pos('|',s);
    if p > 0 then
    begin
      fs := copy(s,1,p-1);
      delete(s,1,p);
      p := pos('|',s);
      if p > 0 then
      begin
        fm := copy(s,1,p-1);
        delete(s,1,p);
      end
      else
      begin
        fm := s;
        s := '';
      end;
    end;

    if (fs <> '') and (fm <> '') then
    begin
      chlFilter.Items.Add(u8(fs));
      FFilterList.Add(fm);
    end;

  until (fs = '') or (fm = '');

end;

destructor TfrmFileDialog.Destroy;
begin
  FFilterList.Free;
  inherited;
end;

procedure TfrmFileDialog.FilterChange(Sender: TObject);
begin
  SetCurrentDirectory('.');
end;

function StringMatches(const astr, apat : string) : boolean;
var
  pati, si : longint;
begin
  result := true;
  pati := 1;
  si := 1;
  while result and (si <= length(astr)) and (pati <= length(apat)) do
  begin
    if (apat[pati] = '?') or (apat[pati] = astr[si]) then
    begin
      inc(si);
      inc(pati);
    end
    else if (apat[pati] = '*') then
    begin
      while (pati <= length(apat)) and (apat[pati] in ['?','*']) do inc(pati);
      if pati > length(apat) then
      begin
        si := length(astr)+1;
        Break;   // * at the end
      end;

      while (si <= length(astr)) and (astr[si] <> apat[pati]) do inc(si);
      if si > length(astr) then result := false;
    end
    else
    begin
      result := false;
    end;
  end;

  result := result and (si > length(astr));
end;

function FileNameMatches(const astr, apats : string) : boolean;   // multiple patterns separated with ;
var
  cpat : string;
  p : integer;
  s : string;
  astrupper : string;
begin
  astrupper := UpperCase(astr);
  result := false;
  s := apats;
  repeat
    cpat := '';
    p := pos(';',s);
    if p > 0 then
    begin
      cpat := copy(s,1,p-1);
      delete(s,1,p);
    end
    else
    begin
      cpat := s;
      s := '';
    end;
    cpat := UpperCase(trim(cpat));
    if cpat <> '' then result := StringMatches(astrupper,cpat);
  until result or (cpat = '');
end;

function TfrmFileDialog.GetFileFilter: string;
var
  i : integer;
begin
  i := chlFilter.FocusItem;
  if (i > 0) and (i <= FFilterList.Count)
    then Result := FFilterList[i-1]
    else Result := '*'; 
end;

function TfrmFileDialog.GetShowHidden: boolean;
begin
  result := btnShowHidden.Down;
end;

procedure TfrmFileDialog.SetShowHidden(const Value: boolean);
begin
  btnShowHidden.Down := Value;
end;

{ TwgFileGrid }

constructor TwgFileGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  flist := TFileList.Create;

{$ifdef Win32}
  AddColumn8('Name',320);
{$else}
  AddColumn8('Name',220);
{$endif}

  AddColumn8('Size',80);
  AddColumn8('Mod. Time',108);
{$ifdef Win32}
  AddColumn8('Attributes',78);
{$else}
  AddColumn8('Rights',78);
  AddColumn8('Owner',54);
  AddColumn8('Group',54);
{$endif}
  RowSelect := true;
  
  FixedFont := GfxGetFont('Courier New-9:antialias=false');
end;

destructor TwgFileGrid.Destroy;
begin
  FixedFont.Free;
  flist.Free;
  inherited Destroy;
end;

function TwgFileGrid.GetRowCount: integer;
begin
  Result:= flist.Count;
end;

procedure TwgFileGrid.DrawCell(row, col: integer; rect: TGfxRect; flags: integer);
const
  modestring : string[9] = 'xwrxwrxwr';
var
  e : TFileEntry;
  x,y : integer;
  s : string;
  img : TGfxImage;
{$ifndef Win32}
  b,n : integer;
{$endif}  
begin
  e := flist.Entry[row];
  if e=nil then Exit;
  x := rect.left+2;
  y := rect.top+1;
  s := '';
  
  if e.etype = etDir then canvas.SetFont(FHeaderFont) else canvas.SetFont(FFont);

  case col of
  1: begin
       case e.etype of
        etDir: img := GfxLibGetImage('stdimg.folder');
       else
         img := GfxLibGetImage('stdimg.document');
{$ifndef Win32}
         if (e.mode and $40) <> 0 then img := GfxLibGetImage('stdimg.yes');
{$endif}         
       end;
                          
       if img <> nil then canvas.DrawImage(rect.Left+1,y,img);
       if e.islink then canvas.DrawImage(rect.Left+1,y,GfxLibGetImage('stdimg.link'));
       x := rect.left + 20;
       s := u8(e.Name);
     end;
  2: begin
       s := u8(FormatFloat('###,###,###,##0',e.size));
       x := rect.right - Font.TextWidth16(s) - 1;
       if x < rect.Left+2 then x := rect.Left+2;
     end;
  3: s := u8(FormatDateTime('yyyy-mm-dd hh:nn',e.modtime));
//  4: s := u8(e.linktarget);
  4: begin
{$ifdef Win32}
       // File attributes
       s := '';
       //if (e.attributes and FILE_ATTRIBUTE_ARCHIVE) <> 0    then s := s + 'a' else s := s + ' ';
       if (e.attributes and FILE_ATTRIBUTE_HIDDEN) <> 0     then s := s + 'h';
       if (e.attributes and FILE_ATTRIBUTE_READONLY) <> 0   then s := s + 'r';
       if (e.attributes and FILE_ATTRIBUTE_SYSTEM) <> 0     then s := s + 's';
       if (e.attributes and FILE_ATTRIBUTE_TEMPORARY) <> 0  then s := s + 't';
       if (e.attributes and FILE_ATTRIBUTE_COMPRESSED) <> 0 then s := s + 'c';
{$else}
       // rights
       //rwx rwx rwx
       b := 1;
       n := 1;
       s := '';
       while n <= 9 do
       begin
         if (e.mode and b) = 0 then s := '-'+s
         else s := modestring[n]+s;
         inc(n);
         b := b shl 1;
       end;
{$endif}

       canvas.SetFont(FixedFont);
       s := u8(s);
     end;
{$ifdef Win32}
{$else}
  5: s := u8(GetUserName(e.ownerid));  // use getpwuid(); for the name of this user
  6: s := u8(GetGroupName(e.groupid));  // use getgrgid(); for the name of this group
{$endif}  
  end;
  canvas.DrawString16(x,y,s);

end;

function TwgFileGrid.CurrentEntry: TFileEntry;
begin
  result := flist.Entry[FocusRow];
end;

{ TFileEntry }

constructor TFileEntry.Create;
begin
  Name := '';
  NameExt := '';
  Size := 0;
  etype := etFile;
  islink := false;
{$ifdef Win32}
  attributes := 0;
{$else}
  mode := 0;
{$endif}  
  modtime := 0;
  ownerid := 0;
  groupid := 0;
  linktarget := '';
end;

{ TFileList }

function TFileList.GetEntry(i : integer): TFileEntry;
begin
  if (i < 1) or (i > FEntries.Count) then result := nil
  else result := TFileEntry(FEntries[i-1]);
end;

constructor TFileList.Create;
begin
  FEntries := TList.Create;
  FDirectoryName := '';
end;

destructor TFileList.Destroy;
begin
  clear;
  FEntries.Free;
  inherited;
end;

procedure TFileList.Clear;
var
  n : integer;
begin
  for n:=0 to FEntries.Count-1 do TObject(FEntries[n]).Free;
  FEntries.Clear;
end;

function TFileList.Count: integer;
begin
  result := FEntries.Count;
end;

{$ifdef Win32}

function TFileList.ReadDirectory(const fmask : string; ShowHidden : boolean): integer;
var
  hff : THANDLE;
  fdata : WIN32_FIND_DATA;
  e : TFileEntry;
  ftime : SYSTEMTIME;
begin
  Clear;

  GetDir(0,FDirectoryName);
  if (FDirectoryName <> '') and (copy(FDirectoryName,Length(FDirectoryName),1) <> DirectorySeparator)
    then FDirectoryName := FDirectoryName + DirectorySeparator;

  hff := FindFirstFile('*', fdata);

  if hff <> INVALID_HANDLE_VALUE then
  begin
    repeat
      e := TFileEntry.Create;
      e.Name := PChar(@fdata.cFileName);
      //Write(e.Name);
      e.NameExt := ExtractFileExt(e.Name);
      
      //fullname := FDirectoryName + e.Name;

      e.islink := false;  // Windows does not support ?

      e.attributes := fdata.dwFileAttributes;
      e.size := fdata.nFileSizeHigh shl 32 + fdata.nFileSizeLow;
      e.ownerid := 0;
      e.groupid := 0;

      FileTimeToSystemTime(fdata.ftLastWriteTime, ftime);
      e.modtime := EncodeDate(ftime.wYear,ftime.wMonth,ftime.wDay)
        + EncodeTime(ftime.wHour, ftime.wMinute, ftime.wSecond, ftime.wMilliseconds);

      if (e.attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY then e.etype := etDir
      else e.etype := etFile;

      //write('  (',e.linktarget,')');

      if (e.name = '.') or
         (not ShowHidden and ((e.attributes and FILE_ATTRIBUTE_HIDDEN) <> 0)) or
         ((e.etype = etFile) and not FileNameMatches(e.Name,fmask))  then
      begin
        // do not add this entry
        e.Free;
      end
      else
        FEntries.Add(e)

    until not FindNextFile(hff, fdata);
  end;

  result := FEntries.Count;
end;

{$else}
function EpochToDateTime(epoch : longint) : TDateTime;
var
  w1,w2,w3,w4,w5,w6 : word;
begin
  EpochToLocal(epoch,w1,w2,w3,w4,w5,w6);
  result := EncodeDate(w1,w2,w3)+EncodeTime(w4,w5,w6,0);
end;

function TFileList.ReadDirectory(const fmask : string; ShowHidden : boolean): integer;
Var
  gres,p : PGlob;
  e : TFileEntry;
  fullname : string;
  info : stat;
  //dname : string;
begin
  Clear;
  GetDir(0,FDirectoryName);
  //Writeln('dname: ', dname);
  if copy(FDirectoryName,Length(FDirectoryName),1) <> '/' then FDirectoryName := FDirectoryName+'/';

  gres := glob('*');
  p := gres;
  while p <> nil do
  begin
    e := TFileEntry.Create;
    e.Name := p^.name;
    //Write(e.Name);
    e.NameExt := ExtractFileExt(e.Name);
    fullname := FDirectoryName + e.Name;

    //Writeln('fullname: ',fullname);
    if lstat(fullname,info) then
    begin
      e.islink := ((info.mode and $F000) = $A000);
      if e.islink then
      begin
        e.linktarget := ReadLink(fullname);
        fstat(fullname,info);
      end;

      e.mode := info.mode;
      e.size := info.size;
      e.ownerid := info.uid;
      e.groupid := info.gid;
      e.modtime   := EpochToDateTime(info.mtime);

      if (e.mode and $F000) = $4000 then e.etype := etDir
      else e.etype := etFile;

      //write('  (',e.linktarget,')');

      if (e.name = '.') or
         ((e.name = '..') and (FDirectoryName = '/')) or
         (not ShowHidden and (copy(e.name,1,1)='.') and (copy(e.name,2,1)<>'.') ) or
         ((e.etype = etFile) and not FileNameMatches(e.Name,fmask))  then
      begin
        // do not add this entry
        e.Free;
      end
      else
        FEntries.Add(e)

    end;
    //writeln;
    p := p^.next;
  end;

  if gres <> nil then GlobFree(gres);

  result := FEntries.Count;
end;

{$endif}

procedure TFileList.Sort(order: TFileListSortOrder);
var
  newl : TList;
  n,i : integer;
  e : TFileEntry;

  function IsBefore(newitem, item : TFileEntry) : boolean;
  begin
    //if newitem.etype = etDir then writeln('dir: ',newitem.name,' (',item.name,')');
    if (newitem.etype = etDir) and (item.etype <> etDir) then
    begin
      result := true;
    end
    else if (newitem.etype <> etDir) and (item.etype = etDir) then
    begin
      result := false;
    end
    else if (newitem.etype = etDir) and (newitem.Name = '..') then
    begin
      result := true;
    end
    else if (item.etype = etDir) and (item.Name = '..') then
    begin
      result := false;
    end
    else
      case order of
      soFileName   : result := UpperCase(newitem.Name) < UpperCase(item.Name);
      soCSFileName : result := newitem.Name < item.Name;
      soFileExt    : result := UpperCase(newitem.NameExt+' '+newitem.Name) < UpperCase(item.NameExt+' '+item.Name);
      soSize       : result := newitem.size < item.size;
      soTime       : result := newitem.modtime < item.modtime;
      else
        result := false;
      end;
  end;
  
begin
  newl := TList.Create;
  for n := 0 to FEntries.Count-1 do
  begin
    e := TFileEntry(FEntries[n]);
    i := 0;
    while (i < newl.Count) and not IsBefore(e,TFileEntry(newl[i])) do inc(i);
    newl.Insert(i,e);
  end;
  FEntries.Free;
  FEntries := newl;
end;

procedure TestFileList;
var
  fl : TFileList;
  n : integer;
begin
  fl := TFileList.Create;
  Writeln('entry count: ',fl.ReadDirectory('*',true));
  fl.Sort(soFileExt);
  for n := 1 to fl.Count do
  begin
    Writeln(n,'.: ',fl.Entry[n].Name,' time: ',DateTimeToStr(fl.Entry[n].modtime));
  end;
  
  write('test finished...'); readln;
end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmFileDialog.AfterCreate;
begin
  FFilterList := TStringList.Create;

  {@VFD_BODY_BEGIN: frmFileDialog}
  SetDimensions(303,171,640,460);
  WindowTitle8 := 'frmFileDialog';

  chlDir := TwgChoiceList.Create(self);
  with chlDir do
  begin
    SetDimensions(8,12,526,22);
    Anchors := [anLeft,anRight,anTop];
    FontName := '#List';
    OnChange := DirChange;
  end;

  grid := TwgFileGrid.Create(self);
  with grid do
  begin
    SetDimensions(8,44,622,252);
    Anchors := [anLeft,anRight,anTop,anBottom];
    OnRowChange := ListChange;
    OnDoubleClick := GridDblClick;
  end;

  btnUpDir := TwgButton.Create(self);
  with btnUpDir do
  begin
    SetDimensions(540,11,26,24);
    Anchors := [anRight,anTop];
    Text := u8('');
    FontName := '#Label1';
    ImageName := 'stdimg.folderup';
    ModalResult := 0;
    Focusable := false;
    OnClick := UpDirClick;
  end;

  btnDirNew := TwgButton.Create(self);
  with btnDirNew do
  begin
    SetDimensions(572,11,26,24);
    Anchors := [anRight,anTop];
    Text := u8('');
    FontName := '#Label1';
    ImageName := 'stdimg.foldernew';
    ModalResult := 0;
    Focusable := false;
  end;

  btnShowHidden := TwgButton.Create(self);
  with btnShowHidden do
  begin
    SetDimensions(604,11,26,24);
    Text := u8('');
    FontName := '#Label1';
    ImageName := 'stdimg.hidden';
    ModalResult := 0;
    Focusable := false;
    GroupIndex := 1;
    AllowAllUp := true;
    OnClick := DirChange;
  end;

  panel1 := TwgBevel.Create(self);
  with panel1 do
  begin
    SetDimensions(8,305,622,25);
    Anchors := [anLeft,anRight,anBottom];
    shape := bsBox;
    style := bsLowered;
  end;

  lbFileInfo := TwgLabel.Create(panel1);
  with lbFileInfo do
  begin
    SetDimensions(5,4,609,16);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('Label');
    FontName := '#Label1';
  end;

  edFilename := TwgEdit.Create(self);
  with edFilename do
  begin
    SetDimensions(8,353,622,22);
    Anchors := [anLeft,anRight,anBottom];
    Text := u8('');
    FontName := '#Edit1';
  end;

  chlFilter := TwgChoiceList.Create(self);
  with chlFilter do
  begin
    SetDimensions(8,397,622,22);
    Anchors := [anLeft,anRight,anBottom];
    FontName := '#List';
    OnChange := FilterChange;
  end;

  lb1 := TwgLabel.Create(self);
  with lb1 do
  begin
    SetDimensions(8,335,80,16);
    Anchors := [anLeft,anBottom];
    Text := u8('Filename:');
    FontName := '#Label1';
  end;

  lb2 := TwgLabel.Create(self);
  with lb2 do
  begin
    SetDimensions(8,379,64,16);
    Anchors := [anLeft,anBottom];
    Text := u8('File type:');
    FontName := '#Label1';
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,427,96,24);
    Anchors := [anLeft,anBottom];
    Text := u8('OK');
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 1;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(534,427,96,24);
    Anchors := [anRight,anBottom];
    Text := u8('Cancel');
    FontName := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := -1;
    OnClick := CancelClick;
  end;

  {@VFD_BODY_END: frmFileDialog}

  FileName := '';
  Filter := 'All Files (*)|*';

//  SetCurrentDirectory('.');

//  grid.flist.ReadDirectory('*');
//  grid.flist.Sort(soFileName);
//  grid.Update;
end;

procedure TfrmFileDialog.ListChange(sender: TObject; row : integer);
var
  s : string;         
begin
  if grid.currententry = nil then Exit;
  s := grid.currententry.Name;
  
  if grid.currententry.islink then s := s + ' -> '+grid.currententry.linktarget;

  if grid.currententry.etype <> etDir
    then edFileName.Text8 := grid.currententry.Name;
    
//    then edFileName.Text := ''

  lbFileInfo.Text8 := s;
end;

procedure TfrmFileDialog.DirChange(Sender: TObject);
begin
  SetCurrentDirectory(chlDir.Text8);
end;

procedure TfrmFileDialog.GridDblClick(Sender: TObject; x, y: integer; var btnstate, shiftstate: word);
var
  e : TFileEntry;
begin
  e := grid.CurrentEntry;
  if (e <> nil) and (e.etype = etDir) then
  begin
    SetCurrentDirectory(e.Name);
  end;
end;

procedure TfrmFileDialog.CancelClick(sender: TObject);
begin
  Close;
end;

procedure TfrmFileDialog.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  e : TFileEntry;
begin
  if not consumed then
  begin
    if (keycode = KEY_ENTER) and (ActiveWidget = grid) then
    begin
      e := grid.CurrentEntry;
      if (e <> nil) and (e.etype = etDir) then
      begin
        SetCurrentDirectory(e.Name);
        consumed := true;
      end;
    end;
  end;
  if not consumed then inherited;
end;

procedure TfrmFileDialog.SetCurrentDirectory(const dir: string);
var
  ds : string;
  n  : integer;
  rootadd : integer;
  fsel : string;
{$ifdef Win32}
  drvind : integer;
  drvs : string;
{$endif}
begin
  GetDir(0,ds);
  fsel := ExtractFileName(ds);

  if not SetCurrentDir(dir) then
  begin
    ShowMessage8('Could not open the directory '+dir,'Error');
    Exit;
  end;

  chlDir.Items.Clear;

  if dir <> '..' then fsel := '';
  //Writeln('fsel=',fsel);

  GetDir(0,ds);

  rootadd := 1;

{$ifdef Win32}
  // making drive list 1
  drvind := -1;
  if copy(ds,2,1) = ':' then drvind := ord(UpCase(ds[1]))-ord('A');

  n := 0;
  while n < drvind do
  begin
    drvs := chr(n+ord('A'))+':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
    begin
      chlDir.Items.Add(u8(drvs));
    end;
    inc(n);
  end;

{$else}
  if copy(ds,1,1) <> DirectorySeparator then ds := DirectorySeparator + ds;
{$endif}

  n := 1;
  while n < length(ds) do
  begin
    if ds[n] = DirectorySeparator then
    begin
      chlDir.Items.Add(u8(copy(ds,1,n-1+rootadd)));
      rootadd := 0;
    end;

    inc(n);
  end;

  chlDir.Items.Add(u8(ds));
  chlDir.FocusItem := chlDir.Items.Count;

{$ifdef Win32}
  // making drive list 2
  n := drvind+1;
  if n < 0 then n := 0;
  while n <= 25 do
  begin
    drvs := chr(n+ord('A'))+':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
    begin
      chlDir.Items.Add(u8(drvs));
    end;
    inc(n);
  end;
{$endif}

  grid.flist.ReadDirectory(GetFileFilter(),ShowHidden);

  grid.flist.Sort(soFileName);

  grid.Update;

  if fsel <> '' then SelectFile(fsel)
                else grid.FocusRow := 1;

end;

var
  frm : TfrmFileDialog;
begin
//  TestFileList;
//  Exit;

//  if StringMatches('asztakutyafajat','?szta*j?') then Writeln('matches') else Writeln('no match');
//  readln;
//  exit;

  GfxOpenDisplay('');
  frm := TfrmFileDialog.Create(nil);
  frm.FileName := 'caltest.pas';
  frm.Filter := 'Pascal files (*.pas)|*.pas;*.inc|Include Files (*.inc)|*.inc|All Files (*)|*';
  frm.RunOpenFile;
  GfxDoMessageLoop;
  GfxCloseDisplay;
end.
