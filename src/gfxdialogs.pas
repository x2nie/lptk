{ gfxdialogs.pas: message box functionality, here will be the standard dialogs later
  File maintainer: nvitya@freemail.hu

History:
}

unit gfxdialogs;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, GfxBase, GfxForm, schar16, GfxStyle,
  gfxwidget, wgButton, wgLabel, wgListBox, wgCheckBox, wgEdit,
  wgCustomGrid, wgChoiceList, wgBevel;

{$ifndef FPC}
const
  DirectorySeparator = '\';
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
    FOpenMode : boolean;
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
    procedure OKClick(sender : TObject);

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure SetCurrentDirectory(const dir : string);

    function SelectFile(const fname : string) : boolean;

    procedure ProcessFilterString;

    function GetFileFilter : string;

  public

    FileName : string;
    property Filter : string read FFilter write SetFilter;
    function RunOpenFile : boolean;
    function RunSaveFile : boolean;
    property ShowHidden : boolean read GetShowHidden write SetShowHidden;
    
  end;

  TMessageBox = class(TGfxForm)
  private
    FLines : TStringList;
    FFont : TGfxFont;
    FTextY : integer;
    FLineHeight : integer;
    FMaxLineWidth : integer;
    FMinWidth : integer;

  protected

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  public
    btn : TwgButton;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SetMessage8(txt : string);

    procedure btnClick(sender : TObject);

    procedure RePaint; override;

  end;

  TfrmFontSelect = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmFontSelect}
    lbLabel1 : TwgLabel;
    lstFaces : TwgTextListBox;
    lstSize : TwgTextListBox;
    lbLabel2 : TwgLabel;
    cbBold : TwgCheckBox;
    cbItalic : TwgCheckBox;
    cbAntiAlias : TwgCheckBox;
    lbLabel3 : TwgLabel;
    lbLabel4 : TwgLabel;
    edSample : TwgEdit;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: frmFontSelect}
    
    procedure AfterCreate; override;

    procedure OkClick(sender : TObject);
    procedure CancelClick(sender : TObject);

    procedure OnParamChange(sender : TObject);

    procedure CreateFontList;

    function GetFontDesc : string;
    procedure SetFontDesc(desc : string);
  end;
  
  TwgShowColor = class(TWidget)
  public
    Color : TGfxColor;

    OnClick : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;

    procedure RePaint; override;
    procedure HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word); override;

  end;

  TwgPredefColors = class(TWidget)
  protected
    Flastx, FLasty : integer;

    FBoxWidth : integer;
    FBoxHeight : integer;
    FGap : integer;

    FColors : TList;

  public
    OnColorClick : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddColor(col : TGfxColor);

  end;

  TfrmSelectColor = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmSelectColor}
    lbLabel1 : TWGLABEL;
    lbLabel2 : TWGLABEL;
    lbLabel3 : TWGLABEL;
    edRComp : TWGEDIT;
    edGComp : TWGEDIT;
    edBComp : TWGEDIT;
    lbLabel4 : TWGLABEL;
    edHex : TWGEDIT;
    lbLabel5 : TWGLABEL;
    ShowColor : TwgShowColor;
    PredefColors : TwgPredefColors;
    btnOK : TWGBUTTON;
    btnCancel : TWGBUTTON;
    {@VFD_HEAD_END: frmSelectColor}

    procedure AfterCreate; override;

    procedure ColorClick(sender : TObject);

    procedure SetCurColor(col : TGfxColor);

    procedure RGBChange(sender : TObject);
    procedure HexChange(sender : TObject);

    procedure ButtonClick(sender : TObject);

  end;


{@VFD_NEWFORM_DECL}

procedure ShowMessage8(msg, title : string); overload;
procedure ShowMessage8(msg : string); overload;

function SelectFontDialog(var fontdesc : string) : boolean;
function SelectColorDialog(var color : TGfxColor) : boolean;

function StrToHex(hnum : string) : longword;

implementation

uses
{$ifdef Win32}
  windows
{$else}
  unitlibc,
  unixutil
{$endif}
  ;

function StrToHex(hnum : string) : longword;
var
  n : integer;
begin
  result := 0;
  for n:=1 to length(hnum) do
  begin
    result := (result shl 4) + longword(pos(upcase(hnum[n]),'123456789ABCDEF'));
  end;
end;

{$ifdef Win32}
{$else}
function GetGroupName(gid : integer) : string;
begin
  result := 'GroupName!';
end;

function GetUserName(uid : integer) : string;
begin
  result := 'UserName!';
end;

(*
function GetGroupName(gid : integer) : string;
var
  p : PGroup;
begin
  p := getgrgid(gid);
  if p <> nil then result := p^.gr_name;
end;

function GetUserName(uid : integer) : string;
var
  p : PPasswd;
begin
  p := getpwuid(uid);
  if p <> nil then result := p^.pw_name else result := '';
end;
*)
{$endif}


function SelectColorDialog(var color : TGfxColor) : boolean;
var
  frm : TfrmSelectColor;
begin
  result := false;
  frm := TfrmSelectColor.Create(nil);
  frm.SetCurColor(color);
  if frm.ShowModal > 0 then
  begin
    color := frm.ShowColor.Color;
    result := true;
  end;
  frm.Free;
end;

function SelectFontDialog(var fontdesc : string) : boolean;
var
  frm : TfrmFontSelect;
begin
  result := false;
  frm := TfrmFontSelect.Create(nil);
  frm.SetFontDesc(fontdesc);
  if frm.ShowModal > 0 then
  begin
    fontdesc := frm.GetFontDesc;
    result := true;
  end;
  frm.Free;
end;

procedure ShowMessage8(msg, title : string); overload;
var
  mf : TMessageBox;
begin
  mf := TMessageBox.Create(nil);
  mf.WindowTitle8 := title;
  mf.SetMessage8(msg);
  mf.ShowModal;
  mf.Free;
end;

procedure ShowMessage8(msg : string); overload;
begin
  ShowMessage8(msg,'Message');
end;


{@VFD_NEWFORM_IMPL}


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
  fname : string;
begin
  FOpenMode := true;
  sdir := ExtractFileDir(FileName);
  if sdir = '' then sdir := '.';
  SetCurrentDirectory(sdir);
  fname := ExtractFileName(FileName);
  if not SelectFile(fname) then edFilename.Text8 := fname;

  if ShowModal > 0
    then result := true
    else result := false;
end;

function TfrmFileDialog.RunSaveFile: boolean;
var
  sdir : string;
  fname : string;
begin
  FOpenMode := false;
  sdir := ExtractFileDir(FileName);
  if sdir = '' then sdir := '.';
  SetCurrentDirectory(sdir);
  fname := ExtractFileName(FileName);
  if not SelectFile(fname) then edFilename.Text8 := fname;
  if ShowModal > 0
    then result := true
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

  if (e.etype = etDir) and (col=1)
    then canvas.SetFont(FHeaderFont)
    else canvas.SetFont(FFont);

  case col of
  1: begin
       if e.etype = etDir then img := GfxLibGetImage('stdimg.folder')
       else
       begin
         img := GfxLibGetImage('stdimg.document');
{$ifndef Win32}
         if (e.mode and $40) <> 0 then img := GfxLibGetImage('stdimg.yes'); // executable
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
  gres : glob_t;
  n,r : integer;
  p : PPChar;
  e : TFileEntry;
  fullname : string;
  info : _stat;
  //dname : string;
begin
  Clear;
  GetDir(0,FDirectoryName);
  //Writeln('dname: ', dname);
  if copy(FDirectoryName,Length(FDirectoryName),1) <> '/' then FDirectoryName := FDirectoryName+'/';

  FillChar(gres,sizeof(gres),0);
  glob('*',GLOB_PERIOD,nil,@gres);
  n := 0;
  p := gres.gl_pathv;
  while n < gres.gl_pathc do
  begin
    e := TFileEntry.Create;
    e.Name := p^;
    //Write(e.Name);
    e.NameExt := ExtractFileExt(e.Name);
    fullname := FDirectoryName + e.Name;

    //Writeln('fullname: ',fullname);
    if lstat(PChar(fullname),info) = 0 then
    begin
      e.islink := ((info.st_mode and $F000) = $A000);
      if e.islink then
      begin
        SetLength(e.linktarget,256);
        r := readlink(PChar(fullname),@(e.linktarget[1]),sizeof(e.linktarget));
        if r > 0 then SetLength(e.linktarget,r) else e.linktarget := '';
        stat(PChar(fullname),info);
      end;

      e.mode := info.st_mode;
      e.size := info.st_size;
      e.ownerid := info.st_uid;
      e.groupid := info.st_gid;
      e.modtime   := EpochToDateTime(info.st_mtime);

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
    inc(p);
    inc(n);
  end;

  if gres.gl_pathc > 0 then GlobFree(@gres);

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

procedure TfrmFileDialog.AfterCreate;
begin
  FFilterList := TStringList.Create;

  {@VFD_BODY_BEGIN: frmFileDialog}
  SetDimensions(303,171,640,460);
  WindowTitle8 := 'File selection';

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
    OnClick := OKClick;
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

  ActiveWidget := edFileName;

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

procedure TfrmFileDialog.OKClick(sender: TObject);
begin
  if not FOpenMode or sysutils.FileExists(edFileName.Text8) then
  begin
    ModalResult := 1
  end;
  
  if ModalResult > 0 then
    FileName := ExpandFileName(edFileName.Text8);
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

{ TMessageBox }

constructor TMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TStringList.Create;
  FFont := GfxGetFont('#Label1');
  FTextY := 10;
  FLineHeight := FFont.Height + 4;
  FMinWidth := 200;
  FMaxLineWidth := 500;
  btn := TwgButton.Create(self);
  btn.Text := str8to16('OK');
  btn.Width := 80;
  btn.OnClick := btnClick;
  Resizeable := false;
end;

destructor TMessageBox.Destroy;
begin
  FFont.Free;
  FLines.Free;
  inherited;
end;

procedure TMessageBox.SetMessage8(txt: string);
var
  maxw : integer;
  n : integer;
  s,s16 : string16;
  c : char;

  procedure AddLine(all : boolean);
  var
    w : integer;
    m : integer;
  begin
    s16 := Str8to16(s);
    w := FFont.TextWidth16(s16);
    if w > FMaxLineWidth then
    begin
      while w > FMaxLineWidth do
      begin
        m := Length(s);
        repeat
          dec(m);
          s16 := str8to16(copy(s,1,m));
          w := FFont.TextWidth16(s16);
        until w <= FMaxLineWidth;
        if w > maxw then maxw := w;
        FLines.Add(s16);
        s := copy(s,m+1,length(s));
        s16 := Str8to16(s);
        w := FFont.TextWidth16(s16);
      end;
      if all then
      begin
        FLines.Add(s16);
        s := '';
      end;
    end
    else
    begin
      FLines.Add(s16);
      s := '';
    end;

    if w > maxw then maxw := w;
  end;

begin
  s := '';
  FLines.Clear;
  n := 1;
  maxw := 0;
  while n <= length(txt) do
  begin
    c := txt[n];
    if (c = #13) or (c = #10) then
    begin
      AddLine(false);
      if (c = #13) and (n < length(txt)) and (txt[n+1] = #10) then inc(n);
    end
    else s := s + c;
    inc(n);
  end;
  AddLine(true);

  width := maxw + 2*10;

  if width < FMinWidth then width := FMinWidth;

  btn.Top := FTextY + FLineHeight*FLines.Count + FTextY;

  btn.Left := (Width div 2) - (btn.Width div 2);

  height := btn.Top + btn.Height + FTextY;
end;

procedure TMessageBox.btnClick(sender: TObject);
begin
  ModalResult := 1;
end;

procedure TMessageBox.RePaint;
var
  n, y : integer;
  tw : integer;
begin
//  inherited RePaint;
  canvas.Clear(FBackgroundColor);
  canvas.SetFont(FFont);

  y := FTextY;
  for n:=0 to FLines.Count-1 do
  begin
    tw := FFont.TextWidth16(FLines[n]);
    canvas.DrawString16(width div 2 - tw div 2, y, FLines[n]);
    inc(y, FLineHeight);
  end;
end;

procedure TMessageBox.HandleKeyPress(var keycode, shiftstate: word; var consumed: boolean);
begin
  inherited;
  if keycode = KEY_ESC then
  begin
    Close;
  end;
end;

{ TfrmFontSelect }

procedure TfrmFontSelect.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmFontSelect}
  SetDimensions(300,100,428,380);
  WindowTitle8 := 'Font Selection';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,8,73,16);
    Text := u8('Font face:');
  end;

  lstFaces := TwgTextListBox.Create(self);
  with lstFaces do
  begin
    SetDimensions(8,28,232,236);
    Items.Add(u8('lstFace'));
    OnChange := OnParamChange;
  end;

  lstSize := TwgTextListBox.Create(self);
  with lstSize do
  begin
    SetDimensions(248,28,52,236);
    Items.Add(u8('6'));
    Items.Add(u8('7'));
    Items.Add(u8('8'));
    Items.Add(u8('9'));
    Items.Add(u8('10'));
    Items.Add(u8('11'));
    Items.Add(u8('12'));
    Items.Add(u8('13'));
    Items.Add(u8('14'));
    Items.Add(u8('15'));
    Items.Add(u8('16'));
    Items.Add(u8('18'));
    Items.Add(u8('20'));
    Items.Add(u8('24'));
    Items.Add(u8('28'));
    Items.Add(u8('32'));
    Items.Add(u8('48'));
    Items.Add(u8('64'));
    Items.Add(u8('72'));
    OnChange := OnParamChange;
    FocusItem := 5;
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(308,8,54,16);
    Text := u8('Type:');
  end;

  cbBold := TwgCheckBox.Create(self);
  with cbBold do
  begin
    SetDimensions(308,32,87,20);
    Text := u8('Bold');
    OnChange := OnParamChange;
  end;

  cbItalic := TwgCheckBox.Create(self);
  with cbItalic do
  begin
    SetDimensions(308,56,87,20);
    Text := u8('Italic');
    OnChange := OnParamChange;
  end;

  cbAntiAlias := TwgCheckBox.Create(self);
  with cbAntiAlias do
  begin
    SetDimensions(308,124,99,20);
    Text := u8('Anti aliasing');
    OnChange := OnParamChange;
    Checked := true;
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(248,8,54,16);
    Text := u8('Size:');
  end;

  lbLabel4 := TwgLabel.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(8,268,55,16);
    Text := u8('Sample:');
  end;

  edSample := TwgEdit.Create(self);
  with edSample do
  begin
    SetDimensions(8,288,414,52);
    Text := u8('ABC abc gyjwstIl 123 ^337^369^336^368');
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,348,105,24);
    Text := u8('OK');
    ImageName := 'stdimg.ok';
    OnClick := OkClick;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(316,348,105,24);
    Text := u8('Cancel');
    ImageName := 'stdimg.cancel';
    OnClick := CancelClick;
  end;

  {@VFD_BODY_END: frmFontSelect}

  CreateFontList;
end;

procedure TfrmFontSelect.OkClick(sender: TObject);
begin
  ModalResult := 1;
  Close;
end;

procedure TfrmFontSelect.CancelClick(sender: TObject);
begin
  ModalResult := -1;
  Close;
end;

procedure TfrmFontSelect.OnParamChange(sender: TObject);
var
  fdesc : string;
begin
  fdesc := GetFontDesc;
  Writeln(fdesc);
  edSample.FontName := fdesc;
end;

procedure TfrmFontSelect.CreateFontList;
var
  fl : TStringList;
  n : integer;
begin
  lstFaces.Items.Clear;

  fl := GfxGetFontFaceList;

  for n:=0 to fl.Count-1 do lstFaces.Items.Add(u8(fl.Strings[n]));

  fl.Free;
end;

function TfrmFontSelect.GetFontDesc: string;
var
  s : string;
begin
  s := str16to8(lstFaces.Text)+'-'+str16to8(lstSize.Text);
  if cbBold.Checked then s := s+':bold';
  if cbItalic.Checked then s := s+':italic';

  s := s + ':';
  if cbAntiAlias.Checked then s := s+'antialias=true' else s := s + 'antialias=false';

  result := s;
end;

procedure TfrmFontSelect.SetFontDesc(desc: string);
var
  cp : integer;
  c : char;
  i : integer;

  token : string;
  prop, propval : string;

  function NextC : char;
  begin
    inc(cp);
    if cp > length(desc) then c := #0
                         else c := desc[cp];
    result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ','a'..'z','A'..'Z','_','0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
//  Writeln('GfxGetFont(''',desc,''')');

  cp := 1;
  c := desc[1];

  cbBold.Checked := false;
  cbItalic.Checked := false;

  cbAntiAlias.Checked := true;

  NextToken;
  i := lstFaces.Items.IndexOf(u8(token));
  if i >= 0 then lstFaces.FocusItem := i+1;
  if c = '-' then
  begin
    NextC;
    NextToken;
    i := lstSize.Items.IndexOf(u8(token));
    if i >= 0 then lstSize.FocusItem := i+1;
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    if prop = 'BOLD' then
    begin
      cbBold.Checked := true;
    end
    else if prop = 'ITALIC' then
    begin
      cbItalic.Checked := true;
    end
    else if prop = 'ANTIALIAS' then
    begin
      if propval = 'FALSE' then cbAntialias.Checked := false;
    end
    ;

  end;

  OnParamChange(self);
end;

{ Color selectors }

{ TwgPredefColors }

constructor TwgPredefColors.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLastx := 0; Flasty := 0;
  FBoxWidth := 32;
  FBoxHeight := 16;
  FGap := 4;

  OnColorClick := nil;

  FColors := TList.Create;
end;

destructor TwgPredefColors.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

procedure TwgPredefColors.AddColor(col: TGfxColor);
var
  sc : TwgShowColor;
begin
  FColors.Add(Pointer(col));

  sc := TwgShowColor.Create(self);
  sc.Color := col;
  sc.Left := FLastx;
  sc.Top := FLasty;
  sc.width := FBoxWidth;
  sc.height := FBoxHeight;
  sc.OnClick := OnColorClick;

  Inc(Flastx,FBoxWidth+FGap);
  if Flastx+FBoxWidth > Width then
  begin
    inc(Flasty,FBoxHeight+FGap);
    FLastx := 0;
  end;
end;

{ TwgShowColor }

constructor TwgShowColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  color := $AA2233;
  OnClick := nil;
end;

procedure TwgShowColor.RePaint;
begin
  canvas.SetColor(color);
  canvas.FillRectangle(1,1,Width-2,Height-2);
  canvas.SetColor(0);
  canvas.DrawRectangle(0,0,Width,Height);
end;

procedure TwgShowColor.HandleMouseDown(X, Y: Integer; Button: Word;
  ShiftState: Word);
begin
  inherited HandleMouseDown(X, Y, Button, ShiftState);
  if Assigned(OnClick) then OnClick(self);
end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmSelectColor.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmSelectColor}
  SetDimensions(286,126,328,262);
  WindowTitle8 := 'Select Color';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,10,22,16);
    Text := u8('R:');
  end;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(8,34,22,16);
    Text := u8('G:');
  end;

  lbLabel3 := TWGLABEL.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(8,58,22,16);
    Text := u8('B:');
  end;

  edRComp := TWGEDIT.Create(self);
  with edRComp do
  begin
    SetDimensions(24,8,62,22);
    Text := u8('255');
    OnChange := RGBChange;
  end;

  edGComp := TWGEDIT.Create(self);
  with edGComp do
  begin
    SetDimensions(24,32,62,22);
    Text := u8('64');
    OnChange := RGBChange;
  end;

  edBComp := TWGEDIT.Create(self);
  with edBComp do
  begin
    SetDimensions(24,56,62,22);
    Text := u8('45');
    OnChange := RGBChange;
  end;

  lbLabel4 := TWGLABEL.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(8,84,54,16);
    Text := u8('Hex:');
  end;

  edHex := TWGEDIT.Create(self);
  with edHex do
  begin
    SetDimensions(8,104,78,22);
    Text := u8('edHex');
    OnChange := HexChange;
  end;

  lbLabel5 := TWGLABEL.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(8,136,57,16);
    Text := u8('Preview:');
  end;

  ShowColor := TwgShowColor.Create(self);
  with ShowColor do
  begin
    SetDimensions(8,156,308,60);
  end;

  PredefColors := TwgPredefColors.Create(self);
  with PredefColors do
  begin
    SetDimensions(104,8,212,116);
    OnColorClick := ColorClick;
  end;

  btnOK := TWGBUTTON.Create(self);
  with btnOK do
  begin
    SetDimensions(8,228,105,24);
    Text := u8('OK');
    OnClick := ButtonClick;
    ImageName := 'stdimg.ok';
  end;

  btnCancel := TWGBUTTON.Create(self);
  with btnCancel do
  begin
    SetDimensions(212,228,105,24);
    Text := u8('Cancel');
    OnClick := ButtonClick;
    ImageName := 'stdimg.cancel';
  end;

  {@VFD_BODY_END: frmSelectColor}

  with PredefColors do
  begin
    AddColor($000000);
    AddColor($000080);
    AddColor($008000);
    AddColor($800000);
    AddColor($008080);
    AddColor($800080);
    AddColor($808000);
    AddColor($808080);
    AddColor($8080ff);
    AddColor($80ff80);
    AddColor($ff8080);
    AddColor($80ffff);
    AddColor($ff80ff);
    AddColor($ffff80);
    AddColor($ffffff);

//    AddColor($000000);
//    AddColor($000000);
//    AddColor($000000);
//    AddColor($000000);
  end;

  SetCurColor(12345);

end;

procedure TfrmSelectColor.ColorClick(sender: TObject);
begin
  Writeln('color clicked: ',IntToHex(TwgShowColor(sender).color,6));

  SetCurColor(TwgShowColor(sender).color);
end;

procedure TfrmSelectColor.SetCurColor(col: TGfxColor);
begin
  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edHex.Text8 := IntToHex(col,6);

  edBComp.Text8 := IntToStr((col and $0000FF));
  edGComp.Text8 := IntToStr((col and $00FF00) shr 8);
  edRComp.Text8 := IntToStr((col and $FF0000) shr 16);
end;

procedure TfrmSelectColor.RGBChange(sender: TObject);
var
  col : TGfxColor;
begin
  col := StrToIntDef(edBComp.Text8,0)
         + StrToIntDef(edGComp.Text8,0) shl 8
         + StrToIntDef(edRComp.Text8,0) shl 16;

  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edHex.Text8 := IntToHex(col,6);
end;

procedure TfrmSelectColor.HexChange(sender: TObject);
var
  col : TGfxColor;
begin
  col := StrToHex(edHex.Text8);

  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edBComp.Text8 := IntToStr((col and $0000FF));
  edGComp.Text8 := IntToStr((col and $00FF00) shr 8);
  edRComp.Text8 := IntToStr((col and $FF0000) shr 16);
end;

procedure TfrmSelectColor.ButtonClick(sender: TObject);
begin
  if sender = btnOK then ModalResult := 1 else ModalResult := -1;
  Close;
end;

end.

