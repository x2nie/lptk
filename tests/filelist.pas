program filelist;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, wgcustomgrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox, wgbevel,
  libc, linux;

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
    mode : integer;
    modtime : TDateTime;
    ownerid : integer;
    groupid : integer;
    linktarget : string;
    
    constructor Create;
  end;
  
  TFileList = class
  private
    FEntries : TList;
    FCurDirEntry : TFileEntry;
    
    FDirectoryName : string;
    
    function GetEntry(i : integer): TFileEntry;
  public

    constructor Create;
    destructor Destroy; override;
    
    procedure Clear;
    function Count : integer;
    
    property Entry[i : integer] : TFileEntry read GetEntry;
    property CurDirEntry : TFileEntry read FCurDirEntry;
    property DirectoryName : string read FDirectoryName;
    
    function ReadDirectory(const fmask : string) : integer;
    
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

  TfrmFileList = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmFileList}
    chlDir : TwgChoiceList;
    grid : TwgFileGrid;
    panel1 : TwgBevel;
    lbFileInfo : TwgLabel;
    edFilename : TwgEdit;
    chlFilter : TwgChoiceList;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    lb1 : TwgLabel;
    lb2 : TwgLabel;
    {@VFD_HEAD_END: frmFileList}
    
    procedure AfterCreate; override;
    
    procedure ListChange(Sender : TObject; row : integer);
    procedure DirChange(Sender : TObject);
    procedure GridDblClick(Sender : TObject; x,y : integer; var btnstate, shiftstate : word);

    procedure CancelClick(sender : TObject);

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    
    procedure SetCurrentDir(const dir : string);

  end;
  
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


{ TwgFileGrid }

constructor TwgFileGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  flist := TFileList.Create;

  AddColumn8('Name',220);
  AddColumn8('Size',80);
  AddColumn8('Mod. Time',108);
  AddColumn8('Rights',78);
  AddColumn8('Owner',54);
  AddColumn8('Group',54);
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
  x,y,b,n : integer;
  rx : integer;
  s : string;
  img : TGfxImage;
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
         if (e.mode and $40) <> 0
           then img := GfxLibGetImage('stdimg.yes')
           else img := GfxLibGetImage('stdimg.document');
       end;
                          
       if img <> nil then canvas.DrawImage(rect.Left+1,y,img);
       x := rect.left + 20;
       s := u8(e.Name);
     end;
  2: begin
       s := u8(FormatFloat('###,###,###,##0',e.size));
       x := rect.right - Font.TextWidth16(s) - 1;
       if x < rect.Left+2 then x := rect.Left+2;
     end;
  3: s := u8(FormatDateTime('yyyy.mm.dd hh:nn',e.modtime));
//  4: s := u8(e.linktarget);
  4: begin
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
       canvas.SetFont(FixedFont);
       s := u8(s);
     end;
  5: s := u8(GetUserName(e.ownerid));  // use getpwuid(); for the name of this user
  6: s := u8(GetGroupName(e.groupid));  // use getgrgid(); for the name of this group
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
  mode := 0;
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
  FCurDirEntry := nil;
  FDirectoryName := '';
end;

destructor TFileList.Destroy;
begin
  if FCurDirEntry <> nil then FCurDirEntry.Free;
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

function EpochToDateTime(epoch : longint) : TDateTime;
var
  w1,w2,w3,w4,w5,w6 : word;
begin
  EpochToLocal(epoch,w1,w2,w3,w4,w5,w6);
  result := EncodeDate(w1,w2,w3)+EncodeTime(w4,w5,w6,0);
end;

function TFileList.ReadDirectory(const fmask : string): integer;
Var
  gres,p : PGlob;
  e : TFileEntry;
  fullname : string;
  info : stat;
  dname : string;
begin
  Clear;
  FDirectoryName := dirname(fmask);
  if FDirectoryName = '' then GetDir(0,dname) else dname := FDirectoryName;
  if dname = '' then dname := '/';
  Writeln('dname: ', dname);

  if (FDirectoryName <> '') and (copy(FDirectoryName,Length(FDirectoryName),1) <> '/') then FDirectoryName := FDirectoryName+'/';
  gres := glob(fmask);
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
      
      if e.name = '.' then
      begin
        if FCurDirEntry <> nil then FCurDirEntry.Free;
        FCurDirEntry := e;
      end
      else if (e.name = '..') and (dname = '/') then
      begin
        // do not add for the root
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

procedure TFileList.Sort(order: TFileListSortOrder);
var
  newl : TList;
  n,i : integer;
  e,e2 : TFileEntry;
  
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
  Writeln('entry count: ',fl.ReadDirectory('/workl/*'));
  fl.Sort(soFileExt);
  for n := 1 to fl.Count do
  begin
    Writeln(n,'.: ',fl.Entry[n].Name,' time: ',DateTimeToStr(fl.Entry[n].modtime));
  end;
  
  write('test finished...'); readln;
end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

{
Var
  G1,G2 : PGlob;
begin
  G1:=Glob ('*');
  if LinuxError=0 then
    begin
    G2:=G1;
    Writeln ('Files in this directory : ');
    While g2<>Nil do
      begin
      Writeln (g2^.name);
      g2:=g2^.next;
      end;
    GlobFree (g1);
    end;
    
      if lstat(dirname+filename,info) then
      begin
        Write('   size: ',info.size,', mode:',IntToHex(info.mode,8));
        if (info.mode and $F000) = $A000 then
        begin
          write('-->',ReadLink(dirname+filename));
        end;
        if (info.mode and $F000) = $4000 then
        begin
          write('(directory)');
        end;
      end
      else
      begin
        Write('fstat error');
      end;
}

procedure TfrmFileList.AfterCreate;
begin
  Writeln('aftercreate');

  {@VFD_BODY_BEGIN: frmFileList}
  SetDimensions(303,171,640,460);
  WindowTitle8 := 'frmFileList';

  chlDir := TwgChoiceList.Create(self);
  with chlDir do
  begin
    SetDimensions(8,12,538,22);
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
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,427,96,24);
    Anchors := [anLeft,anBottom];
    Text := u8('OK');
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 0;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(534,427,96,24);
    Anchors := [anRight,anBottom];
    Text := u8('Cancel');
    FontName := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := 0;
    OnClick := CancelClick;
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

  {@VFD_BODY_END: frmFileList}
  
  grid.flist.ReadDirectory('*');
  grid.flist.Sort(soFileName);
  grid.Update;
end;

procedure TfrmFileList.ListChange(sender: TObject; row : integer);
var
  s : string;
begin
  if grid.currententry = nil then Exit;
  s := grid.currententry.Name;
  
  if grid.currententry.islink then s := s + ' -> '+grid.currententry.linktarget;

  if grid.currententry.etype = etDir
    then edFileName.Text := ''
    else edFileName.Text8 := grid.currententry.Name;

  lbFileInfo.Text8 := s;
end;

procedure TfrmFileList.DirChange(Sender: TObject);
begin
  SetCurrentDir(chlDir.Text8);
end;

procedure TfrmFileList.GridDblClick(Sender: TObject; x, y: integer; var btnstate, shiftstate: word);
var
  e : TFileEntry;
begin
  e := grid.CurrentEntry;
  if (e <> nil) and (e.etype = etDir) then
  begin
    SetCurrentDir(e.Name);
  end;
end;

procedure TfrmFileList.CancelClick(sender: TObject);
begin
  Close;
end;

procedure TfrmFileList.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
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
        SetCurrentDir(e.Name);
        consumed := true;
      end;
    end;
  end;
  if not consumed then inherited;
end;

procedure TfrmFileList.SetCurrentDir(const dir: string);
var
  ds,s : string;
  n : integer;
begin
  try
    chdir(dir);
    edFileName.Text := '';
  except
    ShowMessage8('Could not open the directory '+dir,'Error');
  end;
  
  GetDir(0,ds);
  if copy(ds,1,1) <> DirectorySeparator then ds := DirectorySeparator + ds;
  
  chlDir.Items.Clear;
  chlDir.Items.Add(u8(DirectorySeparator));  // add root
  n := 2;
  while n <= length(ds) do
  begin
    if ds[n] = DirectorySeparator then
    begin
      chlDir.Items.Add(u8(copy(ds,1,n-1)));
    end;

    inc(n);
  end;
  
  chlDir.Items.Add(u8(ds));
  chlDir.FocusItem := chlDir.Items.Count;
  
  grid.flist.ReadDirectory('*');
  grid.flist.Sort(soFileName);
  grid.Update;
  grid.FocusRow := 1;

end;

var
  frm : TfrmFileList;
begin
//  TestFileList;
//  Exit;

  GfxOpenDisplay('');
  frm := TfrmFileList.Create(nil);
  frm.Show;
  GfxDoMessageLoop;
  GfxCloseDisplay;
end.
