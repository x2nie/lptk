{ Copyright (c) 2003, Nagy Viktor

 Main window functionality
}

unit vfdmain;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxdialogs, sqldb, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox,
  vfdresizer, vfdforms, vfddesigner, vfdfile, newformdesigner, wgfiledialog;

const
  program_version = '0.84';

{version description:
0.84
  - "New" in the file menu
  - Small changes
  - Font name editing bugfix on Windows
  - Once in the taskbar on Windows

0.83
  - New file dialog usage
  - File Name in the WindowTitle

0.82
  - Removed file dialogs due instability
  
0.81
  - somewhat rearranged source

0.80b
  - redesigned interface
  
0.58
  - wgGrid interface change
  
0.57
  - WindowTitle8 correction

0.56
  - DBColumn editor

0.55
  - Position editors
  - Widget order editing (F2)
  - Items editor at F4
  - widget selection by TAB
  - New form creation dialog

0.51
  - ENTER switches between the form and properties
  - Other widget handling

0.50
}

type

  TMainDesigner = class
  private
    procedure SetEditedFileName(const Value: string);
  protected
    FDesigners : TList;

    FFile : TVFDFile;

    FEditedFileName : string;

  public
    GridResolution : integer;

    SaveComponentNames : boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CreateWindows;

  public
    selectedform : TFormDesigner;

    procedure SelectForm(aform : TFormDesigner);

    function Designer(index : integer) : TFormDesigner;
    function DesignerCount : integer;

    function NewFormName : string;

  public
    procedure CreateParseForm(const FormName, FormHead, FormBody : string);

  public

    procedure OnNewForm(sender : TObject);

    procedure OnNewFile(sender : TObject);
    procedure OnSaveFile(sender : TObject);
    procedure OnLoadFile(sender : TObject);

    procedure OnPaletteChange(Sender : TObject);

    procedure OnPropTextChange(sender : TObject);
    procedure OnPropNameChange(sender : TObject);

    procedure OnPropPosEdit(sender : TObject);

    procedure OnOtherChange(sender : TObject);
    procedure OnAnchorChange(sender : TObject);

    procedure OnEditWidget(sender : TObject);

    procedure OnEditWidgetOrder(sender : TObject);

    procedure OnExit(sender : TObject);

    procedure OnOptionsClick(sender : TObject);

  public
    property EditedFileName : string read FEditedFileName write SetEditedFileName;

  end;

var
  maindsgn : TMainDesigner;

implementation

uses vfdformparser;

{ TMainDesigner }

procedure TMainDesigner.OnNewFile(sender: TObject);
var
  n : integer;
begin
  EditedFileName := '';
  for n := 0 to FDesigners.Count-1 do
  begin
    selectedform := nil;
    TFormDesigner(FDesigners[n]).Free;
  end;
  FDesigners.Clear;
  OnNewForm(sender);
end;


procedure TMainDesigner.OnLoadFile(sender: TObject);
var
  n, m : integer;
  bl, bl2 : TVFDFileBlock;
  fname : string;
  afiledialog : TfrmFileDialog;
begin
  fname := EditedFileName;

  if sender <> maindsgn then
  begin
    afiledialog := TfrmFileDialog.Create(nil);
    afiledialog.Filename := EditedFilename;
    afiledialog.WindowTitle8 := 'Open form file';
    afiledialog.Filter := 'Pascal source files (*.pas;*.inc;*.dpr)|*.pas;*.inc;*.dpr|All Files (*)|*';
    if afiledialog.RunOpenFile then
    begin
      EditedFileName := aFileDialog.Filename;
      fname := EditedFilename;
    end
    else fname := '';
    FreeAndNil(aFileDialog);
  end;

  if fname = '' then Exit;

  for n := 0 to FDesigners.Count-1 do
  begin
    selectedform := nil;
    TFormDesigner(FDesigners[n]).Free;
  end;
  FDesigners.Clear;

  if not FileExists(fname) then
  begin
    ShowMessage8('File does not exists.','Error loading form');
    Exit;
  end;

  Writeln('loading file...');

  FFile.LoadFile(fname);
  FFile.GetBlocks;

  for n := 1 to FFile.BlockCount do
  begin
    bl := FFile.Block(n);
    if bl.BlockID = 'VFD_HEAD_BEGIN' then
    begin
      for m := n+1 to FFile.BlockCount do
      begin
        bl2 := FFile.Block(m);
        if (bl2.BlockID = 'VFD_BODY_BEGIN') and (bl2.FormName = bl.FormName) then
        begin
          // pair has found
          //Writeln('Parsing form: ',bl.FormName);
          //Writeln(bl.data);
          //Writeln(bl2.data);
          CreateParseForm(bl.FormName, bl.Data, bl2.Data);
        end;
      end;
    end;
  end;

end;

procedure TMainDesigner.OnSaveFile(sender: TObject);
var
  n,i : integer;
  fd : TFormDesigner;

  fdata : string;

  ff : file;

  fname, uname : string;
  aFileDialog : TfrmFileDialog;
begin
  fname := EditedFileName;

  afiledialog := TfrmFileDialog.Create(nil);
  afiledialog.Filename := EditedFilename;
  afiledialog.WindowTitle8 := 'Save form source';
  afiledialog.Filter := 'Pascal source files (*.pas;*.inc;*.dpr)|*.pas;*.inc;*.dpr|All Files (*)|*';
  if afiledialog.RunSaveFile then
  begin
    EditedFileName := aFileDialog.Filename;
    fname := EditedFilename;
  end
  else fname := '';
  aFileDialog.Free;

  if fname = '' then Exit;

  EditedFileName := fname;

  if FileExists(fname) then
  begin
    FFile.LoadFile(fname);
    FFile.GetBlocks;
  end
  else
  begin
    uname := ExtractFileName(fname);
    i := pos('.pas',LowerCase(uname));
    if i > 0 then uname := copy(uname,1,i-1);
    FFile.NewFileSkeleton(uname);
  end;

  for n:=1 to DesignerCount do
  begin
    fd := Designer(n);
    FFile.SetFormData(fd.Form.Name, fd.GetFormSourceDecl, fd.GetFormSourceImpl);
  end;

  fdata := FFile.MergeBlocks;

  AssignFile(ff, fname);
  try
    Rewrite(ff,1);
    try
      BlockWrite(ff,fdata[1],length(fdata));
    finally
      CloseFile(ff);
    end;
    writeln('Form saved.');

    frmMain.WindowTitle8 := fname+' - VFD v'+program_version;
    
  except
    Writeln('Form save I/O failure.');
  end;

end;

procedure TMainDesigner.OnAnchorChange(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnAnchorChange(sender);
end;

procedure TMainDesigner.OnOtherChange(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnOtherChange(sender);
end;

procedure TMainDesigner.OnPropNameChange(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnPropNameChange(sender);
end;

procedure TMainDesigner.OnPropPosEdit(sender: TObject);
begin
  if SelectedForm <> nil then SelectedForm.OnPropPosEdit(sender);
end;

procedure TMainDesigner.OnPropTextChange(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnPropTextChange(sender);
end;

procedure TMainDesigner.OnPaletteChange(Sender: TObject);
begin

end;

procedure TMainDesigner.OnNewForm(sender: TObject);
var
  fd : TFormDesigner;
  nfrm : TNewFormForm;
begin
  Writeln('new form');
  nfrm := TNewFormForm.Create(nil);
  if nfrm.ShowModal = 1 then
  begin
    if nfrm.edName.Text8 <> '' then
    begin
      fd := TFormDesigner.Create;
      fd.Form.Name := nfrm.edName.Text8;
      fd.Form.WindowTitle := nfrm.edName.Text;
      FDesigners.Add(fd);
      fd.Show;
    end;
  end;
  nfrm.Free;
end;

procedure TMainDesigner.CreateWindows;
//var
//  fd : TFormDesigner;
begin
  frmMain := TfrmMain.Create(nil);
  frmMain.WindowTitle8 := 'LPTK VFD - v'+program_version;
  frmMain.Show;

  frmProperties := TfrmProperties.Create(nil);
  frmProperties.Show;

//  fd := TFormDesigner.Create;
//  fd.Form.Name := 'frmNewForm';
//  fd.Form.WindowTitle := u8('frmNewForm');
//  FDesigners.Add(fd);
//  fd.Show;
end;

constructor TMainDesigner.Create;
begin
  FDesigners := TList.Create;
  SelectedForm := nil;
  FFile := TVFDFile.Create;

  // options
  SaveComponentNames := false;
  GridResolution := 4;

  FEditedFileName := '';
end;

destructor TMainDesigner.Destroy;
var
  n : integer;
begin
  for n:=0 to FDesigners.Count-1 do TFormDesigner(FDesigners[n]).Free;
  FDesigners.Free;
  FFile.Free;
  inherited;
end;

procedure TMainDesigner.SelectForm(aform: TFormDesigner);
begin
  //Writeln('selected...');
  if (SelectedForm <> nil) and (SelectedForm <> aform) then SelectedForm.DeSelectAll;
  SelectedForm := aform;
end;

function TMainDesigner.Designer(index: integer): TFormDesigner;
begin
  result := nil;
  if (index < 1) or (index > FDesigners.Count) then Exit;
  result := TFormDesigner(FDesigners[index-1]);
end;

function TMainDesigner.DesignerCount: integer;
begin
  result := FDesigners.Count;
end;

function TMainDesigner.NewFormName: string;
var
  n,i : integer;
  s : string;
begin
  i := 0;
  repeat
    inc(i);
    s := 'Form'+IntToStr(i);
    n := 1;
    while (n <= DesignerCount) do
    begin
      if Designer(n).Form.Name = s then Break;
      inc(n);
    end;
  until n > DesignerCount;
  result := s;
end;

procedure TMainDesigner.CreateParseForm(const FormName, FormHead, FormBody: string);
var
  fd : TFormDesigner;
  fp : TVFDFormParser;
begin
  Writeln('CreateParseForm: ',FormName);

  fp := TVFDFormParser.Create(FormName, FormHead, FormBody);
  fd := fp.ParseForm;
  fp.Free;

  FDesigners.Add(fd);
  fd.Show;
end;

procedure TMainDesigner.OnEditWidget(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.OnEditWidget(sender);
end;

procedure TMainDesigner.OnEditWidgetOrder(sender: TObject);
begin
  if SelectedForm <> nil then
    SelectedForm.EditWidgetOrder;
end;

procedure TMainDesigner.OnExit(sender: TObject);
begin
  halt(0);
end;

procedure TMainDesigner.OnOptionsClick(sender: TObject);
var
  frm : TfrmVFDSetup;
begin
  frm := TfrmVFDSetup.Create(nil);

  case GridResolution of
  1 : frm.chlGrid.FocusItem := 1;
  4 : frm.chlGrid.FocusItem := 2;
  8 : frm.chlGrid.FocusItem := 3;
  end;

  if frm.ShowModal > 0 then
  begin
    case frm.chlGrid.FocusItem of
    1 : GridResolution := 1;
    2 : GridResolution := 4;
    3 : GridResolution := 8;
    end;
  end;

  frm.Free;
end;

procedure TMainDesigner.SetEditedFileName(const Value: string);
var
  s : string;
begin
  FEditedFileName := Value;
  s := ExtractFileName(FEditedFileName);
  if s = '' then s := '[new]';
  frmMain.WindowTitle8 := s + ' - VFD '+program_version;
end;

end.
