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
  vfdresizer, vfdforms, vfddesigner, vfdfile, newformdesigner;

const
  program_version = '0.80b';

{version description:
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
  protected
    FDesigners : TList;

    FFile : TVFDFile;

  public
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

    procedure OnSaveFile(sender : TObject);
    procedure OnLoadFile(sender : TObject);

    procedure OnPaletteChange(Sender : TObject);

    procedure OnPropTextChange(sender : TObject);
    procedure OnPropNameChange(sender : TObject);

    procedure OnPropPosEdit(sender : TObject);

    procedure OnOtherChange(sender : TObject);
    procedure OnAnchorChange(sender : TObject);

    procedure OnEditWidget(sender : TObject);

  end;

var
  maindsgn : TMainDesigner;

implementation

uses vfdformparser;

{ TMainDesigner }

procedure TMainDesigner.OnLoadFile(sender: TObject);
var
  n, m : integer;
  bl, bl2 : TVFDFileBlock;
begin
  for n := 0 to FDesigners.Count-1 do
  begin
    selectedform := nil;
    TFormDesigner(FDesigners[n]).Free;
  end;
  FDesigners.Clear;

  if not FileExists(MainForm.edFormFile.Text8) then Exit;

  Writeln('loading file...');

  FFile.LoadFile(MainForm.edFormFile.Text8);
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
begin
  fname := 'aaatest.pas';
  DeleteFile(fname);

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
var
  fd : TFormDesigner;
begin
  frmMain := TfrmMain.Create(nil);
  frmMain.WindowTitle8 := 'LPTK VFD - v'+program_version;
  frmMain.Show;

  frmProperties := TfrmProperties.Create(nil);
  frmProperties.Show;

  fd := TFormDesigner.Create;
  fd.Form.Name := 'frmNewForm';
//  fd.Form.WindowTitle := u8('frmNewForm');
  FDesigners.Add(fd);
  fd.Show;
end;

constructor TMainDesigner.Create;
begin
  FDesigners := TList.Create;
  SelectedForm := nil;
  FFile := TVFDFile.Create;
  SaveComponentNames := false;
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

end.
