{ Copyright (c) 2003, Nagy Viktor 

 Small, but useful phonebook application
}

program phonebook;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

{ DB structure:

create table phonebook
(
  id int auto_increment,
  category varchar(16),
  name varchar(80),
  phone varchar(40),
  other text,
  primary key (id)
);

}

uses
  SysUtils, Classes, inifiles, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox;

type

  TFormMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: FormMain}
    lbLabel2 : TwgLabel;
    edFLTNAME : TwgEdit;
    lbLabel1 : TwgLabel;
    chlFLTCAT : TwgChoiceList;
    grid : TwgDBGrid;
    txtOther : TwgMemo;
    btnNew : TwgButton;
    btnEdit : TwgButton;
    btnDelete : TwgButton;
    btnClose : TwgButton;
    lbLabel3 : TwgLabel;
    {@VFD_HEAD_END: FormMain}

    procedure AfterCreate; override;
    
  public
    procedure OnEditClick(Sender : TObject);
    
    procedure OnCloseClick(Sender : TObject);
    
    procedure DeleteClick(Sender : TObject);

  public
    qr : TSqlResult;
    
    procedure PopulateCat;
  
    procedure ReQuery(id : integer);
    
    procedure EditEntry(id : integer);
    
    procedure GridRowChange(Sender : TObject; row : integer);
    
    procedure FilterChange(sender : TObject);
  
  end;

  TFormEdit = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: FormEdit}
    lbLabel1 : TwgLabel;
    edCAT : TwgEdit;
    lbLabel2 : TwgLabel;
    edNAME : TwgEdit;
    lbLabel3 : TwgLabel;
    edPHONE : TwgEdit;
    lbLabel4 : TwgLabel;
    edOTHER : TwgMemo;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: FormEdit}

    procedure AfterCreate; override;

    procedure OnButtonClick(Sender : TObject);
  end;

  TfrmAskDelete = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmAskDelete}
    lbLabel1 : TwgLabel;
    btnYES : TwgButton;
    btnNO : TwgButton;
    {@VFD_HEAD_END: frmAskDelete}

    procedure AfterCreate; override;
    
    procedure ButtonClick(Sender : TObject);

  end;

{@VFD_NEWFORM_DECL}

var
  FormMain : TFormMain;
  FormEdit : TFormEdit;
  dbconn : TSqlDBConnection;

{@VFD_NEWFORM_IMPL}

procedure TfrmAskDelete.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmAskDelete}
  SetDimensions(300,100,339,83);
  WindowTitle := 'Deleting entry';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(32,8,274,16);
    Text := u8('Are you sure you want to delete this entry ?');
  end;

  btnYES := TwgButton.Create(self);
  with btnYES do
  begin
    SetDimensions(72,44,80,24);
    Text := u8('YES');
    OnClick := {$ifdef FPC}@{$endif}ButtonClick;
  end;

  btnNO := TwgButton.Create(self);
  with btnNO do
  begin
    SetDimensions(172,44,80,24);
    Text := u8('NO');
    OnClick := {$ifdef FPC}@{$endif}ButtonClick;
  end;

  {@VFD_BODY_END: frmAskDelete}
end;

procedure TfrmAskDelete.ButtonClick(Sender: TObject);
begin
  if Sender = btnYES then ModalResult := 1 else ModalResult := 2;
end;


procedure TFormEdit.AfterCreate;
begin
  {@VFD_BODY_BEGIN: FormEdit}
  SetDimensions(334,266,321,306);
  WindowTitle := 'Edit phonebook entry';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(12,8,74,16);
    Text := u8('Cathegory:');
  end;

  edCAT := TwgEdit.Create(self);
  with edCAT do
  begin
    SetDimensions(12,28,86,20);
    Text := u8('');
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(12,56,54,16);
    Text := u8('Name:');
  end;

  edNAME := TwgEdit.Create(self);
  with edNAME do
  begin
    SetDimensions(12,76,299,20);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('');
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(12,104,110,16);
    Text := u8('Phone numbers:');
  end;

  edPHONE := TwgEdit.Create(self);
  with edPHONE do
  begin
    SetDimensions(12,124,299,20);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('');
  end;

  lbLabel4 := TwgLabel.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(12,148,54,16);
    Text := u8('Memo:');
  end;

  edOTHER := TwgMemo.Create(self);
  with edOTHER do
  begin
    SetDimensions(12,168,299,100);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add(u8(''));
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(12,276,100,24);
    Anchors := [anLeft,anBottom];
    Text := u8('OK');
    OnClick := {$ifdef FPC}@{$endif}OnButtonClick;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(211,276,100,24);
    Anchors := [anRight,anBottom];
    Text := u8('Cancel');
    OnClick := {$ifdef FPC}@{$endif}OnButtonClick;
  end;

  {@VFD_BODY_END: FormEdit}
end;

procedure TFormEdit.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

procedure TFormMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: FormMain}
  SetDimensions(295,122,573,356);
  WindowTitle := 'Phonebook';
  WindowPosition := wpScreenCenter;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(8,8,54,16);
    Text := u8('Name:');
  end;

  edFLTNAME := TwgEdit.Create(self);
  with edFLTNAME do
  begin
    SetDimensions(8,29,154,20);
    Text := u8('');
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(168,8,74,16);
    Text := u8('Cathegory:');
  end;

  chlFLTCAT := TwgChoiceList.Create(self);
  with chlFLTCAT do
  begin
    SetDimensions(168,28,114,22);
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  grid := TwgDBGrid.Create(self);
  with grid do
  begin
    SetDimensions(8,60,465,152);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn8('Name','NAME',160,alLeft);
    AddColumn8('Cat.','CATEGORY',60,alLeft);
    AddColumn8('Phone','PHONE',220,alLeft);
    OnRowChange := {$ifdef FPC}@{$endif}GridRowChange;
    RowSelect := true;
  end;

  txtOther := TwgMemo.Create(self);
  with txtOther do
  begin
    SetDimensions(8,240,465,108);
    Anchors := [anLeft,anRight,anBottom];
    Lines.Add(u8(''));
  end;

  btnNew := TwgButton.Create(self);
  with btnNew do
  begin
    SetDimensions(482,59,80,24);
    Anchors := [anRight,anTop];
    Text := u8('New');
    OnClick := {$ifdef FPC}@{$endif}OnEditClick;
  end;

  btnEdit := TwgButton.Create(self);
  with btnEdit do
  begin
    SetDimensions(482,87,80,24);
    Anchors := [anRight,anTop];
    Text := u8('Edit');
    OnClick := {$ifdef FPC}@{$endif}OnEditClick;
  end;

  btnDelete := TwgButton.Create(self);
  with btnDelete do
  begin
    SetDimensions(482,139,80,24);
    Anchors := [anRight,anTop];
    Text := u8('Delete');
    OnClick := {$ifdef FPC}@{$endif}DeleteClick;
  end;

  btnClose := TwgButton.Create(self);
  with btnClose do
  begin
    SetDimensions(482,324,80,24);
    Anchors := [anRight,anBottom];
    Text := u8('Close');
    OnClick := {$ifdef FPC}@{$endif}OnCloseClick;
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(8,220,54,16);
    Anchors := [anLeft,anBottom];
    Text := u8('Memo:');
  end;

  {@VFD_BODY_END: FormMain}

  qr := nil;
end;

procedure TFormMain.OnEditClick(Sender: TObject);
begin
  if Sender = btnNew then EditEntry(-1)
                     else EditEntry(grid.FocusField('ID').asInteger);
end;

procedure TFormMain.OnCloseClick(Sender: TObject);
begin
  FormMain.Close;
  Halt(0);
end;

procedure TFormMain.DeleteClick(Sender: TObject);
var
  frm : TfrmAskDelete;
begin
  frm := TfrmAskDelete.Create(nil);
  if frm.ShowModal = 1 then
  begin
    dbconn.ExecStatement('delete from phonebook where id='+grid.FocusField('ID').asString);
    PopulateCat;
    ReQuery(-1);
  end;
  frm.Free;
end;

procedure TFormMain.PopulateCat;
var
  r : TSqlResult;
  ci : integer;
  lcat, cat : string;
  n : integer;
begin
  r := dbconn.RunQuery('select distinct category as CAT from phonebook order by 1');
  lcat := chlFLTCAT.Text;
  chlFLTCAT.Items.Clear;
  chlFLTCAT.Items.Add(u8('*'));
  n := 2;
  ci := 1;
  while not r.eof do
  begin
    cat := u8(r.Field('CAT').asString);
    if lcat = cat then ci := n;
    chlFLTCAT.Items.Add(cat);
    r.FetchNext;
    inc(n);
  end;
  r.Free;
end;

procedure TFormMain.ReQuery(id: integer);
var
  fr,n : integer;
  s : string;
begin
  if qr <> nil then qr.Free;
  
  s := 'select * from phonebook where (1=1) ';
  if chlFLTCAT.FocusItem > 1 then s := s + ' and (CATEGORY='''+chlFLTCAT.Text8+ ''')';
  
  if edFLTNAME.Text8 <> '' then s := s + ' and (NAME like '''+edFLTNAME.Text8+'%'')';
  
  s := s + ' order by NAME, ID';
  
  qr := dbconn.RunQuery(s);
  fr := 1;
  n := 1;
  while not qr.Eof do
  begin
    if qr.Field('ID').asInteger = id then fr := n;
    qr.FetchNext;
    inc(n);
  end;
  
  grid.SetResultSet(qr, false);
  grid.FocusRow := fr;
end;

procedure TFormMain.EditEntry(id: integer);
var
  uis : Tdbuis;
begin
  FormEdit := TFormEdit.Create(nil);
  with FormEdit do
  begin
    if id > 0 then
    begin
      edCAT.Text8 := grid.FocusField('CATEGORY').asString;
      edNAME.Text8 := grid.FocusField('NAME').asString;
      edPHONE.Text8 := grid.FocusField('PHONE').asString;
      edOTHER.Text8 := grid.FocusField('OTHER').asString;
    end;
    
    if ShowModal = 1 then
    begin
      uis := Tdbuis.Create('phonebook',dbconn,false);

      uis.SetFieldS('CATEGORY',edCAT.Text8);
      uis.SetFieldS('NAME',edNAME.Text8);
      uis.SetFieldS('PHONE',edPHONE.Text8);
      uis.SetFieldS('OTHER',edOTHER.Text8);

      if id > 0 then uis.ExecUpdate('where id='+IntToStr(id)) else uis.ExecInsert;

      uis.Free;
      
      PopulateCat;

      ReQuery(id);
    end;
    Free;
  end;
end;

procedure TFormMain.GridRowChange(Sender: TObject; row: integer);
begin
  if grid.RowCount > 0 then txtOther.Text8 := grid.FocusField('OTHER').asString
                       else txtOther.Text8 := '';
end;

procedure TFormMain.FilterChange(sender: TObject);
begin
  if grid.RowCount > 0 then ReQuery(grid.FocusField('ID').asInteger)
                       else ReQuery(-1);
end;

begin
  dbconn := TSqlDBConnection.Create;
  if not dbconn.ConnectByDriver('MySQL','192.168.1.1','phonebook','root','','') then
  begin
    Writeln('cannot connect to db.');
  end;

  gfxOpenDisplay('');
  FormMain := TFormMain.Create(nil);
  FormMain.PopulateCat;
  FormMain.ReQuery(-1);
  FormMain.Show;
  
  repeat
    try
      GfxDoMessageLoop;
      break;
    except
      on e : Exception do ShowMessage8('Exception: '#10+e.message,'Exception');
    end;
  until false;
  
  gfxCloseDisplay;
end.
