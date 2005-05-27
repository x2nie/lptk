{ Copyright (c) 2003, Nagy Viktor

 Small, but useful phonebook application
}

program phonebook;

{$APPTYPE GUI}

{$IFDEF FPC}
    {$mode delphi}
    {$H+}
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
    lbLabel2 : TWGLABEL;
    edFLTNAME : TWGEDIT;
    lbLabel1 : TWGLABEL;
    chlFLTCAT : TWGCHOICELIST;
    grid : TWGDBGRID;
    txtOther : TWGMEMO;
    btnNew : TWGBUTTON;
    btnEdit : TWGBUTTON;
    btnDelete : TWGBUTTON;
    btnClose : TWGBUTTON;
    lbLabel3 : TWGLABEL;
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
    procedure gridDoubleClick(Sender: TObject; x,y : integer; var button : word; var shiftstate : word);

    procedure FilterChange(sender : TObject);

  end;

  TFormEdit = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: FormEdit}
    lbLabel1 : TWGLABEL;
    edCAT : TWGEDIT;
    lbLabel2 : TWGLABEL;
    edNAME : TWGEDIT;
    lbLabel3 : TWGLABEL;
    edPHONE : TWGEDIT;
    lbLabel4 : TWGLABEL;
    edOTHER : TWGMEMO;
    btnOK : TWGBUTTON;
    btnCancel : TWGBUTTON;
    {@VFD_HEAD_END: FormEdit}

    procedure AfterCreate; override;

    procedure OnButtonClick(Sender : TObject);
  end;

  TfrmAskDelete = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmAskDelete}
    lbLabel1 : TWGLABEL;
    btnYES : TWGBUTTON;
    btnNO : TWGBUTTON;
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
  WindowTitle8 := 'Deleting entry';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(32,8,274,16);
    Text := u8('Are you sure you want to delete this entry ?');
  end;

  btnYES := TWGBUTTON.Create(self);
  with btnYES do
  begin
    SetDimensions(72,44,80,24);
    Text := u8('YES');
    ImageName := 'stdimg.yes';
    OnClick := ButtonClick;
  end;

  btnNO := TWGBUTTON.Create(self);
  with btnNO do
  begin
    SetDimensions(172,44,80,24);
    Text := u8('NO');
    ImageName := 'stdimg.no';
    OnClick := ButtonClick;
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
  SetDimensions(285,329,321,306);
  WindowTitle8 := 'Edit phonebook entry';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(12,8,74,16);
    Text := u8('Cathegory:');
  end;

  edCAT := TWGEDIT.Create(self);
  with edCAT do
  begin
    SetDimensions(12,28,86,22);
    Text := u8('');
  end;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(12,56,54,16);
    Text := u8('Name:');
  end;

  edNAME := TWGEDIT.Create(self);
  with edNAME do
  begin
    SetDimensions(12,76,299,22);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('');
  end;

  lbLabel3 := TWGLABEL.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(12,104,110,16);
    Text := u8('Phone numbers:');
  end;

  edPHONE := TWGEDIT.Create(self);
  with edPHONE do
  begin
    SetDimensions(12,124,299,22);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('');
  end;

  lbLabel4 := TWGLABEL.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(12,148,54,16);
    Text := u8('Memo:');
  end;

  edOTHER := TWGMEMO.Create(self);
  with edOTHER do
  begin
    SetDimensions(12,168,299,100);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add(u8(''));
  end;

  btnOK := TWGBUTTON.Create(self);
  with btnOK do
  begin
    SetDimensions(12,276,100,24);
    Anchors := [anLeft,anBottom];
    Text := u8('OK');
    ImageName := 'stdimg.ok';
    OnClick := OnButtonClick;
  end;

  btnCancel := TWGBUTTON.Create(self);
  with btnCancel do
  begin
    SetDimensions(211,276,100,24);
    Anchors := [anRight,anBottom];
    Text := u8('Cancel');
    ImageName := 'stdimg.cancel';
    OnClick := OnButtonClick;
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
  SetDimensions(329,187,573,356);
  WindowTitle8 := 'Phonebook';
  WindowPosition := wpScreenCenter;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(8,8,54,16);
    Text := u8('Name:');
  end;

  edFLTNAME := TWGEDIT.Create(self);
  with edFLTNAME do
  begin
    SetDimensions(8,28,154,22);
    Text := u8('');
    OnChange := FilterChange;
  end;

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(168,8,74,16);
    Text := u8('Cathegory:');
  end;

  chlFLTCAT := TWGCHOICELIST.Create(self);
  with chlFLTCAT do
  begin
    SetDimensions(168,28,114,22);
    OnChange := FilterChange;
  end;

  grid := TWGDBGRID.Create(self);
  with grid do
  begin
    SetDimensions(8,60,465,152);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn8('Name','NAME',160,alLeft);
    AddColumn8('Cat.','CATEGORY',60,alLeft);
    AddColumn8('Phone','PHONE',220,alLeft);
    OnRowChange := GridRowChange;
    OnDoubleClick := gridDoubleClick;
    RowSelect := true;
  end;

  txtOther := TWGMEMO.Create(self);
  with txtOther do
  begin
    SetDimensions(8,240,465,108);
    Anchors := [anLeft,anRight,anBottom];
    Lines.Add(u8(''));
  end;

  btnNew := TWGBUTTON.Create(self);
  with btnNew do
  begin
    SetDimensions(482,59,80,24);
    Anchors := [anRight,anTop];
    Text := u8('New');
    ImageName := 'stdimg.new';
    OnClick := OnEditClick;
  end;

  btnEdit := TWGBUTTON.Create(self);
  with btnEdit do
  begin
    SetDimensions(482,87,80,24);
    Anchors := [anRight,anTop];
    Text := u8('Edit');
    ImageName := 'stdimg.edit';
    OnClick := OnEditClick;
  end;

  btnDelete := TWGBUTTON.Create(self);
  with btnDelete do
  begin
    SetDimensions(482,139,80,24);
    Anchors := [anRight,anTop];
    Text := u8('Delete');
    ImageName := 'stdimg.delete';
    OnClick := DeleteClick;
  end;

  btnClose := TWGBUTTON.Create(self);
  with btnClose do
  begin
    SetDimensions(482,324,80,24);
    Anchors := [anRight,anBottom];
    Text := u8('Close');
    ImageName := 'stdimg.close';
    OnClick := OnCloseClick;
  end;

  lbLabel3 := TWGLABEL.Create(self);
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

procedure TFormMain.gridDoubleClick(Sender: TObject; x, y: integer; var button: word; var shiftstate: word);
begin
  btnEdit.Click;
end;

procedure TFormMain.FilterChange(sender: TObject);
begin
  if grid.RowCount > 0 then ReQuery(grid.FocusField('ID').asInteger)
                       else ReQuery(-1);
end;

begin
  gfxOpenDisplay('');
  
  dbconn := TSqlDBConnection.Create;
  if not dbconn.ConnectByDriver('MySQL','192.168.1.1','phonebook','root','','') then
  begin
    Writeln('cannot connect to db.');
  end;

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
