{ Copyright (c) 2003, Nagy Viktor

 A todo manager program (Hungarian)
}

program app_todo;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$APPTYPE CONSOLE}
{$endif}

uses
  SysUtils, Classes, inifiles, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox, gfxbmpimage;

type
  TFormMain = class(TGfxForm)
  public
    sq   : TSqlResult;

    {@VFD_HEAD_BEGIN: FormMain}
    btnNew : TwgButton;
    btnEdit : TwgButton;
    lbLabel1 : TwgLabel;
    chlSorrend : TwgChoiceList;
    lbLabel2 : TwgLabel;
    chlFelelos : TwgChoiceList;
    cbMegoldott : TwgCheckBox;
    cbRejtett : TwgCheckBox;
    grid : TwgDBGrid;
    memo : TwgMemo;
    {@VFD_HEAD_END: FormMain}

    procedure AfterCreate; override;

    procedure EditTodo(row : integer);

    procedure btnEditClick(sender : TObject);
    procedure btnNewClick(sender : TObject);

    procedure gridRowChange(sender : TObject; row : integer);

    procedure FilterChange(sender : TObject);

    procedure ReQuery;

    procedure gridDoubleClick(Sender: TObject; x,y : integer; var button : word; var shiftstate : word);
    procedure gridDrawCell(sender: TObject; row,col : integer; rect : TGfxRect; flags : integer; var stddraw : boolean);

  end;

  TFormTodoEdit = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: FormTodoEdit}
    lbLabel1 : TwgLabel;
    edTemaszam : TwgEdit;
    lbLabel2 : TwgLabel;
    edMegnev : TwgEdit;
    lbLabel3 : TwgLabel;
    lbLabel4 : TwgLabel;
    lbLabel5 : TwgLabel;
    lbLabel6 : TwgLabel;
    chlPrioritas : TwgChoiceList;
    edHatarido : TwgEdit;
    chlFelelos : TwgChoiceList;
    btnEditResp : TwgButton;
    edJelzes : TwgEdit;
    lbLabel7 : TwgLabel;
    memo : TwgMemo;
    cbMegoldva : TwgCheckBox;
    cbRejtett : TwgCheckBox;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: FormTodoEdit}

    procedure AfterCreate; override;

    procedure btnOKClick(sender : TObject);
    procedure btnCancelClick(sender : TObject);

    procedure btnEditPersonsClick(sender : TObject);

  end;

  TfrmEditPersons = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmEditPersons}
    lbLabel1 : TwgLabel;
    edKOD : TwgEdit;
    lbLabel2 : TwgLabel;
    edNEV : TwgEdit;
    grid : TwgDBGrid;
    btnSave : TwgButton;
    btnDelete : TwgButton;
    btnClose : TwgButton;
    {@VFD_HEAD_END: frmEditPersons}

    qres : TSqlResult;

    procedure AfterCreate; override;

    procedure ReQuery;

    procedure SaveClick(Sender : TObject);
    procedure CloseClick(Sender : TObject);
    procedure DeleteClick(Sender : TObject);

    procedure RowChange(sender : TObject; row : integer);
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
  dbconn  : TSqlDBConnection;
  frmMain : TFormMain;
  frmEdit : TFormTodoEdit;

  imgok, imghidden : TGfxImage;

function iifs(expr : boolean; val1,val2 : string) : string;
begin
  if expr then result := val1 else result := val2;
end;

{@VFD_NEWFORM_IMPL}

procedure TfrmAskDelete.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmAskDelete}
  SetDimensions(300,100,279,83);
  WindowTitle8 := 'Törlés';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(32,11,226,16);
    Text := u8('Tényleg kitöröljem ezt a bejegyzést ?');
  end;

  btnYES := TwgButton.Create(self);
  with btnYES do
  begin
    SetDimensions(48,44,80,24);
    Text := u8('Igen');
    OnClick := {$ifdef FPC}@{$endif}ButtonClick;
  end;

  btnNO := TwgButton.Create(self);
  with btnNO do
  begin
    SetDimensions(148,44,80,24);
    Text := u8('Nem');
    OnClick := {$ifdef FPC}@{$endif}ButtonClick;
  end;

  {@VFD_BODY_END: frmAskDelete}
end;

procedure TfrmAskDelete.ButtonClick(Sender: TObject);
begin
  if Sender = btnYES then ModalResult := 1 else ModalResult := 2;
end;

{ TFormEditPersons }

procedure TfrmEditPersons.ReQuery;
begin
  if qres <> nil then qres.Free;
  qres := dbconn.RunQuery('select * from szemely order by KOD, NEV');
  qres.FetchAll;
  grid.SetResultSet(qres, false);
end;

procedure TfrmEditPersons.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmEditPersons}
  SetDimensions(270,391,343,253);
  WindowTitle8 := 'Személyek';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,4,48,16);
    Text := u8('Kód:');
  end;

  edKOD := TwgEdit.Create(self);
  with edKOD do
  begin
    SetDimensions(8,24,74,20);
    Text := u8('');
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(96,4,48,16);
    Text := u8('Név:');
  end;

  edNEV := TwgEdit.Create(self);
  with edNEV do
  begin
    SetDimensions(96,24,150,20);
    Text := u8('');
  end;

  grid := TwgDBGrid.Create(self);
  with grid do
  begin
    SetDimensions(8,56,324,156);
    AddColumn8('Kód','KOD',100,alLeft);
    AddColumn8('Név','NEV',170,alLeft);
    RowSelect := true;
    OnRowChange := {$ifdef FPC}@{$endif}RowChange;
  end;

  btnSave := TwgButton.Create(self);
  with btnSave do
  begin
    SetDimensions(256,20,73,24);
    Text := u8('Mentés');
    OnClick := {$ifdef FPC}@{$endif}SaveClick;
  end;

  btnDelete := TwgButton.Create(self);
  with btnDelete do
  begin
    SetDimensions(108,220,93,24);
    Text := u8('Törlés');
    OnClick := {$ifdef FPC}@{$endif}DeleteClick;
  end;

  btnClose := TwgButton.Create(self);
  with btnClose do
  begin
    SetDimensions(244,220,89,24);
    Text := u8('Bezár');
    OnClick := {$ifdef FPC}@{$endif}CloseClick;
  end;

  {@VFD_BODY_END: frmEditPersons}

  qres := nil;
  ReQuery;

end;


{ TFormMain }

procedure TFormMain.AfterCreate;
begin
  inherited;

  {@VFD_BODY_BEGIN: FormMain}
  SetDimensions(274,83,781,466);
  WindowTitle8 := 'TODO list';

  btnNew := TwgButton.Create(self);
  with btnNew do
  begin
    SetDimensions(8,4,96,24);
    Text := u8('Új');
    OnClick := {$ifdef FPC}@{$endif}btnNewClick;
  end;

  btnEdit := TwgButton.Create(self);
  with btnEdit do
  begin
    SetDimensions(112,4,96,24);
    Text := u8('Szerkesztés');
    OnClick := {$ifdef FPC}@{$endif}btnEditClick;
  end;

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(240,4,53,16);
    Text := u8('Sorrend:');
  end;

  chlSorrend := TwgChoiceList.Create(self);
  with chlSorrend do
  begin
    SetDimensions(240,20,150,22);
    Items.Add(u8('Sorszám'));
    Items.Add(u8('Prioritás, Téma'));
    Items.Add(u8('Felel^337s, Prioritás'));
    FocusItem := 2;
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(408,4,48,16);
    Text := u8('Felel^337s:');
  end;

  chlFelelos := TwgChoiceList.Create(self);
  with chlFelelos do
  begin
    SetDimensions(408,20,118,22);
    Items.Add(u8('*'));
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  cbMegoldott := TwgCheckBox.Create(self);
  with cbMegoldott do
  begin
    SetDimensions(548,20,82,20);
    Text := u8('Megoldott');
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  cbRejtett := TwgCheckBox.Create(self);
  with cbRejtett do
  begin
    SetDimensions(644,20,82,20);
    Text := u8('Rejtett');
    OnChange := {$ifdef FPC}@{$endif}FilterChange;
  end;

  grid := TwgDBGrid.Create(self);
  with grid do
  begin
    SetDimensions(2,47,777,262);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn8('Jel','jel',34,alLeft);
    AddColumn8('Sorsz.','sorszam',40,alRight);
    AddColumn8('Pr.','prioritas',25,alCenter);
    AddColumn8('Témaszám','temaszam',70,alLeft);
    AddColumn8('Megnevezés','megnev',360,alLeft);
    AddColumn8('Felel^337s','felelos',56,alLeft);
    AddColumn8('Jelzés','jelzes',70,alLeft);
    AddColumn8('Határid^337','hatarido',70,alLeft);
    RowSelect := true;
    OnRowChange := {$ifdef FPC}@{$endif}gridRowChange;
    OnDoubleClick := {$ifdef FPC}@{$endif}gridDoubleClick;
    OnDrawCell := {$ifdef FPC}@{$endif}gridDrawCell;
  end;

  memo := TwgMemo.Create(self);
  with memo do
  begin
    SetDimensions(2,314,777,150);
    Anchors := [anLeft,anRight,anBottom];
    Lines.Add(u8('memo'));
  end;

  {@VFD_BODY_END: FormMain}

  with chlFelelos do
  begin
    dbconn.PopulateStringList16('select kod from szemely order by kod',Items,true,false);
    Items.Insert(0,u8('*'));
    FocusItem := 1;
  end;

end;

procedure TFormMain.btnEditClick(sender: TObject);
begin
  writeln('Edit');
  EditTodo(grid.FocusRow);
end;

procedure TFormMain.btnNewClick(sender: TObject);
begin
  EditTodo(-1);
end;

procedure TFormMain.EditTodo(row: integer);
var
  uis : TDBUIS;
  id  : integer;
begin
  frmEdit := TFormTodoEdit.Create(nil);
  with frmEdit do
  begin
    id := 0;

    if row > 0 then
    begin
      sq.RecNo := row;
      id := sq.Field('SORSZAM').asInteger;
      edTemaszam.Text := u8noesc(sq.GetFieldS('TEMASZAM'));
      edMegnev.Text := u8noesc(sq.GetFieldS('MEGNEV'));
      chlPrioritas.FocusItem := sq.GetFieldI('PRIORITAS')+1;
      edHatarido.Text8 := sq.GetFieldS('HATARIDO');
      chlFelelos.FocusItem := chlFelelos.Items.IndexOf(u8noesc(sq.GetFieldS('FELELOS')))+1;
      edJelzes.Text := u8noesc(sq.GetFieldS('JELZES'));
      memo.Text := u8noesc(sq.GetFieldS('MEGJ'));
      cbMegoldva.Checked := pos(sq.GetFieldS('MEGOLDVA'),'TYI') > 0;
      cbRejtett.Checked := pos(sq.GetFieldS('REJTETT'),'TYI') > 0;
    end;

    if ShowModal = 1 then
    begin
      writeln('mentes');
      uis := TDBUIS.Create('feladat',dbconn,false);

      uis.SetFieldS('TEMASZAM',u16u8trunc(edTemaszam.Text));
      uis.SetFieldS('MEGNEV',u16u8trunc(edMegNev.Text));
      uis.SetFieldS('HATARIDO',edHatarido.Text8);
      uis.SetFieldS('FELELOS',u16u8trunc(chlFelelos.Text));
      uis.SetFieldS('JELZES',u16u8trunc(edJelzes.Text));
      uis.SetFieldS('MEGJ',u16u8trunc(memo.Text));
      uis.SetFieldI('PRIORITAS',chlPrioritas.FocusItem - 1);

      uis.SetFieldS('MEGOLDVA',iifs(cbMegoldva.Checked,'T','F'));
      uis.SetFieldS('REJTETT',iifs(cbRejtett.Checked,'T','F'));

      if row > 0 then uis.ExecUpdate('WHERE SORSZAM='+IntToStr(id))
                 else uis.ExecInsert;

      uis.Free;
    end;
    Free;
  end;
  ReQuery;
end;

procedure TFormMain.FilterChange(sender: TObject);
begin
  ReQuery;
end;

procedure TFormMain.gridRowChange(sender: TObject; row: integer);
begin
  //writeln('Row changed: ',row);

  if row > 0 then sq.RecNo := row;

  if not sq.Eof then memo.Text := u8noesc(sq.GetFieldS('megj'))
                else memo.Text := '';
end;

procedure TFormMain.ReQuery;
var
  ri : integer;
  s : string;
  s2 : string;
begin
  ri := grid.FocusRow;
  grid.SetResultSet(nil,false);
  if sq <> nil then sq.Free;

  s := 'SELECT '
       + 'megnev, sorszam, temaszam, felelos, prioritas, hatarido, datum, jelzes, megj, rejtett, megoldva FROM feladat ';

  s2 := 'WHERE ';

  if chlFelelos.FocusItem > 1 then
  begin
    s := s + s2 + 'felelos = '''+u16u8trunc(chlFelelos.Text)+''' ';
    s2 := ' AND ';
  end;

  if not cbMegoldott.Checked then
  begin
    s := s + s2 + 'megoldva <> ''T'' ';
    s2 := ' AND ';
  end;

  if not cbRejtett.Checked then
  begin
    s := s + s2 + 'rejtett <> ''T'' ';
    s2 := ' AND ';
  end;

  case chlSorrend.FocusItem of
    2 : s := s + 'ORDER BY prioritas, temaszam, sorszam';
    3 : s := s + 'ORDER BY felelos, prioritas, sorszam';
  else
    s := s + 'ORDER BY sorszam';
  end;

//  writeln(s);

  sq := dbconn.RunQuery(s);

  if sq <> nil then sq.FetchAll;
  grid.SetResultSet(sq,false);

  grid.FocusRow := ri;
  grid.FollowFocus;
  grid.RePaint;
end;

procedure TFormMain.gridDoubleClick(Sender: TObject; x, y: integer; var button: word; var shiftstate: word);
begin
  btnEdit.Click;
end;

procedure TFormMain.gridDrawCell(sender: TObject; row, col: integer; rect: TGfxRect; flags: integer; var stddraw : boolean);
begin
  if col = 1 then
  begin
    if sq.GetFieldS('megoldva') = 'T' then
      grid.Canvas.DrawImage(rect.left+1,rect.top+1,imgok);
    if sq.GetFieldS('rejtett') = 'T' then
      grid.Canvas.DrawImage(rect.left+18,rect.top+1,imghidden);
    stddraw := false;
  end;
end;

{ TFormTodoEdit }

procedure TFormTodoEdit.AfterCreate;
begin
  inherited AfterCreate;

  {@VFD_BODY_BEGIN: FormTodoEdit}
  SetDimensions(463,301,438,387);
  WindowTitle8 := 'Szerkesztés';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,8,48,16);
    Text := u8('Téma:');
  end;

  edTemaszam := TwgEdit.Create(self);
  with edTemaszam do
  begin
    SetDimensions(8,28,110,20);
    Text := u8('');
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(8,56,76,16);
    Text := u8('Megnevezés:');
  end;

  edMegnev := TwgEdit.Create(self);
  with edMegnev do
  begin
    SetDimensions(8,76,420,20);
    Anchors := [anLeft,anRight,anTop];
    Text := u8('');
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(8,108,52,16);
    Text := u8('Prioritás:');
  end;

  lbLabel4 := TwgLabel.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(80,108,52,16);
    Text := u8('Határid^337:');
  end;

  lbLabel5 := TwgLabel.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(196,108,48,16);
    Text := u8('Felel^337s:');
  end;

  lbLabel6 := TwgLabel.Create(self);
  with lbLabel6 do
  begin
    SetDimensions(348,108,48,16);
    Text := u8('Jelzés:');
  end;

  chlPrioritas := TwgChoiceList.Create(self);
  with chlPrioritas do
  begin
    SetDimensions(8,128,54,22);
    Items.Add(u8('0'));
    Items.Add(u8('1'));
    Items.Add(u8('2'));
    Items.Add(u8('3'));
    Items.Add(u8('4'));
    Items.Add(u8('5'));
  end;

  edHatarido := TwgEdit.Create(self);
  with edHatarido do
  begin
    SetDimensions(80,128,102,20);
    Text := u8('');
  end;

  chlFelelos := TwgChoiceList.Create(self);
  with chlFelelos do
  begin
    SetDimensions(196,128,106,22);
    Items.Add(u8('*'));
  end;

  btnEditResp := TwgButton.Create(self);
  with btnEditResp do
  begin
    SetDimensions(308,127,25,24);
    Text := u8('...');
    OnClick := {$ifdef FPC}@{$endif}btnEditPersonsClick;
  end;

  edJelzes := TwgEdit.Create(self);
  with edJelzes do
  begin
    SetDimensions(348,128,78,20);
    Text := u8('');
  end;

  lbLabel7 := TwgLabel.Create(self);
  with lbLabel7 do
  begin
    SetDimensions(8,160,50,16);
    Text := u8('Leírás:');
  end;

  memo := TwgMemo.Create(self);
  with memo do
  begin
    SetDimensions(8,180,418,132);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add(u8(''));
  end;

  cbMegoldva := TwgCheckBox.Create(self);
  with cbMegoldva do
  begin
    SetDimensions(8,320,82,20);
    Anchors := [anLeft,anBottom];
    Text := u8('Megoldva');
  end;

  cbRejtett := TwgCheckBox.Create(self);
  with cbRejtett do
  begin
    SetDimensions(132,320,82,20);
    Anchors := [anLeft,anBottom];
    Text := u8('Rejtett');
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,352,105,24);
    Anchors := [anLeft,anBottom];
    Text := u8('OK');
    OnClick := {$ifdef FPC}@{$endif}btnOkClick;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(318,352,105,24);
    Anchors := [anRight,anBottom];
    Text := u8('Mégsem');
    OnClick := {$ifdef FPC}@{$endif}btnCancelClick;
  end;

  {@VFD_BODY_END: FormTodoEdit}

  dbconn.PopulateStringList16('select kod from szemely',chlFelelos.Items,true,false);

end;

procedure TFormTodoEdit.btnOKClick(sender: TObject);
begin
  writeln('OK');
  ModalResult := 1;
  Close;
end;

procedure TFormTodoEdit.btnCancelClick(sender: TObject);
begin
  writeln('Cancel');
  ModalResult := -1;
end;

var
  //sq : TSqlResult;

  psf : TIniFile;

  prgpath, conffile : string;

  dbdrv, dbsvr, dbdb : string;

procedure TFormTodoEdit.btnEditPersonsClick(sender: TObject);
var
  frm : TfrmEditPersons;
begin
  frm := TfrmEditPersons.Create(nil);
  frm.ShowModal;
  frm.Free;
end;

procedure TfrmEditPersons.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditPersons.RowChange(sender : TObject; row : integer);
begin
  if (row > 0) and (row <= grid.RowCount) then
  begin
    edKOD.Text8 := grid.FocusField('KOD').AsString;
    edNEV.Text8 := grid.FocusField('NEV').AsString;
  end;
end;

procedure TfrmEditPersons.DeleteClick(Sender: TObject);
var
  frm : TfrmAskDelete;
begin
  frm := TfrmAskDelete.Create(nil);
  if frm.ShowModal = 1 then
  begin
    dbconn.ExecStatement('delete from szemely where KOD='+QuotedStr(grid.FocusField('KOD').asString));
    ReQuery;
  end;
  frm.Free;
end;

procedure TfrmEditPersons.SaveClick(Sender: TObject);
var
  uis : Tdbuis;
  qr : TSqlResult;
  newp : boolean;
begin
  uis := Tdbuis.Create('szemely',dbconn,false);

  qr := dbconn.RunQuery('select * from szemely where kod='''+edKOD.Text8+'''');
  newp := true;
  if qr <> nil then
  begin
    if not qr.Eof then newp := false;
    qr.Free;
  end;

  uis.SetFieldS('KOD',edKOD.Text8);
  uis.SetFieldS('NEV',edNEV.Text8);

  if newp then uis.ExecInsert else uis.ExecUpdate('where kod='''+edKOD.Text8+'''');

  uis.Free;

  ReQuery;
end;

{$I bmp_ok.inc}
{$I bmp_hidden.inc}

//var
//  sq : TSqlResult;

begin
  Writeln('LPTK todo');

  prgpath := ExtractFilePath(paramstr(0));

  SetCurrentDir(prgpath);  // The Delphi IDE not sets the execution directory!

  conffile := prgpath+'app_todo.conf';

  Writeln('Reading configuration file: ',conffile);
  psf := TIniFile.Create(conffile);

  if not FileExists(conffile) then
  begin
    psf.WriteString('database','title','TODO list');
    psf.WriteString('database','driver','MySQL');
    psf.WriteString('database','server','127.0.0.1');
    psf.WriteString('database','database','feladat');
    psf.WriteString('database','username','root');
    psf.WriteString('database','password','');
    psf.UpdateFile;
  end;

  dbdrv := psf.ReadString('database','driver','?');
  dbsvr := psf.ReadString('database','server','?');
  dbdb  := psf.ReadString('database','database','?');

  Writeln(' driver=',dbdrv,' server=',dbsvr,' database=',dbdb);

  dbconn := TSqlDBConnection.Create;
//  dbconn.ConnectByDriver('MySQL','192.168.1.1','feladat','root','','');
  dbconn.ConnectByDriver(dbdrv,dbsvr,dbdb,
    psf.ReadString('database','username','?'),
    psf.ReadString('database','password','?'), ''
                        );
  if not dbconn.Connected then
  begin
    Writeln;
    write('Error connecting the database. Exiting...');
    readln;
    Halt(1);
  end;
{
  sq := dbconn.RunQuery('select sorszam, megnev from feladat');
  if sq <> nil then
  begin
    sq.FetchAll;
    sq.First;
    while not sq.Eof do
    begin
      writeln(sq.GetFieldI('SORSZAM'),' = ',sq.GetFieldS('MEGNEV'));
      sq.Next;
    end;
    sq.Free;
  end;

  writeln('finished.');
  readln;
  halt;
}
  GfxOpenDisplay('');

  imgok := CreateBMPImage(@bmp_ok, sizeof(bmp_ok));
  imgok.CreateMaskFromSample(0,0);
  imghidden := CreateBMPImage(@bmp_hidden, sizeof(bmp_hidden));
  imghidden.CreateMaskFromSample(0,0);

  frmMain := TFormMain.Create(nil);
  frmMain.WindowTitle8 := psf.ReadString('database','title','TODO');
  frmMain.ReQuery;
  frmMain.Show;

  repeat
    try
      GfxDoMessageLoop;
      break;
    except
      on e : Exception do ShowMessage8(e.message,'Exception');
    end;
  until false;

  GfxCloseDisplay;
end.

