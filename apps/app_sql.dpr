{ Copyright (c) 2003, Nagy Viktor

 A general SQL data browser utility
}

program app_sql;

{$APPTYPE GUI}
{.$APPTYPE CONSOLE}

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb,
  wgdbgrid, gfxdialogs;

type
  TSqlForm = class(TGfxForm)
  public
    memo : TwgMemo;
    grid : TwgDbGrid;

    conn : TSqlDbConnection;
    qr : TSqlResult;

    DBName : string;

    FDragPos : integer;
    FSplitterDrag : boolean;

    procedure AfterCreate; override;
    destructor Destroy; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    procedure HandleClose; override;

    procedure RunSql;

  end;

  TDBSelForm = class(TGfxForm)
  public
    l0, l1,l2,l3,l4,l5 : TwgLabel;
    chlMode  : TwgChoiceList;
    chlDriver  : TwgChoiceList;
    chlDSN     : TwgChoiceList;
    edServer   : TwgEdit;
    edDataBase : TwgEdit;
    edUser : TwgEdit;
    edPass : TwgEdit;

    btnOk, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure btnOKClick(sender : TObject);
    procedure btnCancelClick(sender : TObject);

    procedure chlModeChange(sender : TObject);

  end;

{ TSqlForm }

procedure TSqlForm.AfterCreate;
begin
  inherited AfterCreate;

  DBName := '';

  WindowTitle8 := 'SQL query';

  SetDimensions(10,10,400,300);
  WindowPosition := wpAuto;

  memo := TwgMemo.Create(self);
  memo.SetDimensions(1,1,width-2,70);
  memo.Anchors := [anLeft,anTop,anRight];

  grid := twgDBGrid.Create(self);
  grid.SetDimensions(1,memo.Height+4,width-2,80);
  grid.Height := Height - 2 - grid.Top;
  grid.Anchors := [anLeft,anTop,anRight,anBottom];

  qr := nil;

  MouseCursor := CUR_DIR_NS;
  
end;

destructor TSqlForm.Destroy;
begin
  if qr <> nil then qr.Free;
  inherited Destroy;
end;

procedure TSqlForm.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  consumed := true;
  case keycode of
    KEY_F9:
           begin
             Writeln('F9 pressed.');
             RunSql;
           end;
    KEY_F1:
           begin // F1
             ShowMessage8('F9: RUN the query'#13'F4: Show field value','Short help');
           end;
    KEY_F4:
           begin // F4
             if qr <> nil then
             begin
               qr.RecNo := grid.FocusRow;
               ShowMessage8(qr.GetFieldS(grid.FocusCol),'Field: '+qr.FieldName(grid.FocusCol));
             end;
           end;
  else
    consumed := false;
  end;

end;

procedure TSqlForm.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseDown(x, y, button, shiftstate);
  if (y >= memo.Height+memo.Top) and (y <= grid.Top) then
  begin
    FSplitterDrag := true;
    FDragPos := y;
    //MouseCursor := CUR_DIR_NS;
  end;
end;

procedure TSqlForm.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  //MouseCursor := CUR_DEFAULT;
  FSplitterDrag := False;
end;

procedure TSqlForm.HandleMouseMove(x, y: integer; btnstate, shiftstate : word);
var
  dy : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if FSplitterDrag then
  begin
    dy := y - FDragPos;

    if (dy < 0) and (memo.Height + dy > 10) or (dy > 0) and (grid.Height - dy > 10) then
    begin
      memo.MoveResizeBy(0,0,0,dy);

      grid.Top := grid.Top + dy;
      grid.MoveResizeBy(0,0,0,-dy);
    end;

    FDragPos := y;
  end;

end;

procedure TSqlForm.HandleClose;
begin
  inherited HandleClose;
  PostKillMe;
end;

procedure TSqlForm.RunSql;
begin
  grid.SetResultSet(nil,true);
  grid.Visible := false;
  WindowTitle8 := 'Running query...';
  GfxFlush;

  if qr <> nil then
  begin
    qr.Free;
  end;
  qr := conn.RunQuery(memo.Text8);

  grid.Visible := true;
  WindowTitle8 := 'Fetching records...';
  GfxFlush;

  if qr <> nil then
  begin
    writeln('fetching...');
    qr.FetchAll;

    WindowTitle8 := IntToStr(qr.RowCount)+' rows ('+DBName+')';

    writeln('displaying...');
    grid.SetResultSet(qr,true);
    //grid.Repaint;

    activewidget := grid;
  end
  else
  begin
    ShowMessage8(conn.ErrorMsg8, 'SQL error: '+IntToStr(conn.LastError));
    WindowTitle8 := 'Error has happened';
  end;
end;

{ TDBSelForm }

procedure TDBSelForm.AfterCreate;
var
  x,y : integer;
  gap : integer;
  dsnlist, drvlist : TStringList;
begin
  inherited AfterCreate;

  WindowTitle8 := 'Select ODBC database';

  SetDimensions(10,10,340,315);
  WindowPosition := wpAuto;

  gap := 50;
  y := 10;
  x := 10;

  l0 := CreateLabel(self, x,y, 'Mode:');
  chlMode := CreateChoiceList(self, 10,y+20, 200, nil);
  chlMode.Items.Add(Str8to16('DataSource Name'));
  chlMode.Items.Add(str8to16('Driver'));
  chlMode.OnChange := {$ifdef FPC}@{$endif}chlModeChange;
  chlMode.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l1 := CreateLabel(self, x,y, 'DSN:       ');
  drvlist := CreateDriverNameList;
  //for n:=0 to drvlist.Count-1 do writeln('drv ',n+1,' = ',drvlist.Strings[n]);
  chlDriver := CreateChoiceList(self, 10,y+20, 200, drvlist);
  chlDriver.Visible := false;
  chlDriver.Anchors := [anLeft,anTop,anRight];
  drvlist.Free;

  dsnlist := CreateDSNList;
  chlDSN := CreateChoiceList(self, 10,y+20, 200, dsnlist);
  chlDSN.Anchors := [anLeft,anTop,anRight];
  dsnlist.Free;

  inc(y,gap);
  l2 := CreateLabel(self, x,y, 'Server:');
  edServer := CreateEdit(self, 10,y+20, 200, 0);
  edServer.Text := Str8to16('127.0.0.1');
  edServer.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l3 := CreateLabel(self, x,y, 'Database:');
  edDatabase := CreateEdit(self, 10,y+20, 200, 0);
  edDatabase.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l4 := CreateLabel(self, x,y, 'Username:');
  edUser := CreateEdit(self, 10,y+20, 200, 0);
  edUser.Text := Str8to16('root');
  edUser.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l5 := CreateLabel(self, x,y, 'Password:');
  edPass := CreateEdit(self, 10,y+20, 200, 0);
  edPass.PasswordMode := true;
  edPass.Anchors := [anLeft,anTop,anRight];

  y := 30;
  x := 230;

  btnOK := CreateButton(self, x,y,90, 'Connect', {$ifdef FPC}@{$endif}btnOkClick);
  btnOK.Anchors := [anTop,anRight];
  inc(y,30);
  btnCancel := CreateButton(self, x,y,90, 'Cancel', {$ifdef FPC}@{$endif}btnCancelClick);
  btnCancel.Anchors := [anTop,anRight];

  chlModeChange(self);

  ActiveWidget := chlDSN;

end;

procedure TDBSelForm.btnOKClick(sender: TObject);
var
  f : TSqlForm;
  s : string;
begin

  f := TSqlForm.Create(nil);
  f.conn := TSqlDBConnection.Create;

  if chlMode.FocusItem = 2 then
  begin
    s := edDatabase.Text8;
    f.conn.ConnectByDriver( chlDriver.Text8, edServer.Text8, s, edUser.Text8, edPass.Text8, '' )
  end
  else
  begin
    s := chlDSN.Text8;
    f.conn.ConnectByDSN( s, edUser.Text8, edPass.Text8 );
  end;

  if f.conn.Connected then
  begin
    f.DBName := s;
    f.Show;
  end
  else
  begin
    ShowMessage8(f.conn.ErrorMsg8,'Connect error: '+IntToStr(f.conn.LastError));
    f.Free;
  end;
end;

procedure TDBSelForm.btnCancelClick(sender: TObject);
begin
  halt(0);
end;

procedure TDBSelForm.chlModeChange(sender: TObject);
begin
  if chlMode.FocusItem = 2 then
  begin
    l1.Text := str8to16('Driver:');
  end
  else
  begin
    l1.Text := str8to16('DSN:');
  end;

  chlDriver.Visible := (chlMode.FocusItem = 2);
  l2.Visible := chlDriver.Visible;
  edServer.Visible := chlDriver.Visible;
  l3.Visible := chlDriver.Visible;
  edDatabase.Visible := chlDriver.Visible;
  chlDSN.Visible := not chlDriver.Visible;

end;

var
  dbselform : TDBSelForm;

begin
  GfxOpenDisplay('');

  Writeln('LPTK SQL utility');

  dbselform := TDBSelForm.Create(nil);
  dbselform.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;
end.

