unit testsql;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wgmemo, wgchoicelist, wggrid, sqldb, wgdbgrid, gfxdialogs, sqluis;

procedure test_sql;

implementation

type
  TTestForm = class(TGfxForm)
  public
    grid : TwgDBGrid;

    procedure AfterCreate; override;

  end;

{ TTestForm }

procedure TTestForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowTitle8 := 'DBGrid Test Form';

  SetDimensions(10,10,500,390);

  grid := TwgDBGrid.Create(self);
  grid.SetDimensions(1,1, width-2, height - 2);
  grid.Anchors := AllAnchors;

end;

procedure InsertTest;
var
  dbconn : TSqlDBConnection;
  uis : Tdbuis;
  n : integer;
begin
  dbconn := TSqlDBConnection.Create;
  if not dbconn.ConnectByDriver('MySQL','192.168.1.1','test','root','','') then
  begin
    writeln('SQL connection failed!');
    dbconn.Free;
    Exit;
  end;

  uis := Tdbuis.Create('test',dbconn,false);

  uis.ExecSQL('truncate test');

  for n:=1 to 1000 do
  begin
    uis.SetFieldI('ID',n);
    uis.SetFieldS('NAME','Name'+IntToStr(n));
    uis.ExecInsert;
  end;

  uis.Free;
  dbconn.Free;
end;

procedure test_sql;
var
  testform : TTestForm;
  dbconn : TSqlDBConnection;
  qr : TSqlResult;
begin
  writeln('doing sql test...');

  InsertTest;

  dbconn := TSqlDBConnection.Create;
  if not dbconn.ConnectByDriver('MySQL','192.168.1.1','test','root','','') then
//  if not dbconn.ConnectByDriver('MySQL','127.0.0.1','honfoglalo','root','','') then
//  if not dbconn.ConnectByDriver('PostgreSQL','192.168.1.1','szamla','root','','') then
  begin
    writeln('SQL connection failed!');
    dbconn.Free;
    Exit;
  end;

  writeln('connection ok.');

  qr := dbconn.RunQuery('select * from test'); // order by szamlasz');
  if qr <> nil then
  begin
    writeln('query ok.');
    qr.FetchAll;

    testform := TTestForm.Create(nil);
    testform.grid.SetResultSet(qr,true);
    testform.ShowModal;
    testform.Free;

{
    qr.First;
    while not qr.Eof do
    begin
      Writeln('OSSZEG =',qr.GetField('OSSZEG'),' o2=',qr.GetField('OSSZEG') );
      qr.Next;
    end;
}
    qr.Free;
  end
  else
  begin
    ShowMessage8(dbconn.ErrorMsg8);
  end;

  dbconn.Free;

end;

end.

