program stringgridtest;

// $Log: stringgridtest.pas,v $
// Revision 1.1  2012/09/16 16:36:03  nvitya
// bjorn changes
//
// Revision 1.1  2003/11/09 15:18:28  aegluke
// Initial release
//

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, pgf_defs, pgf_main, pgf_form, wggrids;

type
    TMainForm = class(TpgfForm)
      public
	Grid : TwgStringGrid;
	procedure AfterCreate; override;
    end;

var
    MainForm : TpgfForm;

procedure TMainForm.AfterCreate;
var
    i : integer;
begin
    inherited;
    SetPosition(500,10,400,400);
    WindowTitle := 'wgStringGrid-Test';

    Grid := TwgStringGrid.Create(self);
    Grid.top := 10;
    Grid.left := 10;
    Grid.height := 360;
    Grid.width := 380;
    Grid.anchors := [anLeft,anBottom, anRight, anTop];
    Grid.ColumnCount := 10;
    Grid.RowCount := 20;
    Grid.Cells[0,0] := u8('String 0,0');
    Grid.Cells[1,1] := u8('String 1,1');    
    for i := 0 to Grid.ColumnCount - 1 do
	Grid.ColumnTitle[i] := u8('Title '+IntToStr(i+1));
end;

begin
    pgfOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    pgfRunMessageLoop;
    pgfCloseDisplay;
end.

