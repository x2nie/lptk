program stringgridtest;

// $Log$
// Revision 1.1  2003/11/09 15:18:28  aegluke
// Initial release
//

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys,messagequeue, schar16, gfxbase, gfxstyle, gfxform, wgstringgrid, wggrid;

type
    TMainForm = class(TGfxForm)
      public
	Grid : TwgStringGrid;
	procedure AfterCreate; override;
    end;

var
    MainForm : TGfxForm;

procedure TMainForm.AfterCreate;
var
    i : integer;
begin
    inherited;
    SetDimensions(500,10,400,400);
    WindowTitle8 := 'wgStringGrid-Test';
    Grid := TwgStringGrid.Create(self);
    Grid.top := 100;
    Grid.left := 10;
    Grid.height := 200;
    Grid.width := 200;
    Grid.anchors := [anLeft,anBottom, anRight, anTop];
    Grid.ColumnCount := 10;
    Grid.RowCount := 20;
    Grid.Cells[0,0] := Str8To16('0,0');
    Grid.Cells[1,1] := Str8To16('1,1');    
    for i := 0 to Grid.ColumnCount - 1 do
	Grid.ColumnTitle[i] := Str8To16('Title '+IntToStr(i+1));
end;

begin
    GfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    GfxDoMessageLoop;
    GfxCloseDisplay;
end.

