unit wgstringgrid;

// $Log$
// Revision 1.1  2003/11/09 15:59:17  aegluke
// first release without freeing memory after destroy
//

{$IFDEF FPC}
    {$mode objfpc}
{$ENDIF}

interface

uses
    gfxbase, gfxstyle, wggrid, classes, schar16;

type
    TStringColumn = class
	Cells : TStringList;
	width : TgfxCoord;
	Title : String16;
	constructor Create;
    end;
        
    TwgStringGrid = class(TwgGrid)
	private
	    FColumns : TList;
	    FDefaultColumnWidth : TgfxCoord;
	    FRowCount : integer;
	    FColumnCount : integer;
	    function GetCell(aColumn, aRow : Longword) : String16;
	    procedure SetCell(aColumn, aRow : Longword; aValue : String16);
	protected
	    function GetColumnWidth(aCol : Integer) : integer; override;	    
	    procedure SetColumnWidth(aCol : Integer; aWidth : Integer); override;
	    function GetColumnCount : integer; override;
	    function GetRowCount : integer; override;	    
	    procedure SetColumnCount(aValue : integer); override;
	    procedure SetRowCount(aValue : integer); override;
	    procedure DrawCell(aRow, aCol : Integer; aRect : TgfxRect; aFlags : integer); override;
	    procedure DrawHeader(aCol : integer; aRect : TgfxRect; aFlags : integer); override;
	    
	    procedure SetColumnTitle(aColumn : integer; aValue : string16);
	    function GetColumnTitle(aColumn : integer) : string16;
	public	    	    
	    constructor Create(aOwner : TComponent); override;
	    property ColumnTitle[aColumn : integer] : string16 read GetColumnTitle write SetColumnTitle;
	    property ColumnWidth[aColumn : integer] : integer read GetColumnWidth write SetColumnWidth;
	    property Cells[aColumn, aRow : Longword] : string16 read GetCell write SetCell;
	    property DefaultColumnWidth : TgfxCoord read FDefaultColumnWidth write FDefaultColumnWidth;
    end;

implementation

// {$DEFINE DEBUG}

procedure TwgStringGrid.SetColumnTitle(aColumn : integer; aValue : string16);
var
    aCalc : integer;
begin
    aCalc := aColumn - FColumns.Count + 1;
    if aCalc > 0 then
	Cells[aColumn,0] := '';
    if aValue <> TStringColumn(FColumns[aColumn]).Title then
    begin
	TStringColumn(FColumns[aColumn]).Title := aValue;
	RePaint;
    end;
end;

function TwgStringGrid.GetColumnTitle(aColumn : integer) : string16;
begin
    if FColumns.Count - 1 < aColumn then result := ''
    else result := TStringColumn(FColumns[aColumn]).Title;
end;

procedure TwgStringGrid.SetRowCount(aValue : integer);
begin
    if aValue <> FRowCount then
    begin
	FRowCount := aValue;
	RePaint;
    end;
end;

procedure TwgStringGrid.SetColumnCount(aValue : integer);
begin
    if aValue <> FColumnCount then
    begin
	FColumnCount := aValue;
	RePaint;
    end;
end;

procedure TwgStringGrid.DrawHeader(aCol : integer; aRect : TgfxRect; aFlags : integer);
var
    aCell : String16;
begin
    {$IFDEF DEBUG}
    writeln('DrawHeader');
    {$ENDIF}
    aCell := ColumnTitle[aCol-1];
    Canvas.DrawString16(aRect.Left + aRect.Width div 2 - HeaderFont.TextWidth16(aCell) div 2, aRect.top + 1, aCell);
end;

procedure TwgStringGrid.SetColumnWidth(aCol : Integer; aWidth : Integer);
var
    aCalc : integer;
    i : integer;    
    TmpCol : TStringColumn;
begin
    {$IFDEF DEBUG}
    writeln('SetColumnWidth');
    {$ENDIF}
    aCalc := aCol - FColumns.Count;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	begin
	    TmpCol := TStringColumn.Create;
	    TmpCol.Width := DefaultColumnWidth;
	    TmpCol.Cells := TStringList.Create;
	    FColumns.Add(TmpCol);
	end;
    end;
    if TStringColumn(FColumns[aCol]).Width <> aWidth then
    begin
	TStringColumn(FColumns[aCol]).Width := aWidth;
	RePaint;
    end;
end;

function TwgStringGrid.GetColumnWidth(aCol : Integer) : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetColumnWidth');
    {$ENDIF}
    if aCol > FColumns.Count - 1 then result := DefaultColumnWidth
    else result := TStringColumn(FColumns[aCol]).Width;
end;

function TwgStringGrid.GetColumnCount : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetColumnCount');
    {$ENDIF}
    result := FColumnCount;
end;

function TwgStringGrid.GetRowCount : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetRowCount: ',FRowCount);
    {$ENDIF}
    result := FRowCount;
end;

procedure TwgStringGrid.DrawCell(aRow, aCol : Integer; aRect : TgfxRect; aFlags : integer);
begin
    {$IFDEF DEBUG}
    writeln('DrawCell:',aRow,',',aCol,': ',Cells[aCol-1,aRow-1]);
    {$ENDIF}
    Canvas.DrawString16(aRect.Left + 1, aRect.top + 1, Cells[aCol-1, aRow-1]);
end;

function TwgStringGrid.GetCell(aColumn, aRow : Longword) : String16;
begin
    {$IFDEF DEBUG}
    writeln('GetCell:',aColumn,',',aRow);
    {$ENDIF}
    if aColumn > FColumns.Count - 1 then
	result := ''
    else
	if aRow > TStringColumn(FColumns[aColumn]).Cells.Count - 1 then
	    result := ''
	else
	    result := TStringColumn(FColumns[aColumn]).Cells[aRow];
end;

procedure TwgStringGrid.SetCell(aColumn, aRow : Longword; aValue : String16);
var
    aCalc : integer;
    TmpCol : TStringColumn;
    i : Longword;
begin
    {$IFDEF DEBUG}
    writeln('SetCell:',aColumn,',',aRow);
    {$ENDIF}    
    aCalc := aColumn - FColumns.Count + 1;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	begin
	    TmpCol := TStringColumn.Create;
	    TmpCol.Width := DefaultColumnWidth;
	    FColumns.Add(TmpCol);
	end;
    end;
    aCalc := aRow - TStringColumn(FColumns[aColumn]).Cells.Count + 1;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	    TStringColumn(FColumns[aColumn]).Cells.Append('');
    end;
    TStringColumn(FColumns[aColumn]).Cells[aRow] := aValue;    
    if aColumn > FColumnCount - 1 then FColumnCount := aColumn + 1;
    if aRow > FRowCount - 1 then FRowCount := aRow + 1;
end;

constructor TwgStringGrid.Create(aOwner : TComponent); 
begin
    {$IFDEF DEBUG}
    writeln('Create');
    {$ENDIF}
    inherited Create(aOwner);
    FColumns := TList.Create;
    DefaultColumnWidth := 100;
    ColumnCount := 10;
    RowCount := 10;
end;

// TStringColumn

constructor TStringColumn.Create;
begin
    Cells := TStringList.Create;
    Width := 100;
end;

end.