unit wgstringgrid;

// $Log$
// Revision 1.3  2003/11/10 00:00:41  nvitya
// GetRowCount, RowCount, GetColumnCount, ColumnCount changes
//
// Revision 1.2  2003/11/09 17:24:48  aegluke
// dynamic memory freeing, freeing row/column memory on destroy
//
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
	destructor Destroy; override;
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

	    procedure DrawCell(aRow, aCol : Integer; aRect : TgfxRect; aFlags : integer); override;
	    procedure DrawHeader(aCol : integer; aRect : TgfxRect; aFlags : integer); override;
	    
	    procedure SetColumnTitle(aColumn : integer; aValue : string16);
	    function GetColumnTitle(aColumn : integer) : string16;
	    
	    procedure SetCell8(aColumn, aRow : longword; aValue : string);
	    function GetCell8(aColumn, aRow : Longword) : string;
	public	    	    
	    constructor Create(aOwner : TComponent); override;
	    destructor Destroy; override;
	    property ColumnTitle[aColumn : integer] : string16 read GetColumnTitle write SetColumnTitle;
	    property ColumnWidth[aColumn : integer] : integer read GetColumnWidth write SetColumnWidth;
	    property Cells[aColumn, aRow : Longword] : string16 read GetCell write SetCell;
	    property Cells8[aColumn, aRow : Longword] : string read GetCell8 write SetCell8;
	    property DefaultColumnWidth : TgfxCoord read FDefaultColumnWidth write FDefaultColumnWidth;

	    procedure SetRowCount(aValue : integer);
            property RowCount : integer read GetRowCount write SetRowCount;

	    procedure SetColumnCount(aValue : integer);
            property ColumnCount : integer read GetColumnCount write SetColumnCount;
    end;

implementation

// {$DEFINE DEBUG}

destructor TwgStringGrid.Destroy;
begin
    ColumnCount := 0;
    RowCount := 0;
    inherited Destroy;
end;

function TwgStringGrid.GetCell8(aColumn, aRow : Longword) : string;
begin
    result := Str16To8(Cells[aColumn, aRow]);
end;

procedure TwgStringGrid.SetCell8(aColumn, aRow : Longword; aValue : string);
begin
    Cells[aColumn, aRow] := Str8To16(aValue);
end;

procedure TwgStringGrid.SetColumnTitle(aColumn : integer; aValue : string16);
var
    aCalc : integer;
begin
    aCalc := aColumn - FColumns.Count + 1;
    if aCalc > 0 then
	Cells[aColumn,0] := '';
    if aValue <> TStringColumn(FColumns[aColumn]).Title then
    begin
	if aColumn+1 > FColumnCount then FColumnCount := aColumn + 1;
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
var
    i, i1 : integer;
//    TmpCol : TStringColumn;
    aCalc : integer;
    SL : TStringList;
begin
    if aValue <> FRowCount then
    begin
	if aValue < FRowCount then
	begin
	    // loeschen und freigeben der zeilen
	    for i := 0 to FColumns.Count - 1 do
	    begin
		
		aCalc := TStringColumn(FColumns[i]).Cells.Count - aValue;
		if aCalc > 0 then
		begin
		    sl := TStringColumn(FColumns[i]).Cells;
		    for i1 := 1 to aCalc do
			sl.Delete(sl.Count-1);
		end;
	    end;
	end;
	FRowCount := aValue;
	RePaint;
    end;
end;

procedure TwgStringGrid.SetColumnCount(aValue : integer);
var
    i : integer;
    aCalc : integer;
//    TmpCol : TStringColumn;
begin
    if aValue <> FColumnCount then
    begin
	// freigeben der spalten
	if aValue < FColumnCount then
	begin
	    aCalc := FColumns.Count - aValue;
	    if aCalc > 0 then
	    begin
		for i := 1 to aCalc do
		begin
		    TStringColumn(FColumns[i]).Destroy;
		    FColumns.Delete(FColumns.Count-1);
		end;
	    end;
	end;
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
var
    diff : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetCell:',aColumn,',',aRow);
    {$ENDIF}
    if aColumn > FColumns.Count - 1 then
	result := ''
    else
    begin
	diff := (TStringColumn(FColumns[aColumn]).Cells.Count - 1) - integer(aRow);
	if diff < 0 then
	    result := ''
	else
	begin
	    result := TStringColumn(FColumns[aColumn]).Cells[aRow];
	end;
    end;
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

destructor TStringColumn.Destroy;
begin
    Cells.Destroy;
end;

end.
