{ wgdbgrid.pas: DataGrid widget, displays SQL results
  File maintainer: nvitya@freemail.hu

History:
}
unit wgdbgrid;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget, wgscrollbar, wggrid, sqldb,
  gfxclipboard;

type

  TGridDrawNotifyEvent = procedure(Sender: TObject; row,col : integer; rect : TGfxRect;
                            flags : integer; var stddraw : boolean) of object;

  TDBColumn = class
  public
    Width : integer;
    Title : string16;
    FieldName8 : string;
    FieldIndex : integer;
    Alignment : TAlignment;
  end;

  TwgDBGrid = class(TwgGrid)
  private
    function GetColumns(index: integer): TDBColumn;
  protected

    FColumns : TList;
    FResultSet : TSqlResult;

    function GetColumnWidth(col : integer) : TGfxCoord; override;
    procedure SetColumnWidth(col : integer; cwidth : TgfxCoord); override;
  public

    MaxColWidth : integer;
    MinColWidth : integer;

    function GetColumnCount : integer; override;
    function GetRowCount : integer; override;


    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;
    procedure DrawHeader(col : integer; rect : TGfxRect; flags : integer); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  public

    procedure SetResultSet(const AValue: TSqlResult; AutoSetUp : boolean);

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function AddColumn(ATitle : string16; AFieldName8 : string; Awidth : integer; Align : TAlignment) : TDBColumn;
    function AddColumn8(ATitle : string; AFieldName8 : string; Awidth : integer; Align : TAlignment) : TDBColumn;
    procedure FreeColumns;

    procedure DeleteColumn(index : integer);
    procedure MoveColumn(oldindex, newindex : integer);

    property Columns[index : integer] : TDBColumn read GetColumns;

    function FocusField(fname : string) : TSqlField;

  public

    OnDrawCell : TGridDrawNotifyEvent;

  end;

implementation

{ TwgDBGrid }

procedure TwgDBGrid.SetResultSet(const AValue: TSqlResult; AutoSetUp : boolean);
var
  n,i : integer;
  f : TSqlField;
  c : TDBColumn;
  hw,cw : integer;
  s : string16;
  al : TAlignment;
begin
  FResultSet:=AValue;

  FFirstRow := 1;
  FFocusCol := 1;
  FFocusRow := 1;

  if AutoSetup then
  begin
    FreeColumns;

    if (FResultSet <> nil) then
      with FResultSet do
      begin
        for n:=1 to FieldCount do
        begin
          f := Field(n);
          if f <> nil then
          begin
            cw := integer(f.FieldSize)*FFont.TextWidth16('0'#0) + 2;
            s := Str8To16(f.FieldName);
            hw := FHeaderFont.TextWidth16(s) + 2;
            if cw < hw then cw := hw;

            if cw > MaxColWidth then cw := MaxColWidth;
            if cw < MinColWidth then cw := MinColWidth;

            al := alLeft;
            if f.FieldType in [ftInteger,ftBigInt,ftDouble,ftCurrency] then al := alRight;

            AddColumn(s,f.FieldName,cw,al);

          end;
        end;
      end;
  end;

  //searching column indexes
  for n := 0 to FColumns.Count-1 do
  begin
    c := TDBColumn(FColumns[n]);
    if (FResultSet <> nil) then i := FResultSet.GetFieldIndex(c.FieldName8) else i := 0;
    c.FieldIndex := i;
  end;

  if WinHandle > 0 then
  begin
    UpdateScrollBar;
    Repaint;
  end;

  if FResultSet <> nil then
  begin
    // create focuschange events
    FPrevCol := 0;
    FPrevRow := 0;
    CheckFocusChange;
  end;

  //Writeln('Columns: ',ColumnCount,' rows: ',RowCount);
end;

procedure TwgDBGrid.FreeColumns;
var
  n : integer;
begin
  for n:=0 to FColumns.Count-1 do TDBColumn(FColumns[n]).Free;
  FColumns.Clear;
end;

procedure TwgDBGrid.DeleteColumn(index: integer);
var
  c : TDBColumn;
begin
  c := Columns[index];
  if c <> nil then
  begin
    c.Free;
    FColumns.Delete(index);
    if FWinHandle > 0 then Update;
  end;
end;

procedure TwgDBGrid.MoveColumn(oldindex, newindex: integer);
begin
  FColumns.Move(oldindex,newindex);
  if FWinHandle > 0 then Update;
end;

function TwgDBGrid.FocusField(fname: string): TSqlField;
begin
  if FResultSet <> nil then
  begin
    FResultSet.RecNo := FocusRow;
    result := FResultSet.Field(fname);
  end
  else result := nil;
end;

function TwgDBGrid.GetColumnCount: integer;
begin
  Result := FColumns.Count;
end;

function TwgDBGrid.GetRowCount: integer;
begin
  if FResultSet <> nil then result := FResultSet.RowCount
                       else result := 0;
end;

function TwgDBGrid.GetColumnWidth(col: integer): TGfxCoord;
begin
  if (col > 0) and (col <= ColumnCount) then
    Result := TDBColumn(FColumns[col-1]).Width
  else
    result := 10;
end;

procedure TwgDBGrid.SetColumnWidth(col : integer; cwidth: TgfxCoord);
begin
  with TDBColumn(FColumns[col-1]) do
  begin
    if width <> cwidth then
    begin
      width := cwidth;
      if width < MinColWidth then width := MinColWidth;
      self.repaint;
    end;
  end;
end;

procedure TwgDBGrid.DrawCell(row, col: integer; rect: TGfxRect; flags: integer);
var
  s16 : string16;
  x, x2, cw : integer;
  c : TDBColumn;
  cont : boolean;
begin
  FResultSet.RecNo := row;

  if Assigned(OnDrawCell) then
  begin
    cont := true;
    OnDrawCell(self, row, col, rect, flags, cont);
    if not cont then Exit;
  end;

  c := TDBColumn(FColumns[col-1]);

  if c.FieldIndex < 1 then Exit;

  s16 := Str8to16(FResultSet.GetFieldS(c.FieldIndex));

  case TDBColumn(FColumns[col-1]).Alignment of
    alRight :  begin
                 cw := FFont.TextWidth16(s16);
                 x := rect.Right - 1 - cw;
               end;

    alCenter : begin
                 cw := FFont.TextWidth16(s16);
                 x2 := (rect.Width div 2) - (cw div 2);
                 if x2 < 1 then x2 := 1;
                 x := rect.Left + x2;
               end;
  else
    x := rect.Left + 1;
  end;
  canvas.DrawString16(x,rect.top+1,  s16 );
end;

procedure TwgDBGrid.DrawHeader(col: integer; rect: TGfxRect; flags: integer);
var
  s : string16;
  x : integer;
begin
  s := TDBColumn(FColumns[col-1]).Title;
  x := (rect.width div 2) - (FHeaderFont.TextWidth16(s) div 2);
  if x < 1 then x := 1;
  canvas.DrawString16(rect.left + x, rect.top+1, s);
end;

constructor TwgDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResultSet := nil;
  FColumns := TList.Create;
  MaxColWidth := 300;
  MinColWidth := 16;
  OnDrawCell := nil;
end;

destructor TwgDBGrid.Destroy;
begin
  FreeColumns;
  FColumns.Free;
  inherited Destroy;
end;

function TwgDBGrid.AddColumn(ATitle : String16; AFieldName8: string;
            Awidth: integer; Align : TAlignment): TDBColumn;
var
  c : TDBColumn;
begin
  c := TDBColumn.Create;
  c.Title := ATitle;
  c.FieldName8 := AFieldName8;
  c.FieldIndex := 0;
  c.Width := AWidth;
  c.Alignment := Align;
  FColumns.Add(c);
  result := c;
  if FWinHandle > 0 then Update;
end;

function TwgDBGrid.AddColumn8(ATitle: string; AFieldName8: string; Awidth: integer; Align : TAlignment): TDBColumn;
begin
  result := AddColumn(u8(ATitle),AFieldName8,AWidth,Align);
end;

function TwgDBGrid.GetColumns(index: integer): TDBColumn;
begin
  if (Index < 0) or (Index > FColumns.Count - 1) then result := nil
  else Result := TDBColumn(FColumns[index]);
end;

procedure TwgDBGrid.HandleKeyPress(var keycode, shiftstate: word; var consumed: boolean);
var
  c : TDBColumn;
begin

  inherited HandleKeyPress(keycode,shiftstate,consumed);

  if not Consumed then
  begin
    if GfxCheckClipboardKey(keycode, shiftstate) = ckCopy then
    begin
      c := Columns[FocusCol-1];
      if c <> nil then
      begin
        if FocusField(c.FieldName8) <> nil then SetClipboardText(FocusField(c.FieldName8).AsString);
        Consumed := true;
      end;  
    end;
  end;

end;

end.

