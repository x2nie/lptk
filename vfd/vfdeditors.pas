{ Copyright (c) 2003, Nagy Viktor

 Some property editors
}

unit vfdeditors;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox, vfdforms;

type
  TItemEditorForm = class(TVFDDialog)
  public
    l1 : TwgLabel;

    edItems : TwgMemo;

    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);

  end;

  TColumnsGrid = class(TwgGrid)
  protected
    function GetColumnWidth(col : integer) : TgfxCoord; override;
  public
    function ColumnCount : integer; override;
    function RowCount : integer; override;

    //procedure ResizeCol(col, cwidth : integer); override;

    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;
    procedure DrawHeader(col : integer; rect : TGfxRect; flags : integer); override;

  public
    dbgrid : TwgDBGrid;

    constructor Create(AOwner : TComponent); override;

  end;

  TColumnEditForm = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: ColumnEditForm}
    lbLabel1 : TWGLABEL;
    grid : TColumnsGrid;
    lbLabel2 : TWGLABEL;
    lbLabel3 : TWGLABEL;
    lbLabel4 : TWGLABEL;
    lbLabel5 : TWGLABEL;
    lbLabel6 : TWGLABEL;
    lbCOLNO : TWGLABEL;
    edTITLE : TWGEDIT;
    edFIELDNAME : TWGEDIT;
    edCOLWIDTH : TWGEDIT;
    chlALIGN : TWGCHOICELIST;
    btnNew : TWGBUTTON;
    btnDelete : TWGBUTTON;
    btnUP : TWGBUTTON;
    btnDOWN : TWGBUTTON;
    btnClose : TWGBUTTON;
    {@VFD_HEAD_END: ColumnEditForm}

    dbgrid : TwgDBGrid;

    procedure AfterCreate; override;

    procedure GridRowChange(Sender : TObject; row : integer);

    procedure SaveColumn(row : integer);

    procedure EditChange(Sender : TObject);

    procedure NewButtonClick(Sender : TObject);

    procedure DeleteButtonClick(Sender : TObject);

    procedure CloseButtonClick(Sender : TObject);

    procedure UpDownButtonClick(Sender : TObject);

  end;

procedure EditDBGridColumns(agrid : TwgDBGRid);

implementation

uses sqldb;

procedure EditDBGridColumns(agrid : TwgDBGRid);
var
  frm : TColumnEditForm;
begin
  frm := TColumnEditForm.Create(nil);
{
  if agrid.ColumnCount < 1 then
  begin
    agrid.AddColumn8('Title','FIELD',80,ftString);
    agrid.AddColumn8('Titlexx','FIELD2',50,ftInteger);
  end;
}
  frm.dbgrid := agrid;
  frm.grid.dbgrid := agrid;
  frm.ShowModal;
  frm.Free;
end;

{ TItemEditorForm }

procedure TItemEditorForm.AfterCreate;
begin
  inherited;
  WindowPosition := wpUser;

  WindowTitle8 := 'Items';

  SetDimensions(0,0,360,230);

  l1 := CreateLabel(self, 8,4, 'Items:');

  edItems := TwgMemo.Create(self);
  with edItems do
  begin
    SetDimensions(8,24,344,168);
    Anchors := [anLeft,anTop,anRight,anBottom];
  end;

  btnOK := CreateButton(self,8,200,105, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnOK.Anchors := [anLeft, anBottom];
  btnCancel := CreateButton(self,244,200,105, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel.Anchors := [anRight, anBottom];

end;

procedure TItemEditorForm.OnButtonClick(sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

{ TColumnEditForm }

procedure TColumnEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: ColumnEditForm}
  SetDimensions(281,137,511,269);
  WindowTitle8 := 'Column editor';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,4,110,16);
    Text := u8('DB Grid columns:');
  end;

  grid := TColumnsGrid.Create(self);
  with grid do
  begin
    SetDimensions(8,24,328,204);
    OnRowChange := {$ifdef FPC}@{$endif}GridRowChange;
  end;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(344,24,56,16);
    Text := u8('Column:');
  end;

  lbLabel3 := TWGLABEL.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(344,56,34,16);
    Text := u8('Title:');
  end;

  lbLabel4 := TWGLABEL.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(344,100,75,16);
    Text := u8('Field Name:');
  end;

  lbLabel5 := TWGLABEL.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(344,188,83,16);
    Text := u8('Alignment:');
  end;

  lbLabel6 := TWGLABEL.Create(self);
  with lbLabel6 do
  begin
    SetDimensions(344,144,88,16);
    Text := u8('Column width:');
  end;

  lbCOLNO := TWGLABEL.Create(self);
  with lbCOLNO do
  begin
    SetDimensions(404,24,54,16);
    Text := u8('1');
  end;

  edTITLE := TWGEDIT.Create(self);
  with edTITLE do
  begin
    SetDimensions(344,76,160,20);
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  edFIELDNAME := TWGEDIT.Create(self);
  with edFIELDNAME do
  begin
    SetDimensions(344,120,160,20);
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  edCOLWIDTH := TWGEDIT.Create(self);
  with edCOLWIDTH do
  begin
    SetDimensions(344,160,160,20);
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  chlALIGN := TWGCHOICELIST.Create(self);
  with chlALIGN do
  begin
    SetDimensions(344,204,160,22);
    Items.Add(u8('Left'));
    Items.Add(u8('Right'));
    Items.Add(u8('Center'));
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  btnNew := TWGBUTTON.Create(self);
  with btnNew do
  begin
    SetDimensions(8,236,60,24);
    Text := u8('New');
    OnClick := {$ifdef FPC}@{$endif}NewButtonClick;
  end;

  btnDelete := TWGBUTTON.Create(self);
  with btnDelete do
  begin
    SetDimensions(76,236,60,24);
    Text := u8('Delete');
    OnClick := {$ifdef FPC}@{$endif}DeleteButtonClick;
  end;

  btnUP := TWGBUTTON.Create(self);
  with btnUP do
  begin
    SetDimensions(152,236,60,24);
    Text := u8('UP');
    OnClick := {$ifdef FPC}@{$endif}UpDownButtonClick;
  end;

  btnDOWN := TWGBUTTON.Create(self);
  with btnDOWN do
  begin
    SetDimensions(216,236,60,24);
    Text := u8('DOWN');
    OnClick := {$ifdef FPC}@{$endif}UpDownButtonClick;
  end;

  btnClose := TWGBUTTON.Create(self);
  with btnClose do
  begin
    SetDimensions(444,236,60,24);
    Text := u8('Close');
    OnClick := {$ifdef FPC}@{$endif}CloseButtonClick;
  end;

  {@VFD_BODY_END: ColumnEditForm}
end;

procedure TColumnEditForm.GridRowChange(Sender: TObject; row: integer);
var
  i : integer;
  c : TDBColumn;
begin
  c := dbgrid.Columns[row-1];
  if c=nil then Exit;

  lbCOLNO.Text8 := IntToStr(row);
  edTITLE.Text := c.Title;
  edFIELDNAME.Text8 := c.FieldName8;
  edCOLWIDTH.Text8 := IntToStr(c.width);
  case c.Alignment of
  alRight  : i := 2;
  alCenter : i := 3
  else
    i := 1;
  end;

  chlALIGN.FocusItem := i;

end;

procedure TColumnEditForm.SaveColumn(row : integer);
var
  c : TDBColumn;
begin
  c := dbgrid.Columns[row-1];
  if c=nil then Exit;

  c.Title := edTITLE.Text;
  c.FieldName8 := edFIELDNAME.Text8;
  c.width := StrToIntDef(edCOLWIDTH.Text8,30);
  case chlALIGN.FocusItem of
  2 : c.Alignment := alRight;
  3 : c.Alignment := alCenter;
  else
    c.Alignment := alLeft;
  end;

  grid.RePaint;
  dbgrid.Update;

end;

procedure TColumnEditForm.EditChange(Sender: TObject);
begin
  if grid.FocusRow < 1 then Exit;

  SaveColumn(grid.FocusRow);
end;

procedure TColumnEditForm.NewButtonClick(Sender: TObject);
begin
  dbgrid.AddColumn8('New','',50,alLeft);
  grid.FocusRow := grid.RowCount;
  grid.Update;
  GridRowChange(sender, grid.FocusRow);
end;

procedure TColumnEditForm.DeleteButtonClick(Sender: TObject);
begin
  dbgrid.DeleteColumn(grid.FocusRow-1);
  grid.Update;
end;

procedure TColumnEditForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TColumnEditForm.UpDownButtonClick(Sender: TObject);
begin
  if Sender = btnUP then
  begin
    if grid.FocusRow > 1 then
    begin
      dbgrid.MoveColumn(grid.FocusRow-1, grid.FocusRow-2);
      grid.FocusRow := grid.FocusRow - 1;
      grid.Update;
    end;
  end
  else
  begin
    if grid.FocusRow < grid.RowCount then
    begin
      dbgrid.MoveColumn(grid.FocusRow-1, grid.FocusRow);
      grid.FocusRow := grid.FocusRow + 1;
      grid.Update;
    end;
  end;
end;


{ TColumnsGrid }

function TColumnsGrid.ColumnCount: integer;
begin
  Result:=5;
end;

function TColumnsGrid.RowCount: integer;
begin
  Result := dbgrid.ColumnCount;
end;

function TColumnsGrid.GetColumnWidth(col: integer): TGfxCoord;
begin
  case col of
    1 : result := 30;
    2 : result := 80;
    3 : result := 80;
    4 : result := 40;
    5 : result := 50;
  else
    result := 80;
  end;
end;

procedure TColumnsGrid.DrawCell(row, col: integer; rect: TGfxRect; flags: integer);
var
  s : string;
  x : integer;
  c : TDBColumn;
begin
  c := dbgrid.Columns[row-1];
  if c=nil then Exit;

  x := rect.Left+1;

  case col of
    1: s := u8(IntToStr(row));
    2: s := c.Title;
    3: s := u8(c.FieldName8);
    4: s := u8(IntToStr(c.Width));
    5: begin
         case c.Alignment of
         alRight: s:=u8('Right');
         alCenter: s:=u8('Center');
         else
           s := u8('Left');
         end;
       end
  else
    s := u8('?');
  end;

  canvas.DrawString16(x, rect.top+1,  s );
end;

procedure TColumnsGrid.DrawHeader(col: integer; rect: TGfxRect; flags: integer);
var
  s : string16;
  x : integer;
begin
  case col of
    1 : s := 'Col.';
    2 : s := 'Title';
    3 : s := 'Field';
    4 : s := 'Width';
    5 : s := 'Align';
  else
    s := 'xxx';
  end;

  s := u8(s);

  x := (rect.width div 2) - (FHeaderFont.TextWidth16(s) div 2);
  if x < 1 then x := 1;
  canvas.DrawString16(rect.left + x, rect.top+1, s);

end;

constructor TColumnsGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowSelect := true;
end;

end.
