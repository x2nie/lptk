{ Copyright (c) 2003, Nagy Viktor

 $Id$
 
 Some property editors
}

unit vfdeditors;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wgcustomgrid, wgdbgrid, wgcheckbox, vfdforms;

type
  TItemEditorForm = class(TVFDDialog)
  public
    l1 : TwgLabel;

    edItems : TwgMemo;

    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);

  end;

  TColumnsGrid = class(TwgCustomGrid)
  public
    function GetRowCount : integer; override;

    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;

  public
    dbgrid : TwgDBGrid;

    constructor Create(AOwner : TComponent); override;

  end;

  TColumnEditForm = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: ColumnEditForm}
    lbLabel1 : TwgLabel;
    grid : TColumnsGrid;
    lbLabel2 : TwgLabel;
    lbLabel3 : TwgLabel;
    lbLabel4 : TwgLabel;
    lbLabel5 : TwgLabel;
    lbLabel6 : TwgLabel;
    lbCOLNO : TwgLabel;
    edTITLE : TwgEdit;
    edFIELDNAME : TwgEdit;
    edCOLWIDTH : TwgEdit;
    chlALIGN : TwgChoiceList;
    btnNew : TwgButton;
    btnDelete : TwgButton;
    btnUP : TwgButton;
    btnDOWN : TwgButton;
    btnClose : TwgButton;
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

  lbLabel1 := TwgLabel.Create(self);
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

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(344,24,56,16);
    Text := u8('Column:');
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(344,56,34,16);
    Text := u8('Title:');
  end;

  lbLabel4 := TwgLabel.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(344,100,75,16);
    Text := u8('Field Name:');
  end;

  lbLabel5 := TwgLabel.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(344,188,83,16);
    Text := u8('Alignment:');
  end;

  lbLabel6 := TwgLabel.Create(self);
  with lbLabel6 do
  begin
    SetDimensions(344,144,88,16);
    Text := u8('Column width:');
  end;

  lbCOLNO := TwgLabel.Create(self);
  with lbCOLNO do
  begin
    SetDimensions(404,24,54,16);
    Text := u8('1');
  end;

  edTITLE := TwgEdit.Create(self);
  with edTITLE do
  begin
    SetDimensions(344,72,160,22);
    Text := u8('');
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  edFIELDNAME := TwgEdit.Create(self);
  with edFIELDNAME do
  begin
    SetDimensions(344,116,160,22);
    Text := u8('');
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  edCOLWIDTH := TwgEdit.Create(self);
  with edCOLWIDTH do
  begin
    SetDimensions(344,160,160,22);
    Text := u8('');
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  chlALIGN := TwgChoiceList.Create(self);
  with chlALIGN do
  begin
    SetDimensions(344,206,160,22);
    Items.Add(u8('Left'));
    Items.Add(u8('Right'));
    Items.Add(u8('Center'));
    OnChange := {$ifdef FPC}@{$endif}EditChange;
  end;

  btnNew := TwgButton.Create(self);
  with btnNew do
  begin
    SetDimensions(8,236,60,24);
    Text := u8('New');
    OnClick := {$ifdef FPC}@{$endif}NewButtonClick;
  end;

  btnDelete := TwgButton.Create(self);
  with btnDelete do
  begin
    SetDimensions(76,236,60,24);
    Text := u8('Delete');
    OnClick := {$ifdef FPC}@{$endif}DeleteButtonClick;
  end;

  btnUP := TwgButton.Create(self);
  with btnUP do
  begin
    SetDimensions(152,236,60,24);
    Text := u8('UP');
    OnClick := {$ifdef FPC}@{$endif}UpDownButtonClick;
  end;

  btnDOWN := TwgButton.Create(self);
  with btnDOWN do
  begin
    SetDimensions(216,236,60,24);
    Text := u8('DOWN');
    OnClick := {$ifdef FPC}@{$endif}UpDownButtonClick;
  end;

  btnClose := TwgButton.Create(self);
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

function TColumnsGrid.GetRowCount: integer;
begin
  Result := dbgrid.ColumnCount;
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

constructor TColumnsGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  RowSelect := true;
  
  AddColumn8('Col.',30);
  AddColumn8('Title',80);
  AddColumn8('Field',80);
  AddColumn8('Width',40);
  AddColumn8('Align',50);
end;

{
  $Log$
  Revision 1.1  2004/05/05 01:35:32  nvitya
  new vfd branch

  Revision 1.6  2004/04/26 10:25:08  nvitya
  look corrections

  Revision 1.5  2003/11/10 00:01:50  nvitya
  using wgCustomGrid

}

end.
