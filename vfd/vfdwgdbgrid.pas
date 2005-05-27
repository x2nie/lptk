unit vfdwgdbgrid;

{$ifdef FPC}
  {$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wgcustomgrid, wgdbgrid, wgcheckbox,
  vfdforms, vfdwidgetclass, vfdprops, vfdformparser;

type
  TPropertyDBColumns = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;

    procedure OnExternalEdit(wg : TWidget); override;
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

implementation

procedure EditDBGridColumns(agrid : TwgDBGRid);
var
  frm : TColumnEditForm;
begin
  frm := TColumnEditForm.Create(nil);
  frm.dbgrid := agrid;
  frm.grid.dbgrid := agrid;
  frm.ShowModal;
  frm.Free;
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
    OnRowChange := GridRowChange;
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
    OnChange := EditChange;
  end;

  edFIELDNAME := TwgEdit.Create(self);
  with edFIELDNAME do
  begin
    SetDimensions(344,116,160,22);
    Text := u8('');
    OnChange := EditChange;
  end;

  edCOLWIDTH := TwgEdit.Create(self);
  with edCOLWIDTH do
  begin
    SetDimensions(344,160,160,22);
    Text := u8('');
    OnChange := EditChange;
  end;

  chlALIGN := TwgChoiceList.Create(self);
  with chlALIGN do
  begin
    SetDimensions(344,206,160,22);
    Items.Add(u8('Left'));
    Items.Add(u8('Right'));
    Items.Add(u8('Center'));
    OnChange := EditChange;
  end;

  btnNew := TwgButton.Create(self);
  with btnNew do
  begin
    SetDimensions(8,236,60,24);
    Text := u8('New');
    OnClick := NewButtonClick;
  end;

  btnDelete := TwgButton.Create(self);
  with btnDelete do
  begin
    SetDimensions(76,236,60,24);
    Text := u8('Delete');
    OnClick := DeleteButtonClick;
  end;

  btnUP := TwgButton.Create(self);
  with btnUP do
  begin
    SetDimensions(152,236,60,24);
    Text := u8('UP');
    OnClick := UpDownButtonClick;
  end;

  btnDOWN := TwgButton.Create(self);
  with btnDOWN do
  begin
    SetDimensions(216,236,60,24);
    Text := u8('DOWN');
    OnClick := UpDownButtonClick;
  end;

  btnClose := TwgButton.Create(self);
  with btnClose do
  begin
    SetDimensions(444,236,60,24);
    Text := u8('Close');
    OnClick := CloseButtonClick;
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

{ TPropertyDBColumns }

function TPropertyDBColumns.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TExternalPropertyEditor.Create(AOwner,self);
end;

function TPropertyDBColumns.GetValueText(wg: TWidget): string;
begin
  with TwgDBGrid(wg) do
  begin
    result := u8('['+IntToStr(ColumnCount)+' columns]');
  end;
end;

procedure TPropertyDBColumns.OnExternalEdit(wg: TWidget);
begin
  EditDBGridColumns(TwgDBGrid(wg));
end;

function TPropertyDBColumns.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  c : TDBColumn;
  s : string;
  sval : string;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase('ADDCOLUMN8') then Exit;

  c := TDBColumn.Create;

  result := CheckSymbol(s, '(');

  if result then c.Title := u8(GetStringValue(s));
  result := result and CheckSymbol(s, ',');
  if result then c.FieldName8 := GetStringValue(s);
  result := result and CheckSymbol(s, ',');
  if result then c.Width := GetIntValue(s);
  result := result and CheckSymbol(s, ',');
  if result then
  begin
    sval := UpperCase(GetIdentifier(s));
    if sval = 'ALRIGHT' then c.Alignment := alRight
    else if sval = 'ALCENTER' then c.Alignment := alCenter
    else c.Alignment := alLeft;
  end;

  result := result and CheckSymbol(s, ')');
  result := result and CheckSymbol(s, ';');

  if result then
  begin
    TwgDBGrid(wg).AddColumn(c.Title, c.FieldName8, c.Width, c.Alignment)
  end;

  c.Free;
end;

function TPropertyDBColumns.GetPropertySource(wg: TWidget; const ident: string): string;
var
  f : integer;
  c : TDBColumn;
  alstr : string;
begin
  result := '';
  with TwgDBGrid(wg) do
  begin
    for f := 0 to ColumnCount - 1 do
    begin
      c := Columns[f];
      case c.Alignment of
      alRight  : alstr := 'alRight';
      alCenter : alstr := 'alCenter';
      else
        alstr := 'alLeft';
      end;
      result := result + ident + 'AddColumn8('+QuotedStr(u16u8safe(c.Title))+','+QuotedStr(c.FieldName8)
                +','+IntToStr(c.Width)+','+alstr+');'#10;
    end;
  end;
end;

end.
