{ Copyright (c) 2003, Nagy Viktor

 The main code is here
}

unit vfddesigner;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxdialogs, sqldb, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox,
  vfdresizer, vfdforms, vfdeditors,
  vfdwidgetclass, vfdwidgets, vfdprops, newformdesigner;

type

  TOtherWidget = class(TWidget)
  protected
    FFont : TGfxFont;
  public
    wgClassName : string;

    constructor Create(AOwner : TComponent); override;

    procedure RePaint; override;
  end;

  TDesignedForm = class(TGfxForm)
  public
    procedure AfterCreate; override;

    procedure HandleClose; override;

  end;

  TFormDesigner = class;

  TWidgetDesigner = class
  private
    procedure SetSelected(const AValue: boolean);
  public
    FFormDesigner : TFormDesigner;
    FWidget : TWidget;
    FVFDClass : TVFDWidgetClass;

    FSelected  : boolean;

    resizer : array[1..8] of TwgResizer;

    other : TStringList;  // 8 bit

    constructor Create(AFormDesigner : TFormDesigner; wg : TWidget; wgc : TVFDWidgetClass);
    destructor Destroy; override;

    property Selected : boolean read FSelected write SetSelected;
    property Widget : TWidget read FWidget;

    procedure UpdateResizerPositions;

    property FormDesigner : TFormDesigner read FFormDesigner;

  end;

  TFormDesigner = class
  protected
    FWidgets : TList;

    FForm : TDesignedForm;

    FFormOther : string;

    FDragging : boolean;
    FDragPosX, FDragPosY : TGfxCoord;
    FWasDrag  : boolean;

  protected
    // messages of the designed widgets
    procedure MsgMouseDown(var msg : TMessageRec); message MSG_MOUSEDOWN;
    procedure MsgMouseUp(var msg : TMessageRec); message MSG_MOUSEUP;
    procedure MsgMouseMove(var msg : TMessageRec); message MSG_MOUSEMOVE;

    procedure MsgKeyPress(var msg : TMessageRec); message MSG_KEYPRESS;

    procedure MsgMove(var msg : TMessageRec); message MSG_MOVE;
    procedure MsgResize(var msg : TMessageRec); message MSG_RESIZE;

    procedure MsgActivate(var msg : TMessageRec); message MSG_ACTIVATE;

  public
    OneClickMove : boolean; // the widgets can be selected and dragged within one click

    constructor Create;
    destructor Destroy; override;

    procedure ClearForm;

    procedure DefaultHandler(var msg); override;

    procedure Show;

    procedure InitTest;

    function AddWidget(wg : TWidget; wgc : TVFDWidgetClass) : TWidgetDesigner;

    function WidgetDesigner(wg : TWidget) : TWidgetDesigner;
    function FindWidgetByName(const wgname : string) : TWidget;

    procedure DeSelectAll;
    procedure SelectAll;
    procedure SelectNextWidget(fw : boolean);

    procedure MoveResizeWidgets(dx,dy,dw,dh : integer);
    procedure DeleteWidgets;

    procedure EditWidgetOrder;

    procedure DesignerKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);

    procedure PutControlByName(x,y : integer; cname : string);
    procedure InsertWidget(pwg : TWidget; x,y : integer; wgc : TVFDWidgetClass);

    procedure OnPaletteChange(Sender : TObject);


    procedure UpdatePropWin;


    procedure OnPropTextChange(sender : TObject);
    procedure OnPropNameChange(sender : TObject);

    procedure OnPropPosEdit(sender : TObject);

    procedure OnOtherChange(sender : TObject);

    procedure OnAnchorChange(sender : TObject);

    procedure OnEditWidget(sender : TObject);

    function GenerateNewName(namebase : string) : string;

    procedure RunWidgetEditor(wgd : TWidgetDesigner; wg : TWidget);

  public

    function GetFormSourceDecl : string;
    function GetFormSourceImpl : string;

    function GetWidgetSourceImpl(wd : TWidgetDesigner; ident : string) : string;

    property Form : TDesignedForm read FForm;

    property FormOther : string read FFormOther write FFormOther;

  end;

implementation

uses vfdutils, vfdmain;

{ TWidgetDesigner }

procedure TWidgetDesigner.SetSelected(const AValue: boolean);
var
  n : integer;
begin
  if FSelected=AValue then exit;
  FSelected:=AValue;

  if FSelected then Widget.MouseCursor := CUR_MOVE else Widget.MouseCursor := CUR_DEFAULT;

  for n:=1 to 8 do
  begin
    if FSelected then
    begin
      resizer[n] := TwgResizer.Create(self, n);
    end
    else
    begin
      if resizer[n] <> nil then resizer[n].Free;
      resizer[n] := nil;
    end;
  end;

  UpdateResizerPositions;

  if FSelected and (Widget.Parent.WinHandle > 0) then
  begin
    for n:=1 to 8 do
    begin
      resizer[n].Show;
    end;
  end;
end;

constructor TWidgetDesigner.Create(AFormDesigner : TFormDesigner; wg : TWidget;
  wgc : TVFDWidgetClass );
var
  n : integer;
begin
  FFormDesigner := AFormDesigner;
  FWidget := wg;
  FVFDClass := wgc;
  for n:=1 to 8 do resizer[n] := nil;
  FSelected := false;
  wg.MouseCursor := CUR_DEFAULT;
  other := TStringList.Create;
end;

destructor TWidgetDesigner.Destroy;
var
  n : integer;
begin
  for n:=1 to 8 do
    if resizer[n] <> nil then resizer[n].Free;
  other.Free;
  inherited Destroy;
end;

procedure TWidgetDesigner.UpdateResizerPositions;
var
  n : integer;
  rs : TwgResizer;
begin
  if not FSelected then Exit;

  for n:=1 to 8 do
  begin
    rs := resizer[n];

    if rs <> nil then
    begin
      case n of
        1:  begin
              rs.left := Widget.left - 2;
              rs.Top  := Widget.Top - 2;
            end;
        2:  begin
              rs.Top  := Widget.Top - 2;
              rs.left := Widget.left + Widget.Width div 2 - 2;
            end;
        3:  begin
              rs.Top  := Widget.Top - 2;
              rs.left := Widget.left + Widget.Width - 1 - 2;
            end;
        4:  begin
              rs.Top  := Widget.Top + Widget.Height div 2 - 2;
              rs.left := Widget.left + Widget.Width - 1 - 2;
            end;
        5:  begin
              rs.Top  := Widget.Top + Widget.Height - 1 - 2;
              rs.left := Widget.left + Widget.Width - 1 - 2;
            end;
        6:  begin
              rs.Top  := Widget.Top + Widget.Height - 1 - 2;
              rs.left := Widget.left + Widget.Width div 2 - 2;
            end;
        7:  begin
              rs.Top  := Widget.Top + Widget.Height - 1 - 2;
              rs.left := Widget.left - 2;
            end;
        8:  begin
              rs.Top  := Widget.Top + Widget.Height div 2 - 2;
              rs.left := Widget.left - 2;
            end;
      end; // case
      if rs.WinHandle > 0 then rs.UpdateWindowPosition;
    end;
  end;

end;

{ TFormDesigner }

procedure TFormDesigner.MsgMouseDown(var msg: TMessageRec);
var
  wgd : TWidgetDesigner;
  shift : boolean;
begin
  FDragging := true;
  FWasDrag := false;
  FDragPosX := msg.Param1;
  FDragPosy := msg.Param2;

  if msg.dest = FForm then Exit;

  wgd := WidgetDesigner(TWidget(msg.dest));
  if wgd = nil then Exit;

  if not OneClickMove then Exit; // this Exit disables one click move

  shift := ((msg.Param3 and $FF) and ss_shift) <> 0;

  if shift then Exit;

  if not wgd.Selected then
  begin
    DeSelectAll;
    wgd.Selected := true;
    UpdatePropWin;
  end;
end;

procedure TFormDesigner.MsgMouseUp(var msg: TMessageRec);
var
  wgd : TWidgetDesigner;
  wgc : TVFDWidgetClass;
  pwg : TWidget;
  shift : boolean;
  x,y : integer;
begin
  FDragging := false;

  shift := ((msg.Param3 and $FF) and ss_shift) <> 0;

  wgc := frmMain.SelectedWidget;
  pwg := TWidget(msg.dest);
  wgd := WidgetDesigner(TWidget(msg.dest));
  if wgd = nil then pwg := FForm
  else if not wgd.FVFDClass.Container then wgc := nil;

  if wgc <> nil then
  begin
    DeSelectAll;

    if wgc <> nil then
    begin
      x := msg.Param1;
      y := msg.Param2;

      if GridResolution > 1 then
      begin
        x := x - x mod GridResolution;
        y := y - y mod GridResolution;
      end;

      InsertWidget(pwg, x,y, wgc);

      if not shift then
      begin
        FForm.MouseCursor := CUR_DEFAULT;
        frmMain.SelectedWidget := nil;
      end;
    end;
  end
  else
  begin
    wgd := WidgetDesigner(TWidget(msg.dest));
    if wgd = nil then
    begin
      DeSelectAll;
      UpdatePropWin;
      Exit;
    end;

    if not shift then
    begin
      if not wgd.Selected then DeSelectAll;
      wgd.Selected := true;
    end
    else wgd.Selected := not wgd.Selected;
  end;

  UpdatePropWin;
end;

procedure TFormDesigner.MsgMouseMove(var msg: TMessageRec);
var
  dx,dy : integer;
  wgd : TWidgetDesigner;
begin
  if not FDragging then Exit;

  FWasDrag := true;

  dx := msg.Param1 - FDragPosX;
  dy := msg.Param2 - FDragPosY;

  wgd := WidgetDesigner(TWidget(msg.dest));
  if (wgd = nil) or (not wgd.Selected) then Exit;

  if GridResolution > 1 then
  begin
    dx := dx - (dx mod GridResolution);
    dy := dy - (dy mod GridResolution);
  end;

  MoveResizeWidgets(dx,dy,0,0);
end;

procedure TFormDesigner.MsgKeyPress(var msg: TMessageRec);
var
  key, ss : word;
  consumed : boolean;
begin
  key := msg.Param1 and $FFFF;
  ss := msg.Param2 and $FFFF;
  consumed := false;

  DesignerKeyPress(key,ss,consumed);
end;

procedure TFormDesigner.MsgMove(var msg: TMessageRec);
begin
  if msg.dest = FForm then
  begin
    UpdatePropWin;
  end;
end;

procedure TFormDesigner.MsgResize(var msg: TMessageRec);
begin
  if msg.dest = FForm then
  begin
    DeSelectAll; // because of the anchorings
    UpdatePropWin;
  end;
end;

constructor TFormDesigner.Create;
begin
  FWidgets := TList.Create;
  FWasDrag := false;

  OneClickMove := true; //false;

  FForm := TDesignedForm.Create(nil);
  FForm.FormDesigner := self;
  FForm.Name := maindsgn.NewFormName;
  FForm.WindowTitle8 := FForm.Name;
  FFormOther := '';
end;

destructor TFormDesigner.Destroy;
var
  n : integer;
begin
  for n:=0 to FWidgets.Count-1 do TObject(FWidgets.Items[n]).Free;
  FWidgets.Free;

  if FForm <> nil then FForm.Free;
  inherited Destroy;
end;

procedure TFormDesigner.ClearForm;
var
  n : integer;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    TWidgetDesigner(FWidgets.Items[n]).Widget.Free;
    TObject(FWidgets.Items[n]).Free;
  end;
  FWidgets.Clear;
end;

procedure TFormDesigner.DefaultHandler(var msg);
begin
  //Writeln('Designer message: ',TMessageRec(msg).msgcode,' from ',TMessageRec(msg).dest.ClassName);
end;

procedure TFormDesigner.Show;
begin
  FForm.Show;
  UpdatePropWin;
end;

procedure TFormDesigner.InitTest;
var
  l1 : TwgLabel;
  ed1 : TwgEdit;
begin
  Exit;

  l1 := CreateLabel(FForm, 10,10, 'Test Label' );
  ed1 := CreateEdit(FForm, 10, 50, 150, 0);

  AddWidget(l1, nil);
  AddWidget(ed1, nil);
end;

function TFormDesigner.AddWidget(wg : TWidget; wgc : TVFDWidgetClass) : TWidgetDesigner;
var
  cd : TWidgetDesigner;
begin
  cd := TWidgetDesigner.Create(self, wg, wgc);
  FWidgets.Add(cd);
  //cd.Selected := true;
  wg.FormDesigner := self;
  result := cd;
end;

function TFormDesigner.WidgetDesigner(wg: TWidget): TWidgetDesigner;
var
  n : integer;
  cd : TWidgetDesigner;
begin
  result := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Widget = wg then
    begin
      result := cd;
      Exit;
    end;
  end;
end;

procedure TFormDesigner.DeSelectAll;
var
  n : integer;
  cd : TWidgetDesigner;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    cd.Selected := false;
  end;
end;

procedure TFormDesigner.SelectAll;
var
  n : integer;
  cd : TWidgetDesigner;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    cd.Selected := True;
  end;
end;

procedure TFormDesigner.SelectNextWidget(fw: boolean);
var
  n, dir : integer;
  cd, scd : TWidgetDesigner;
begin
  if FWidgets.Count < 1 then Exit;

  if fw then
  begin
    n := 0;
    dir := 1;
  end
  else
  begin
    dir := -1;
    n := FWidgets.Count-1;
  end;

  scd := TWidgetDesigner(FWidgets.Items[n]);

  while (n >= 0) and (n < FWidgets.Count) do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      if fw then
      begin
        if n < FWidgets.Count - 1 then scd := TWidgetDesigner(FWidgets.Items[n+1]);
      end
      else
      begin
        if n > 0 then scd := TWidgetDesigner(FWidgets.Items[n-1]);
      end;
      break;
    end;
    n := n + dir;
  end;
  DeSelectAll;
  scd.Selected := true;
  UpdatePropWin;
end;

procedure TFormDesigner.MoveResizeWidgets(dx, dy, dw, dh: integer);
var
  n : integer;
  cd : TWidgetDesigner;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      if GridResolution > 1 then
      begin

      end;
      cd.Widget.MoveResizeBy(dx,dy,dw,dh);
      cd.UpdateResizerPositions;
    end;
  end;
  UpdatePropWin;
end;

procedure TFormDesigner.DeleteWidgets;
var
  n : integer;
  cd : TWidgetDesigner;
begin
  n := 0;
  while n < FWidgets.Count do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      cd.Widget.Free;
      cd.Free;
      FWidgets.Delete(n);
    end
    else inc(n);
  end;
end;


procedure TFormDesigner.EditWidgetOrder;
var
  frm : TWidgetOrderForm;
  n,fi : integer;
  cd : TWidgetDesigner;
  identlevel : integer;

  procedure AddChildWidgets(pwg : TWidget; slist : TStringList);
  var
    f : integer;
    fcd : TWidgetDesigner;
  begin
    for f:=0 to FWidgets.Count-1 do
    begin
      fcd := TWidgetDesigner(FWidgets.Items[f]);

      if fcd.Widget.Parent = pwg then
      begin
        frm.list.Items.AddObject(u8(StringOfChar(' ',identlevel)+fcd.Widget.Name + ' : ' + fcd.Widget.ClassName),fcd);
        inc(identlevel);
        AddChildWidgets(fcd.Widget,slist);
        dec(identlevel);
      end;

      if fcd.Selected then fi := f+1;
    end;
  end;


begin
  frm := TWidgetOrderForm.Create(nil);
  fi := 1;

  identlevel := 0;

  AddChildWidgets(FForm, frm.list.Items);

  if fi <= frm.list.ItemCount then frm.list.FocusItem := fi;

  if frm.ShowModal = 1 then
  begin
    for n:=0 to FWidgets.Count-1 do TWidgetDesigner(FWidgets.Items[n]).Widget.Visible := false;

    for n:=0 to FWidgets.Count-1 do
    begin
      FWidgets.Items[n] := frm.List.Items.Objects[n];
    end;

    for n:=0 to FWidgets.Count-1 do
    begin
      cd := TWidgetDesigner(FWidgets.Items[n]);
      cd.Widget.Visible := true;
    end;

    for n:=0 to FWidgets.Count-1 do
    begin
      cd := TWidgetDesigner(FWidgets.Items[n]);
      if cd.Selected then
      begin
        // re-creating the resizers
        cd.Selected := false;
        cd.Selected := true;
      end;
    end;

  end;
  frm.Free;
end;

procedure TFormDesigner.DesignerKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  dx,dy : integer;
begin
  dx := 0;
  dy := 0;
  consumed := true;

  case keycode of
    KEY_LEFT:   dx := -1;
    KEY_RIGHT:  dx := +1;
    KEY_UP:     dy := -1;
    KEY_DOWN:   dy := +1;

    KEY_DELETE: DeleteWidgets;

    KEY_TAB: SelectNextWidget(True);
    KEY_STAB: SelectNextWidget(False);

    KEY_F1: ShowMessage8( 'ENTER, F11: switch to Properties'+#10
                          +'TAB, SHIFT+TAB: select next widget'+#10
                          +'F2: edit widget order'#10
                          +'F4: edit items'#10
                          ,'Small help');

    KEY_F2: EditWidgetOrder;

    //KEY_F4: if PropertyForm.btnEdit.Visible then PropertyForm.btnEdit.Click;

    KEY_F11,
    KEY_ENTER:
      begin
        GfxActivateWindow(frmProperties.WinHandle);
      end;
  else
    consumed := false;
  end;

  if (dx <> 0) or (dy <> 0) then
  begin
    if (shiftstate and ss_shift) <> 0
      then MoveResizeWidgets(0,0,dx,dy)
      else MoveResizeWidgets(dx,dy,0,0);
  end;
end;

procedure TFormDesigner.PutControlByName(x, y: integer; cname: string);
var
  wg : TWidget;
  wgd : TWidgetDesigner;
  wgcname : string;
  newname : string;
  cfrm : TInsertCustomForm;
begin
  wgcname := UpperCase(cname);

  Writeln('Putting control: ', wgcname);

  wg := nil;
  newname := 'Widget';
  if wgcname = 'LABEL' then
  begin
    newname := GenerateNewName('lbLabel');
    wg := CreateLabel(FForm, x,y, newname);
  end
  else if wgcname = 'EDIT' then
  begin
    newname := GenerateNewName('edEdit');
    wg := CreateEdit(FForm, x,y, 150, 0);
    TwgEdit(wg).Text8 := newname;
  end
  else if wgcname = 'BUTTON' then
  begin
    newname := GenerateNewName('btnButton');
    wg := CreateButton(FForm, x,y, 105, newname, nil);
  end
  else if wgcname = 'CHECKBOX' then
  begin
    newname := GenerateNewName('cbCheck');
    wg := CreateCheckBox(FForm, x,y, newname);
  end
  else if wgcname = 'MEMO' then
  begin
    newname := GenerateNewName('edMemo');
    wg := TwgMemo.Create(FForm);
    wg.SetDimensions(x,y,120,80);
    TwgMemo(wg).Text := Str8To16(newname);
  end
  else if wgcname = 'TEXTLISTBOX' then
  begin
    newname := GenerateNewName('lstList');
    wg := TwgTextListBox.Create(FForm);
    wg.SetDimensions(x,y,160,200);
    TwgTextListBox(wg).Items.Add(str8to16(newname));
  end
  else if wgcname = 'CHOICELIST' then
  begin
    newname := GenerateNewName('chlChoice');
    wg := CreateChoicelist(FForm, x,y,150, nil);
    TwgChoiceList(wg).Items.Add(str8to16(newname));
  end
  else if wgcname = 'DBGRID' then
  begin
    newname := GenerateNewName('gridGrid');
    wg := TwgDBGrid.Create(FForm);
    wg.SetDimensions(x,y,160,140);
    //TwgDBGrid(wg).AddColumn(str8To16(newname), 'FIELDNAME', 120, alLeft);
  end
  else
  begin
    // custom component

    cfrm := TInsertCustomForm.Create(nil);
    if cfrm.ShowModal = 1 then
    begin
      newname := cfrm.edName.Text8;
      if newname = '' then newname := GenerateNewName(cfrm.edClass.Text8);
      wg := TOtherWidget.Create(FForm);
      TOtherWidget(wg).wgClassName := cfrm.edClass.Text8;
      wg.SetDimensions(x,y,200,24);
    end;
    cfrm.Free;

  end;
  ;

  if wg <> nil then
  begin
    wg.name := newname;
    wgd := AddWidget(wg, nil);
    wg.ShowWidget;
    DeSelectAll;
    wgd.Selected := true;
    UpdatePropWin;
  end;
end;

procedure TFormDesigner.OnPaletteChange(Sender: TObject);
begin
  if PaletteForm.clist.FocusItem > 1 then FForm.MouseCursor := CUR_CROSSHAIR
                                     else FForm.MouseCursor := CUR_DEFAULT;
end;

procedure TFormDesigner.UpdatePropWin;
var
  n,i : integer;
  cd, scd : TWidgetDesigner;
  wg : TWidget;

  wgcnt : integer;
  //btxt  : boolean;
  //bedit : boolean;

  lastpropname : string;

  wgc : TVFDWidgetClass;
begin
  wgcnt := 0;
  wg := FForm;
  wgc := VFDFormWidget;
  scd := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      inc(wgcnt);
      if wgcnt < 2 then
      begin
        wg := cd.Widget;
        scd := cd;
      end;
    end;
  end;

  if scd <> nil then wgc := scd.FVFDClass;

  //Writeln('wg: ',wg.ClassName);

  n := frmProperties.lstProps.FocusItem;
  if (n > 0) and (PropList.GetItem(n) <> nil)
    then lastpropname := PropList.GetItem(n).Name
    else lastpropname := '';

  i := 0;

  if PropList.Widget <> wg then
  begin
    frmProperties.lstProps.ReleaseEditor;
    PropList.Clear;
    for n:=1 to wgc.PropertyCount do
    begin
      PropList.AddItem(wgc.GetProperty(n));
      if UpperCase(wgc.GetProperty(n).Name) = UpperCase(lastPropName) then i := n;
    end;
    PropList.Widget := wg;
    frmProperties.lstProps.Update;
    if i > 0 then frmProperties.lstProps.FocusItem := i;
  end;

  with frmProperties do
  begin
    if wg is TOtherWidget then lbClass.Text8 := TOtherWidget(wg).wgClassName
                          else lbClass.Text8 := wg.ClassName;
    edName.Text8 := wg.Name;

    if scd <> nil then
    begin
      edOther.Text := str8to16(scd.other.Text);
    end
    else
    begin
      edOther.Text := str8to16(FFormOther);
    end;
    
    edName.Visible := (wgcnt < 2);
    edOther.Visible := (wgcnt < 2);

    lstProps.RePaint;
  end;

  with frmProperties do
  begin
    btnLeft.Text8 := IntToStr(wg.Left);
    btnTop.Text8 := IntToStr(wg.Top);
    btnWidth.Text8 := IntToStr(wg.Width);
    btnHeight.Text8 := IntToStr(wg.Height);

    btnAnLeft.Down := anLeft in wg.Anchors;
    btnAnTop.Down := anTop in wg.Anchors;
    btnAnRight.Down := anRight in wg.Anchors;
    btnAnBottom.Down := anBottom in wg.Anchors;
  end;

  exit;
{
  with PropertyForm do
  begin
    if wg is TOtherWidget then lbClass.Text8 := TOtherWidget(wg).wgClassName
                          else lbClass.Text8 := wg.ClassName;

    edName.Text8 := wg.Name;

    lbLeft.Text8 := IntToStr(wg.Left);
    lbTop.Text8 := IntToStr(wg.Top);
    lbWidth.Text8 := IntToStr(wg.Width);
    lbHeight.Text8 := IntToStr(wg.Height);

    cbAL.Checked := anLeft in wg.Anchors;
    cbAT.Checked := anTop in wg.Anchors;
    cbAR.Checked := anRight in wg.Anchors;
    cbAB.Checked := anBottom in wg.Anchors;

    btxt  := true;
    bedit := false;
    lbText.Text := u8('Text:');

    if wg is TGfxForm then edText.Text := TGfxForm(wg).WindowTitle
    else if wg is TwgLabel then edText.Text := TwgLabel(wg).Text
    else if wg is TwgEdit  then edText.Text := TwgEdit(wg).Text
    else if wg is TwgButton then edText.Text := TwgButton(wg).Text
    else if wg is TwgCheckBox then edText.Text := TwgCheckBox(wg).Text
    else btxt := false;

    if not btxt and ((wg is TwgMemo) or (wg is TwgChoiceList) or (wg is TwgTextListBox))
    then
    begin
      bedit := true;
      lbText.Text := u8('Items:');
      btnEdit.Text := u8('Edit items...');
    end
    else if (wg is TwgDBGrid) then
    begin
      bedit := true;
      //lbText.Text := u8('Items:');
      btnEdit.Text := u8('Edit columns...');
    end;

    if scd <> nil then
    begin
      edOther.Text := str8to16(scd.other.Text);
    end
    else
    begin
      edOther.Text := str8to16(FFormOther);
    end;

    edText.Visible := btxt;
    lbText.Visible := btxt;
    btnEdit.Visible := bedit;
    edName.Visible := (wgcnt < 2);
    edOther.Visible := (wgcnt < 2);

  end; // with PropertyForm
}
end;

procedure TFormDesigner.OnPropTextChange(sender: TObject);
{
var
  n : integer;
  cd : TWidgetDesigner;
  wg : TWidget;
  s : string16;
}
begin
{
  s := PropertyForm.edText.Text;
  wg := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      wg := cd.Widget;
      SetWidgetText(wg,s);
      if wg is TwgLabel then
      with TwgLabel(wg) do
      begin
        if Font.TextWidth16(Text) > width then
        begin
          Width := Font.TextWidth16(Text);
          UpdateWindowPosition;
          cd.UpdateResizerPositions;
        end;
      end;
    end;
  end;

  if wg = nil then
  begin
    FForm.WindowTitle := s;
  end;
}
end;

procedure TFormDesigner.OnPropNameChange(sender: TObject);
var
  n : integer;
  cd : TWidgetDesigner;
  wg : TWidget;
  s8 : string;
begin
//  writeln('namechange');
  s8 := str16to8(frmProperties.edName.Text);
  wg := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      wg := cd.Widget;
{
      if GetWidgetText(wg,s) and (wg.Name = str16to8(s)) then
      begin
        PropertyForm.edText.Text8 := s8;
        OnPropTextChange(sender);
      end;
}
    end;
  end;

  if wg = nil then
  begin
    wg := FForm;
{
    if FForm.Name = FForm.WindowTitle8 then
    begin
      FForm.WindowTitle8 := s8;
      PropertyForm.edText.Text8 := s8;
    end;
}    
  end;

  try
    wg.Name := s8;
  except
    // invalid name...
  end;
end;

procedure TFormDesigner.OnPropPosEdit(sender: TObject);
var
  frm : TEditPositionForm;
  btn : TwgButton;
  ax,ay : TGfxCoord;
  wg : TWidget;

  n : integer;
  cd : TWidgetDesigner;

  posval : integer;

  procedure SetNewPos(awg : TWidget; pval : integer);
  begin
    if sender = frmProperties.btnLeft then awg.Left := pval
    else if sender = frmProperties.btnTop then awg.Top := pval
    else if sender = frmProperties.btnWidth then awg.Width := pval
    else if sender = frmProperties.btnHeight then awg.Height := pval
    ;
  end;

begin
  wg := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      wg := cd.Widget;
      break;
    end;
  end;

  if wg = nil then wg := Form;

  btn := TwgButton(sender);
  frm := TEditPositionForm.Create(nil);
  GfxGetAbsolutePosition(btn.WinHandle, btn.width, 0, ax,ay);
  frm.Left := ax;
  frm.Top := ay;
  if sender = frmProperties.btnLeft then
  begin
    frm.lbPos.Text8 := 'Left:';
    frm.edPos.Text8 := IntToStr(wg.Left);
  end
  else if sender = frmProperties.btnTop then
  begin
    frm.lbPos.Text8 := 'Top:';
    frm.edPos.Text8 := IntToStr(wg.Top);
  end
  else if sender = frmProperties.btnWidth then
  begin
    frm.lbPos.Text8 := 'Width:';
    frm.edPos.Text8 := IntToStr(wg.Width);
  end
  else if sender = frmProperties.btnHeight then
  begin
    frm.lbPos.Text8 := 'Height:';
    frm.edPos.Text8 := IntToStr(wg.Height);
  end
  ;

  posval := -9999;
  if frm.ShowModal = 1 then
  begin
    posval := StrToIntDef(frm.edPos.Text8,-9999);
  end;
  frm.Free;

  if posval > -999 then
  begin
    wg := nil;
    for n:=0 to FWidgets.Count-1 do
    begin
      cd := TWidgetDesigner(FWidgets.Items[n]);
      if cd.Selected then
      begin
        wg := cd.Widget;
        SetNewPos(wg, posval);
        wg.UpdateWindowPosition;
        cd.UpdateResizerPositions;
      end;
    end;

    if wg = nil then
    begin
      SetNewPos(FForm, posval);
      FForm.UpdateWindowPosition;
    end;

  end;

  UpdatePropWin;

end;

procedure TFormDesigner.OnOtherChange(sender: TObject);
var
  n : integer;
  cd : TWidgetDesigner;
  s : string16;
  sc : integer;
begin
  sc := 0;
  s := frmProperties.edOther.Text8;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      cd.other.Text := s;
      inc(sc);
    end;
  end;

  if sc < 1 then
  begin
    FFormOther := s;
  end;
end;

procedure TFormDesigner.OnAnchorChange(sender: TObject);
var
  n : integer;
  cd : TWidgetDesigner;
  wg : TWidget;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      wg := cd.Widget;

      wg.Anchors := [];
      if frmProperties.btnAnLeft.Down then wg.Anchors := wg.Anchors + [anLeft];
      if frmProperties.btnAnTop.Down then wg.Anchors := wg.Anchors + [anTop];
      if frmProperties.btnAnRight.Down then wg.Anchors := wg.Anchors + [anRight];
      if frmProperties.btnAnBottom.Down then wg.Anchors := wg.Anchors + [anBottom];
    end;
  end;
end;

function TFormDesigner.GenerateNewName(namebase: string): string;
var
  nind,n : integer;
  cd : TWidgetDesigner;
  newname : string;
  bok : boolean;
begin
  nind := 1;
  repeat
    newname := namebase + IntToStr(nind);
    bok := true;
    for n:=0 to FWidgets.Count-1 do
    begin
      cd := TWidgetDesigner(FWidgets.Items[n]);
      if cd.Widget.Name = newname then
      begin
        bok := false;
        break;
      end;
    end;
    inc(nind);
  until bok;
  result := newname;
end;


procedure TFormDesigner.MsgActivate(var msg: TMessageRec);
begin
  maindsgn.SelectForm(self);
end;

function TFormDesigner.GetFormSourceDecl: string;
var
  n : integer;
  wd : TWidgetDesigner;
  wgclass : string;
begin
  result := '';
  for n:=0 to FWidgets.Count-1 do
  begin
    wd := TWidgetDesigner(FWidgets.Items[n]);
    if wd.Widget is TOtherWidget then wgclass := TOtherWidget(wd.Widget).wgClassName else wgclass := wd.Widget.ClassName;
    result := result + '    '+wd.Widget.Name+' : '+wgclass+';'#10;
  end;
end;

function TFormDesigner.GetFormSourceImpl: string;
var
  s : string;
  sl : TStringList;
  n : integer;
  wd : TWidgetDesigner;
  wg : TWidget;
  wgclass, pwgname : string;
begin
  s := '';

  if maindsgn.SaveComponentNames then
    s := s + '  Name := '+QuotedStr(FForm.Name)+';'#10;

  s := s + '  SetDimensions('+IntToStr(FForm.Left)+','+IntToStr(FForm.Top)
    +','+IntToStr(FForm.Width)+','+IntToStr(FForm.Height)+');'#10;
  s := s + '  WindowTitle8 := '+QuotedStr(u16u8safe(FForm.WindowTitle))+';'#10;

  //adding other form properties, idented
  sl := TStringList.Create;
  sl.Text := FFormOther;
  for n:=0 to sl.Count-1 do s := s + '  ' + sl.Strings[n] + #10;
  sl.Free;

  s := s + #10;

  // FORM WIDGETS

  for n:=0 to FWidgets.Count-1 do
  begin
    wd := TWidgetDesigner(FWidgets.Items[n]);
    wg := wd.Widget;
    if wg.Parent = FForm then pwgname := 'self' else pwgname := wg.Parent.Name;
    if wg is TOtherWidget then wgclass := TOtherWidget(wg).wgClassName else wgclass := wg.ClassName;

    s := s + '  '+wg.Name+' := '+wgclass+'.Create('+pwgname+');'#10
           + '  with '+wg.Name+' do'#10
           + '  begin'#10
           + GetWidgetSourceImpl(wd,'    ')
           + '  end;'#10
           + #10;
  end;

  result := s;
end;

function TFormDesigner.GetWidgetSourceImpl(wd: TWidgetDesigner; ident: string): string;
var
  ts,cs : string;
  s : string;
  wg : TWidget;
  wgc : TVFDWidgetClass;
  n : integer;

  procedure SaveItems(name : string; sl : TStringList);
  var
    f : integer;
  begin
    for f := 0 to sl.Count - 1 do
    begin
      s := s + ident + name + '.Add(u8('+QuotedStr(u16u8safe(sl.Strings[f]))+'));'#10;
    end;
  end;

  procedure SaveColumns(grid : TwgDBGrid);
  var
    f : integer;
    c : TDBColumn;
    alstr : string;
  begin
    for f := 0 to grid.ColumnCount - 1 do
    begin
      c := grid.Columns[f];
      case c.Alignment of
      alRight  : alstr := 'alRight';
      alCenter : alstr := 'alCenter';
      else
        alstr := 'alLeft';
      end;
      s := s + ident + 'AddColumn8('+QuotedStr(u16u8safe(c.Title))+','+QuotedStr(c.FieldName8)
                +','+IntToStr(c.Width)+','+alstr+');'#10;
    end;
  end;

begin
  wg := wd.Widget;
  wgc := wd.FVFDClass;
  s := '';

  if maindsgn.SaveComponentNames then
    s := s + ident + 'Name := '+QuotedStr(wg.Name)+';'#10;

  s := s + ident + 'SetDimensions('
         + IntToStr(wg.Left)+','+IntToStr(wg.Top)+','+IntToStr(wg.Width)+','+IntToStr(wg.Height)+');'#10;

  if wg.Anchors <> [anLeft,anTop] then
  begin
    ts := '[';
    cs := '';
    if anLeft in wg.Anchors then begin ts := ts + cs + 'anLeft';  cs := ',';  end;
    if anRight in wg.Anchors then begin ts := ts + cs + 'anRight';  cs := ',';  end;
    if anTop in wg.Anchors then begin ts := ts + cs + 'anTop';  cs := ',';  end;
    if anBottom in wg.Anchors then begin ts := ts + cs + 'anBottom';  cs := ',';  end;
    ts := ts + '];';
    s := s + ident + 'Anchors := ' + ts + #10;
  end;

  for n:=1 to wgc.PropertyCount do
  begin
    s := s + wgc.GetProperty(n).GetPropertySource(wg, ident);
  end;
  
{
  if wg is TwgMemo then
  begin
    SaveItems('Lines',TwgMemo(wg).Lines);
  end
  else if wg is TwgChoiceList then
  begin
    SaveItems('Items',TwgChoiceList(wg).Items);
  end
  else if wg is TwgTextListBox then
  begin
    SaveItems('Items',TwgTextListBox(wg).Items);
  end
  else if wg is TwgDBGrid then
  begin
    SaveColumns(TwgDBGrid(wg));
  end
  else
    if GetWidgetText(wg, ts) then
    begin
      s := s + ident + 'Text := u8('+QuotedStr(u8encode(ts))+');'#10;  // encoding with all printable characters
    end;
}    

  for n:=0 to wd.other.Count-1 do
    if trim(wd.other.Strings[n]) <> '' then
      s := s + ident + wd.other.Strings[n] + #10;

  result := s;
end;

procedure TFormDesigner.OnEditWidget(sender: TObject);
var
  n : integer;
  cd : TWidgetDesigner;
  wg : TWidget;
begin
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if cd.Selected then
    begin
      wg := cd.Widget;

      // Running widget editor;
      RunWidgetEditor(cd, wg);

      Exit;
    end;
  end;
end;

procedure EditItems(sl : TStringList);
var
  frmie : TItemEditorForm;
  //ax,ay : integer;
begin
  frmie := TItemEditorForm.Create(nil);
  //GfxGetAbsolutePosition(PropertyForm.btnEdit.WinHandle, PropertyForm.btnEdit.width, 0, ax,ay);
  //frmie.Left := ax;
  //frmie.Top := ay;

  frmie.edItems.Lines.Assign(sl);
  if frmie.ShowModal = 1 then
  begin
    Writeln('OK');
    sl.Assign(frmie.edItems.Lines);
  end;
  frmie.Free;
end;

procedure TFormDesigner.RunWidgetEditor(wgd: TWidgetDesigner; wg: TWidget);
begin
  if wg is TwgMemo then
  begin
    EditItems(TwgMemo(wg).Lines);
    wg.RePaint;
  end
  else if wg is TwgChoicelist then
  begin
    EditItems(TwgChoiceList(wg).Items);
    wg.RePaint;
  end
  else if wg is TwgTextListBox then
  begin
    EditItems(TwgTextListBox(wg).Items);
    wg.RePaint;
  end
  else if wg is TwgDBGrid then
  begin
    //EditDBGridColumns(TwgDBGrid(wg));
  end;
end;

procedure TFormDesigner.InsertWidget(pwg : TWidget; x,y : integer; wgc : TVFDWidgetClass);
var
  cfrm : TInsertCustomForm;
  newname, newclassname : string;
  wg : TWidget;
  wgd : TWidgetDesigner;
begin
  if wgc = nil then Exit;

  newname := '';

  if wgc.WidgetClass = TOtherWidget then
  begin
    newclassname := '';
    cfrm := TInsertCustomForm.Create(nil);
    cfrm.edName.Text8 := GenerateNewName(wgc.NameBase);
    cfrm.edClass.Text8 := 'Twg';
    if cfrm.ShowModal = 1 then
    begin
      newname := cfrm.edName.Text8;
      newClassName := cfrm.edClass.Text8;
    end;
    cfrm.Free;
    if (newname = '') or (newclassname = '') then Exit;
  end;

  wg := wgc.CreateWidget(pwg);
  if wg <> nil then
  begin
    wg.Left := x;
    wg.Top  := y;
    if newname = '' then newname := GenerateNewName(wgc.NameBase);
    wg.name := newname;
    if wgc.WidgetClass = TOtherWidget then TOtherWidget(wg).wgClassName := newclassname;
    wgd := AddWidget(wg, wgc);
    wg.ShowWidget;
    DeSelectAll;
    wgd.Selected := true;
    UpdatePropWin;
  end;
end;

function TFormDesigner.FindWidgetByName(const wgname: string): TWidget;
var
  n : integer;
  wgnameuc : string;
  cd : TWidgetDesigner;
begin
  wgnameuc := UpperCase(wgname);
  result := nil;
  for n:=0 to FWidgets.Count-1 do
  begin
    cd := TWidgetDesigner(FWidgets.Items[n]);
    if UpperCase(cd.Widget.Name) = wgnameuc then
    begin
      result := cd.Widget;
      Exit;
    end;
  end;
end;

{ TDesignedForm }

procedure TDesignedForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowPosition := wpUser;

  WindowTitle8 := 'New Form';

  SetDimensions(300,100,300,250);
//  SetDimensions(300,100,150,150);
end;

procedure TDesignedForm.HandleClose;
begin
  //inherited HandleClose;
  // do nothing.
end;

{ TOtherWidget }

constructor TOtherWidget.Create(AOwner: TComponent);
begin
  inherited;
  wgClassName := 'TWidget';
  FBackgroundColor := $C0E0C0;
  FFont := guistyle.DefaultFont;
  FWidth := 120;
  FHeight := 32;
end;

procedure TOtherWidget.RePaint;
var
  s : string;
begin
  inherited;

  if FWinHandle <= 0 then Exit;

  //canvas.ClearClipRect;
  canvas.Clear(FBackgroundColor);
  canvas.SetFont(FFont);

  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0,0,width,height);

  canvas.SetTextColor(clText1);

  s := u8( Name + ' : ' +wgClassName );

  canvas.DrawString16(2, 2, s);
end;

end.

