{ Copyright (c) 2003, Nagy Viktor

 Main VFD forms
}

unit vfdforms;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox;

type

  TVFDDialog = class(TGfxForm)
  public

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  end;

  TInsertCustomForm = class(TVFDDialog)
  public

    l1,l2 : TwgLabel;
    edClass : TwgEdit;
    edName : TwgEdit;
    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);

  end;

  TNewFormForm = class(TVFDDialog)
  public
    l1 : TwgLabel;
    edName : TwgEdit;
    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);
  end;

  TEditPositionForm = class(TVFDDialog)
  public
    lbPos : TwgLabel;
    edPos : TwgEdit;

    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);
  end;

  TWidgetOrderForm = class(TVFDDialog)
  public
    l1 : TwgLabel;

    list : TwgTextListBox;

    btnUP, btnDOWN,
    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);
  end;

  TPaletteForm = class(TGfxForm)
  public
    clab  : TwgLabel;
    clist : TwgTextListBox;

    procedure AfterCreate; override;

    procedure HandleClose; override;
  end;

  TMainForm = class(TGfxForm)
  public
    l1,l2 : TwgLabel;
    edFormFile : TwgEdit;
    btnSave : TwgButton;
    btnLoad : TwgButton;

    btnNewForm : TwgButton;

    chlGrid : TwgChoiceList;

    procedure AfterCreate; override;
  end;

  TPropertyForm = class(TGfxForm)
  public

    l1,l2,l3,l4,l5,l6,l7,l8 : TwgLabel;

    lbClass : TwgLabel;
    edName  : TwgEdit;

    lbText  : TwgLabel;
    edText  : TwgEdit;
    btnEdit : TwgButton;

    lbTop, lbLeft, lbWidth, lbHeight : TwgLabel;
    btnTop, btnLeft, btnWidth, btnHeight : TwgButton;

    cbAL,cbAT,cbAR,cbAB : TwgCheckBox;

    edOther : TwgMemo;

    procedure AfterCreate; override;

    procedure HandleClose; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  end;

var
  PaletteForm : TPaletteForm;
  MainForm : TMainForm;
  PropertyForm : TPropertyForm;

function GridResolution : integer;

implementation

uses vfdmain;

function GridResolution : integer;
begin
  case MainForm.chlGrid.FocusItem of
    2 : result := 4;
    3 : result := 8;
  else
    result := 1;
  end;
end;

{ TPaletteForm }

procedure TPaletteForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowPosition := wpUser;

  WindowTitle8 := 'Palette';

  SetDimensions(10,450,110,220);

  clab := CreateLabel(self, 3,3,'Widget Palette:');

  clist := TwgTextListBox.Create(self);
  clist.Left := 1;
  clist.width := width - 2;
  clist.Top := 22;
  clist.height := height - clist.top - 2;
  clist.Anchors := AllAnchors;

  clist.Items.Add(str8to16('-'));
  clist.Items.Add(str8to16('Label'));
  clist.Items.Add(str8to16('Edit'));
  clist.Items.Add(str8to16('Button'));
  clist.Items.Add(str8to16('CheckBox'));
  clist.Items.Add(str8to16('ChoiceList'));
  clist.Items.Add(str8to16('Memo'));
  clist.Items.Add(str8to16('TextListBox'));
  clist.Items.Add(str8to16('DBGrid'));
  clist.Items.Add(str8to16('[OTHER]'));

  clist.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnPaletteChange;
end;

procedure TPaletteForm.HandleClose;
begin
  //inherited HandleClose;
end;

{ TPropertyForm }

procedure TPropertyForm.AfterCreate;
var
  x, x2, w, y, gap : integer;
begin
  inherited AfterCreate;

  WindowPosition := wpUser;

  WindowTitle8 := 'Properties';

  SetDimensions(10,80,250,320);

  x  := 3;
  x2 := x + 90;
  gap := 22;

  w := width - x2 - 3;

  y := 3;

  l1 := CreateLabel(self, x,y, 'Class name:');
  lbClass := CreateLabel(self, x2,y, 'CLASS');
  lbClass.Width := w;
  lbClass.Font := guistyle.LabelFont2;

  inc(y, gap);

  l2 := CreateLabel(self, x,y, 'Name:');
  edName := CreateEdit(self, x2,y,w,0);
  edName.Text8 := 'NAME';
  edName.Anchors := [anLeft,anRight,anTop];

  inc(y, gap);
  lbText := CreateLabel(self, x,y, 'Text/Items:');
  edText := CreateEdit(self, x2,y,w,0);
  edText.Text8 := 'Text';
  edText.Anchors := [anLeft,anRight,anTop];

  btnEdit := CreateButton(self, x2,y,100, 'Edit...', nil);

  inc(y, 2*gap);
  l3 := CreateLabel(self, x,y, 'Left:');
  lbLeft := CreateLabel(self, x2,y, 'Left');
  lbLeft.Width := 50;
  btnLeft := CreateButton(self, x2 + 50, y-2, 30, '...', {$ifdef FPC}@{$endif}maindsgn.OnPropPosEdit);
  btnLeft.Height := 20;
  inc(y, gap);
  l4 := CreateLabel(self, x,y, 'Top:');
  lbTop := CreateLabel(self, x2,y, 'Top');
  lbTop.Width := 50;
  btnTop := CreateButton(self, x2 + 50, y-2, 30, '...', {$ifdef FPC}@{$endif}maindsgn.OnPropPosEdit);
  btnTop.Height := 20;
  inc(y, gap);
  l5 := CreateLabel(self, x,y, 'Width:');
  lbWidth := CreateLabel(self, x2,y, 'w');
  lbWidth.Width := 50;
  btnWidth := CreateButton(self, x2 + 50, y-2, 30, '...', {$ifdef FPC}@{$endif}maindsgn.OnPropPosEdit);
  btnWidth.Height := 20;
  inc(y, gap);
  l6 := CreateLabel(self, x,y, 'Height:');
  lbHeight := CreateLabel(self, x2,y, 'h');
  lbHeight.Width := 50;
  btnHeight := CreateButton(self, x2 + 50, y-2, 30, '...', {$ifdef FPC}@{$endif}maindsgn.OnPropPosEdit);
  btnHeight.Height := 20;

  inc(y, gap);
  l6 := CreateLabel(self, x,y, 'Anchors:');

  cbAL := CreateCheckBox(self, x2, y, 'L');
  cbAT := CreateCheckBox(self, x2+36, y, 'T');
  cbAR := CreateCheckBox(self, x2+2*36, y, 'R');
  cbAB := CreateCheckBox(self, x2+3*36, y, 'B');

  inc(y, gap);
  l7 := CreateLabel(self, x,y, 'Other settings:');

  edOther := TwgMemo.Create(self);
  edOther.SetDimensions(x,y+gap,self.Width - 2*x,self.Height-x-y-gap);
  edOther.Anchors := AllAnchors;
  edOther.Font := guistyle.EditFont2;

//  lbHeight := CreateLabel(self, x2,y, 'h');
//  lbHeight.Width := w;

  edText.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnPropTextChange;
  edName.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnPropNameChange;

  cbAL.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnAnchorChange;
  cbAT.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnAnchorChange;
  cbAR.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnAnchorChange;
  cbAB.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnAnchorChange;

  edOther.OnChange := {$ifdef FPC}@{$endif}maindsgn.OnOtherChange;

  btnEdit.OnClick := {$ifdef FPC}@{$endif}maindsgn.OnEditWidget;

end;

procedure TPropertyForm.HandleClose;
begin
  //inherited HandleClose;
end;

procedure TPropertyForm.HandleKeyPress(var keycode, shiftstate: word; var consumed: boolean);
begin
  if (keycode = KEY_ENTER) or (keycode = KEY_F11) then
  begin
    if maindsgn.selectedform <> nil then GfxActivateWindow(maindsgn.selectedform.Form.WinHandle);
    consumed := true;
  end
  else inherited;
end;

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowPosition := wpUser;

  WindowTitle8 := 'LPTK Visual Form Designer';

  SetDimensions(0,0,550,50);

  l1 := CreateLabel(self, 5,5, 'File name:');

  edFormFile := CreateEdit(self, 5,5+20, 250, 0);
  edFormFile.Text8 := './aanewform.pas';

  btnSave := CreateButton(self, 270,12, 50, 'Save', nil);
  btnLoad := CreateButton(self, 330,12, 50, 'Load', nil);

  l1 := CreateLabel(self, 400,5, 'Grid:');
  chlGrid := CreateChoiceList(self, 400, 5+20, 50, nil);
  chlGrid.Items.Add(str8to16('1'));
  chlGrid.Items.Add(str8to16('4'));
  chlGrid.Items.Add(str8to16('8'));
  chlGrid.FocusItem := 2;

  btnNewForm := CreateButton(self, 460,12, 80, 'New Form', nil);

  btnSave.OnClick := {$ifdef FPC}@{$endif}maindsgn.OnSaveFile;
  btnLoad.OnClick := {$ifdef FPC}@{$endif}maindsgn.OnLoadFile;

  btnNewForm.OnClick := {$ifdef FPC}@{$endif}maindsgn.OnNewForm;
end;

{ TInsertCustomForm }

procedure TInsertCustomForm.AfterCreate;
begin
  inherited;
  WindowPosition := wpScreenCenter;

  WindowTitle8 := 'Insert Custom Widget';

  SetDimensions(0,0,300,100);

  l1 := CreateLabel(self, 8,4, 'Class name:');
  edClass := CreateEdit(self, 8,24, 150, 0);
  edClass.Text8 := 'Twg';
  l2 := CreateLabel(self, 8,48, 'Name:');
  edName := CreateEdit(self, 8,68, 150, 0);
  btnOK := CreateButton(self,180,20,100, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel := CreateButton(self,180,52,100, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);
end;

procedure TInsertCustomForm.OnButtonClick(sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

{ TNewFormForm }

procedure TNewFormForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowPosition := wpScreenCenter;

  SetDimensions(0,0,286,66);
  WindowTitle8 := 'New Form';

  l1 := CreateLabel(self, 8,8, 'Form name:');
  edName := CreateEdit(self, 8,28, 180, 0);
  edName.Text8 := 'frm';
  btnOK := CreateButton(self,196,8,80, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel := CreateButton(self,196,36,80, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);
end;

procedure TNewFormForm.OnButtonClick(sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

{ TEditPositionForm }

procedure TEditPositionForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowPosition := wpUser;

  SetDimensions(0,0,186,66);
  WindowTitle8 := 'Position';

  lbPos := CreateLabel(self, 8,8, 'Pos:      ');
  edPos := CreateEdit(self, 8,28, 80, 0);
  btnOK := CreateButton(self,96,8,80, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel := CreateButton(self,96,36,80, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);
end;

procedure TEditPositionForm.OnButtonClick(sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

{ TWidgetOrderForm }

procedure TWidgetOrderForm.AfterCreate;
begin
  inherited AfterCreate;

  WindowPosition := wpScreenCenter;

  SetDimensions(0,0,322,258);
  WindowTitle8 := 'Widget order';

  l1 := CreateLabel(self, 4,4, 'Form widget order:');

  list := TwgTextListBox.Create(self);
  list.SetDimensions(4,24,220,228);

  btnOK := CreateButton(self,232,24,80, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel := CreateButton(self,232,52,80, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);

  btnUP := CreateButton(self,232,108,80, 'UP', {$ifdef FPC}@{$endif}OnButtonClick);
  btnDOWN := CreateButton(self,232,136,80, 'DOWN', {$ifdef FPC}@{$endif}OnButtonClick);

end;

procedure TWidgetOrderForm.OnButtonClick(sender: TObject);
var
  i : integer;
begin
  if Sender = btnOK then ModalResult := 1
  else if Sender = btnCancel then ModalResult := 2
  else
  begin
    // up / down
    i := list.FocusItem;
    if i < 1 then exit;

    if Sender = btnUP then
    begin
      if i > 1 then
      begin
        list.Items.Move(i-1,i-2);
        list.FocusItem := i-1;
      end;
    end
    else if Sender = btnDOWN then
    begin
      if i < list.Items.Count then
      begin
        list.Items.Move(i-1,i);
        list.FocusItem := i+1;
      end;
    end;
  end;
end;

{ TVFDDialogBase }

procedure TVFDDialog.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  if keycode = KEY_ESC then
  begin
    ModalResult := 2;
    consumed := true;
  end
  else inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

end.
