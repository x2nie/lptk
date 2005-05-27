unit newformdesigner;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, gfxbase, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton, wgedit,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox, gfxmenu,
  vfdwidgetclass, vfdwidgets,
  vfdprops;

type

  TwgPaletteButton = class(TwgButton)
  public
    VFDWidget : TVFDWidgetClass;
  end;

  TwgPalette = class(TWidget)
  end;

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnOpen : TwgButton;
    MainMenu : TMenuBar;
    btnSave : TwgButton;
    wgpalette : TwgPalette;
    chlPalette : TwgChoiceList;
    {@VFD_HEAD_END: frmMain}

    FileMenu : TPopupMenu;
    FormMenu : TPopupMenu;
    SetMenu : TPopupMenu;

    function GetSelectedWidget : TVFDWidgetClass;
    procedure SetSelectedWidget(wgc : TVFDWidgetClass);

    procedure AfterCreate; override;

    procedure OnPaletteClick(sender : TObject);

    property SelectedWidget : TVFDWidgetClass read GetSelectedWidget write SetSelectedWidget;

  end;

  TPropertyList = class
  private
    FList : TList;
  public
    Widget : TWidget;

    constructor Create;
    destructor Destroy; override;

    function GetCount : integer;
    procedure Clear;

    property Count : integer read GetCount;

    procedure AddItem(aProp : TVFDWidgetProperty);

    function GetItem(index : integer) : TVFDWidgetProperty;


  end;

  TwgPropertyList = class(TwgListBox)
  public
    Props : TPropertyList;

    NameWidth : integer;

    editor : TVFDPropertyEditor;

    NameDrag : boolean;
    NameDragPos : integer;

    procedure ReleaseEditor;
    procedure AllocateEditor;

    constructor Create(AOwner : TComponent); override;

    function ItemCount : integer; override;
    function RowHeight : integer; override;
    procedure DrawItem(num : integer; rect : TGfxRect; flags : integer); override;

    procedure OnRowChange(sender : TObject);

    procedure OnUpdateProperty(sender : TObject);

    procedure RealignEditor;

    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;

    procedure DoSetFocus; override;
    procedure DoKillFocus; override;

  end;

  TfrmProperties = class(TGfxForm)
  public
    l1,l2,l3,l4,l5,l6,l7,l8 : TwgLabel;

    lbClass : TwgLabel;
    edName  : TwgEdit;
    edOther : TwgMemo;

    btnTop, btnLeft, btnWidth, btnHeight : TwgButton;

    btnAnLeft, btnAnTop, btnAnRight, btnAnBottom : TwgButton;

    lstProps : TwgPropertyList;

    procedure AfterCreate; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
  end;

{@VFD_NEWFORM_DECL}

var
  frmProperties : TfrmProperties;
  frmMain : TfrmMain;

  PropList : TPropertyList;

implementation

uses vfdmain;

Const
  vfd_anchorbottom : Array[0..121] of byte = (
      66, 77,122,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0, 15,  0,  0,  0, 15,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      60,  0,  0,  0,235, 10,  0,  0,235, 10,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,255,255,255,  0,  0,  0,  0,  0,255,254,  0,  0,128,  2,
       0,  0,254,254,  0,  0,252,126,  0,  0,248, 62,  0,  0,240, 30,  0,
       0,255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,
     255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,255,
     254,  0,  0);

  vfd_anchorleft : Array[0..121] of byte = (
      66, 77,122,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0, 15,  0,  0,  0, 15,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      60,  0,  0,  0,235, 10,  0,  0,235, 10,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,255,255,255,  0,  0,  0,  0,  0,255,254,  0,  0,191,254,
       0,  0,191,254,  0,  0,191,254,  0,  0,187,254,  0,  0,179,254,  0,
       0,163,254,  0,  0,131,254,  0,  0,163,254,  0,  0,179,254,  0,  0,
     187,254,  0,  0,191,254,  0,  0,191,254,  0,  0,191,254,  0,  0,255,
     254,  0,  0);

  vfd_anchorright : Array[0..121] of byte = (
      66, 77,122,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0, 15,  0,  0,  0, 15,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      60,  0,  0,  0,235, 10,  0,  0,235, 10,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,255,255,255,  0,  0,  0,  0,  0,255,254,  0,  0,255,250,
       0,  0,255,250,  0,  0,255,250,  0,  0,255,186,  0,  0,255,154,  0,
       0,255,138,  0,  0,255,130,  0,  0,255,138,  0,  0,255,154,  0,  0,
     255,186,  0,  0,255,250,  0,  0,255,250,  0,  0,255,250,  0,  0,255,
     254,  0,  0);

  vfd_anchortop : Array[0..121] of byte = (
      66, 77,122,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0, 15,  0,  0,  0, 15,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      60,  0,  0,  0,235, 10,  0,  0,235, 10,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,255,255,255,  0,  0,  0,  0,  0,255,254,  0,  0,255,254,
       0,  0,255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,255,254,  0,
       0,255,254,  0,  0,255,254,  0,  0,255,254,  0,  0,240, 30,  0,  0,
     248, 62,  0,  0,252,126,  0,  0,254,254,  0,  0,128,  2,  0,  0,255,
     254,  0,  0);

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
var
  n : integer;
  x : integer;
  wgc : TVFDWidgetClass;
  btn : TwgPaletteButton;
  mi : TMenuItem;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(0,0,506,87);
  WindowTitle8 := 'frmMain';
  WindowPosition := wpUser;

  MainMenu := TMenuBar.Create(self);
  with MainMenu do
  begin
    SetDimensions(0,0,500,24);
  end;

  btnOpen := TwgButton.Create(self);
  with btnOpen do
  begin
    SetDimensions(4,40,25,24);
    Text := u8('');
    ImageName := 'stdimg.open';
    Focusable := false;
    OnClick := maindsgn.OnLoadFile;
  end;

  btnSave := TwgButton.Create(self);
  with btnSave do
  begin
    SetDimensions(32,40,25,24);
    Text := u8('');
    ImageName := 'stdimg.save';
    Focusable := false;
    OnClick := maindsgn.OnSaveFile;
  end;

  wgpalette := TwgPalette.Create(self);
  with wgpalette do
  begin
    SetDimensions(116,28,384,28);
  end;

  chlPalette := TwgChoiceList.Create(self);
  with chlPalette do
  begin
    SetDimensions(116,60,386,22);
    Items.Add(u8('-'));
  end;

  {@VFD_BODY_END: frmMain}


  wgpalette.Focusable := false;

  x := 0;
  for n:=1 to VFDWidgetCount do
  begin
    wgc := VFDWidget(n);
    btn := TwgPaletteButton.Create(wgpalette);
    btn.VFDWidget := wgc;
    btn.SetDimensions(x,0,30,28);
    btn.ImageName := wgc.WidgetIconName;
    btn.ImageMargin := -1;
    btn.Text := '';
    btn.Focusable := false;
    btn.OnClick := OnPaletteClick;
    btn.GroupIndex := 1;
    btn.AllowAllUp := true;
    chlPalette.Items.AddObject(u8(wgc.WidgetClass.ClassName),wgc);

    inc(x,32);
  end;

  filemenu := TPopupMenu.Create(self);
  with filemenu do
  begin
    mi := AddMenuItem8('New','',nil);
    mi.OnClick := maindsgn.OnNewFile;
    mi := AddMenuItem8('Open','',nil);
    mi.OnClick := maindsgn.OnLoadFile;
    mi := AddMenuItem8('Save','',nil);
    mi.OnClick := maindsgn.OnSaveFile;
    AddMenuItem8('-','',nil);
    mi := AddMenuItem8('New Form...','',nil);
    mi.OnClick := maindsgn.OnNewForm;
    AddMenuItem8('-','',nil);
    mi := AddMenuItem8('Exit','',nil);
    mi.OnClick := maindsgn.OnExit;
  end;

  formmenu := TPopupMenu.Create(self);
  with formmenu do
  begin
    mi := AddMenuItem8('Widget Order...','',nil);
    mi.OnClick := maindsgn.OnEditWidgetOrder;
    AddMenuItem8('-','',nil);
    AddMenuItem8('Edit special...','',nil);
  end;

  setmenu := TPopupMenu.Create(self);
  with setmenu do
  begin
    mi := AddMenuItem8('General options ...','',nil);
    mi.OnClick := maindsgn.OnOptionsClick;
  end;

  MainMenu.AddMenuItem8('&File',nil).SubMenu := filemenu;
  MainMenu.AddMenuItem8('&Settings',nil).SubMenu := setmenu;
  MainMenu.AddMenuItem8('Fo&rm',nil).SubMenu := formmenu;

end;

procedure TfrmMain.OnPaletteClick(sender: TObject);
var
  s : string;
  i : integer;
begin
  if TwgPaletteButton(sender).Down then
  begin
    s := TwgPaletteButton(sender).VFDWidget.WidgetClass.ClassName;
    i := chlPalette.Items.IndexOf(u8(s));
    if i >= 0 then chlPalette.FocusItem := i+1;
  end
  else chlPalette.FocusItem := 1;

end;

{ TfrmProperties }

procedure TfrmProperties.AfterCreate;
var
  x, x2, w, y, gap : integer;
begin
  inherited;

  GfxLibAddBMP(
            'vfd.anchorleft',
            @vfd_anchorleft,
      sizeof(vfd_anchorleft)
              );

  GfxLibAddBMP(
            'vfd.anchorright',
            @vfd_anchorright,
      sizeof(vfd_anchorright)
              );

  GfxLibAddBMP(
            'vfd.anchortop',
            @vfd_anchortop,
      sizeof(vfd_anchortop)
              );

  GfxLibAddBMP(
            'vfd.anchorbottom',
            @vfd_anchorbottom,
      sizeof(vfd_anchorbottom)
              );


  WindowPosition := wpUser;

  WindowTitle8 := 'Properties';

  SetDimensions(0,120,250,450);

  x  := 3;
  x2 := x + 50;
  gap := 20;

  w := width - x2;

  y := 3;

  l1 := CreateLabel(self, 0,y, 'Class:');
  lbClass := CreateLabel(self, x2,y, 'CLASS');
  lbClass.Width := w;
  lbClass.FontName := '#Label2';
  lbClass.Anchors := [anLeft,anRight,anTop];
  inc(y, gap);

  l2 := CreateLabel(self, 0,y+1, 'Name:');
  edName := CreateEdit(self, x2,y,w,0);
  edName.Text8 := 'NAME';
  edName.Anchors := [anLeft,anRight,anTop];
  edName.OnChange := maindsgn.OnPropNameChange;

  inc(y, gap+5);

  lstProps := TwgPropertyList.Create(self);
  lstProps.SetDimensions(0,y,Width,self.height-y-220);
  lstProps.Anchors := AllAnchors;
  lstProps.Props := PropList;
  lstProps.Props.Widget := edName;

  y := lstProps.Bottom+5;

  //inc(y, gap+5);

  l3 := CreateLabel(self, 3,y+1, 'Left:');
  l3.Anchors := [anLeft,anBottom];
  btnLeft := CreateButton(self, 50, y-2, 48, '1234', maindsgn.OnPropPosEdit);
  with btnLeft do
  begin
    Height := 22;
    btnLeft.Anchors := [anLeft,anBottom];
    Focusable := false;
  end;
  l4 := CreateLabel(self, 110, y, 'Top:');
  l4.Anchors := [anLeft,anBottom];
  btnTop := CreateButton(self, 160, y-2, 48, '45', maindsgn.OnPropPosEdit);
  with btnTop do
  begin
    Height := 22;
    btnLeft.Anchors := [anLeft,anBottom];
    Focusable := false;
  end;
  inc(y, gap+5);
  l5 := CreateLabel(self, 3,y+1, 'Width:');
  l5.Anchors := [anLeft,anBottom];
  btnWidth := CreateButton(self, 50, y-2, 48, '1234', maindsgn.OnPropPosEdit);
  with btnWidth do
  begin
    Height := 22;
    btnLeft.Anchors := [anLeft,anBottom];
    Focusable := false;
  end;
  l6 := CreateLabel(self, 110, y, 'Height:');
  l6.Anchors := [anLeft,anBottom];
  btnHeight := CreateButton(self, 160, y-2, 48, '45', maindsgn.OnPropPosEdit);
  with btnHeight do
  begin
    Height := 22;
    btnLeft.Anchors := [anLeft,anBottom];
    Focusable := false;
  end;
  inc(y, gap+5);

  l8 := CreateLabel(self, 3,y+1, 'Anchors:');
  l8.Anchors := [anLeft,anBottom];

  x := 64;

  btnAnLeft := CreateButton(self,x,y-2,28,'',nil);
  with btnAnLeft do
  begin
    ImageName := 'vfd.anchorleft';
    AllowAllUp := true;
    GroupIndex := 1;
    Focusable := false;
    Anchors := [anLeft,anBottom];
    OnClick := maindsgn.OnAnchorChange;
  end;

  inc(x,30);
  btnAnTop := CreateButton(self,x,y-2,26,'',nil);
  with btnAnTop do
  begin
    ImageName := 'vfd.anchortop';
    AllowAllUp := true;
    GroupIndex := 2;
    Focusable := false;
    Anchors := [anLeft,anBottom];
    OnClick := maindsgn.OnAnchorChange;
  end;

  inc(x,30);
  btnAnBottom := CreateButton(self,x,y-2,26,'',nil);
  with btnAnBottom do
  begin
    ImageName := 'vfd.anchorbottom';
    AllowAllUp := true;
    GroupIndex := 3;
    Focusable := false;
    Anchors := [anLeft,anBottom];
    OnClick := maindsgn.OnAnchorChange;
  end;

  inc(x,30);
  btnAnRight := CreateButton(self,x,y-2,26,'',nil);
  with btnAnRight do
  begin
    ImageName := 'vfd.anchorright';
    AllowAllUp := true;
    GroupIndex := 4;
    Focusable := false;
    Anchors := [anLeft,anBottom];
    OnClick := maindsgn.OnAnchorChange;
  end;

  y := btnAnRight.Bottom + 5;

  l7 := CreateLabel(self, 0,y, 'Unknown lines:');
  l7.Anchors := [anLeft,anBottom];
  inc(y,16);

  edOther := TwgMemo.Create(self);
  edOther.SetDimensions(0,y,self.Width,self.Height-y);
  edOther.Anchors := [anLeft,anRight,anBottom];
  edOther.FontName := '#Edit2';
  edOther.OnChange := maindsgn.OnOtherChange;

end;

procedure TfrmProperties.HandleKeyPress(var keycode, shiftstate: word;
  var consumed: boolean);
begin
  if (keycode = KEY_ENTER) or (keycode = KEY_F11) then
  begin
    if maindsgn.selectedform <> nil then GfxActivateWindow(maindsgn.selectedform.Form.WinHandle);
    consumed := true;
  end
  else inherited;
end;

{ TPropertyList }

procedure TPropertyList.AddItem(aProp : TVFDWidgetProperty);
begin
{
  result := TPropertyLine.Create;
  result.name := aPropName;
  result.propclass := apropclass;
  result.value := aPropName;
}
  FList.Add(aProp);
end;

procedure TPropertyList.Clear;
//var
//  n : integer;
begin
  //for n:=0 to FList.Count-1 do TObject(FList[n]).Free;
  FList.Clear;
end;

constructor TPropertyList.Create;
begin
  FList := TList.Create;
  Widget := nil;
end;

destructor TPropertyList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPropertyList.GetCount: integer;
begin
  result := FList.Count;
end;

function TPropertyList.GetItem(index: integer): TVFDWidgetProperty;
begin
  if (index < 1) or (index > Count)
    then result := nil
    else result := TVFDWidgetProperty(FList[index-1]);
end;

{ TwgPropertyList }

constructor TwgPropertyList.Create(AOwner : TComponent);
begin
  inherited;
  NameWidth := 80;
  editor := nil;
  OnChange := OnRowChange;
  BackgroundColor := clWindowBackground;
  NameDrag := false;
  //FontName := 'arial-10:antialias=false';
end;

procedure TwgPropertyList.OnRowChange(sender : TObject);
begin
  AllocateEditor;
end;

procedure TwgPropertyList.DrawItem(num: integer; rect: TGfxRect; flags: integer);
var
  x,y,fy : integer;
  s : string;
  prop : TVFDWidgetProperty;
  r : TGfxRect;
begin
  //inherited;
  prop := Props.GetItem(num);
  if prop = nil then Exit;

  x := rect.left;
  y := rect.top;
  fy := y + rect.height div 2 - FFont.Height div 2;

  s := u8(prop.name);
  Canvas.DrawString16(x+1,fy,s);

  inc(x,NameWidth);
  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(x,rect.top,x,rect.bottom);
  inc(x);
  // Drawing the contents
  r.SetRect(x,y,rect.right-x,rect.height);
  canvas.SetColor(FBackgroundColor);
  canvas.FillRect(r);
  Canvas.SetTextColor(clText1);
  inc(r.left,2);
  dec(r.width,2);
  prop.DrawValue(props.Widget,canvas,r,flags);

  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(0,rect.bottom,rect.right,rect.bottom);
end;

function TwgPropertyList.ItemCount: integer;
begin
  result := Props.Count;
end;

function TwgPropertyList.RowHeight: integer;
begin
  result := 22;
end;

procedure TwgPropertyList.OnUpdateProperty(sender: TObject);
begin
  writeln('updating property...');
  editor.StoreValue(props.Widget);
  props.Widget.UpdateWindowPosition;
end;

procedure TwgPropertyList.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
begin
  if not NameDrag then
  begin
    if (x >= FMargin+NameWidth - 2) and (x <= FMargin+NameWidth + 2)
      then MouseCursor := CUR_DIR_EW
      else MouseCursor := CUR_DEFAULT;
  end
  else
  begin
    NameWidth := x - FMargin;
    ReAlignEditor;
    RePaint;
  end;
  inherited;
end;

procedure TwgPropertyList.HandleMouseDown(x, y: integer; button, shiftstate: word);
begin
  if MouseCursor = CUR_DIR_EW then
  begin
    NameDrag := true
    //NameDragPos := x;
  end
  else inherited;
end;

procedure TwgPropertyList.HandleMouseUp(x, y: integer; button, shiftstate: word);
begin
  if NameDrag then NameDrag := false
              else inherited;
end;

procedure TwgPropertyList.RealignEditor;
var
  x : integer;
begin
  if editor = nil then Exit;
  x := 3+NameWidth;
  editor.SetDimensions(x,editor.Top,self.Width-ScrollBarWidth-x,editor.Height);
end;

function TfrmMain.GetSelectedWidget: TVFDWidgetClass;
begin
  if chlPalette.FocusItem > 1 then
  begin
    result := TVFDWidgetClass(chlPalette.Items.Objects[chlPalette.FocusItem-1]);
  end
  else result := nil;
end;

procedure TfrmMain.SetSelectedWidget(wgc: TVFDWidgetClass);
var
  n : integer;
begin
  if wgc = nil then
  begin
    chlPalette.FocusItem := 1;
    for n:=0 to wgpalette.ComponentCount-1 do
      if wgpalette.Components[n] is TwgPaletteButton then TwgPaletteButton(wgpalette.Components[n]).Down := false;
  end;
end;

procedure TwgPropertyList.ReleaseEditor;
begin
  self.ActiveWidget := nil;
  if editor <> nil then editor.Free;
  editor := nil;
end;

procedure TwgPropertyList.DoKillFocus;
begin
  inherited;
  Editor.Visible := true;
end;

procedure TwgPropertyList.DoSetFocus;
begin
  inherited;
  if Editor <> nil then Editor.Visible := true else AllocateEditor;
end;

procedure TwgPropertyList.AllocateEditor;
var
  x,y : integer;
  prop : TVFDWidgetProperty;
begin
  prop := Props.GetItem(FFocusItem);
  if prop = nil then Exit;

  self.ActiveWidget := nil;
  if editor <> nil then editor.Free;

  editor := prop.CreateEditor(Self);
  x := 3+NameWidth;
  y := FMargin+(FFocusItem-FFirstItem)*RowHeight;
  editor.SetDimensions(x,y-1,Width-ScrollBarWidth-x,RowHeight);
  editor.CreateLayout;
  editor.OnUpdate := OnUpdateProperty;
  editor.LoadValue(Props.Widget);
  editor.ShowWidget;

  self.ActiveWidget := editor;
end;

end.
