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

    procedure AfterCreate; override;

    procedure OnPaletteClick(sender : TObject);
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

    constructor Create(AOwner : TComponent); override;

    function ItemCount : integer; override;
    function RowHeight : integer; override;
    procedure DrawItem(num : integer; rect : TGfxRect; flags : integer); override;

    procedure OnRowChange(sender : TObject);

    procedure OnUpdateProperty(sender : TObject);
  end;

  TfrmProperties = class(TGfxForm)
  public
    l1,l2,l3,l4,l5,l6,l7,l8 : TwgLabel;

    lbClass : TwgLabel;
    edName  : TwgEdit;
    edOther : TwgMemo;

    lstProps : TwgPropertyList;

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  frmProperties : TfrmProperties;
  frmMain : TfrmMain;

  PropList : TPropertyList;

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
var
  n : integer;
  x : integer;
  wgc : TVFDWidgetClass;
  btn : TwgPaletteButton;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(0,0,506,87);
  WindowTitle8 := 'frmMain';
  WindowPosition := wpUser;

  btnOpen := TwgButton.Create(self);
  with btnOpen do
  begin
    SetDimensions(4,40,25,24);
    Text := u8('');
    ImageName := 'stdimg.open';
    Focusable := false;
  end;

  MainMenu := TMenuBar.Create(self);
  with MainMenu do
  begin
    SetDimensions(0,0,500,24);
  end;

  btnSave := TwgButton.Create(self);
  with btnSave do
  begin
    SetDimensions(32,40,25,24);
    Text := u8('');
    ImageName := 'stdimg.save';
    Focusable := false;
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
    //btn.AllowDown := true;
    chlPalette.Items.Add(u8(wgc.WidgetClass.ClassName));

    inc(x,32);
  end;

  filemenu := TPopupMenu.Create(self);
  with filemenu do
  begin
    AddMenuItem8('Open','',nil);
    AddMenuItem8('Save','',nil);
    AddMenuItem8('-','',nil);
    AddMenuItem8('New Form...','',nil);
    AddMenuItem8('-','',nil);
    AddMenuItem8('Exit','',nil);
  end;

  formmenu := TPopupMenu.Create(self);
  with formmenu do
  begin
    AddMenuItem8('Widget Order...','',nil);
    AddMenuItem8('-','',nil);
    AddMenuItem8('Edit special...','',nil);
  end;

  MainMenu.AddMenuItem8('&File',nil).SubMenu := filemenu;
  MainMenu.AddMenuItem8('&Settings',nil).SubMenu := nil;
  MainMenu.AddMenuItem8('Fo&rm',nil).SubMenu := formmenu;

end;

procedure TfrmMain.OnPaletteClick(sender: TObject);
var
  s : string;
  i : integer;
begin
  s := TwgPaletteButton(sender).VFDWidget.WidgetClass.ClassName;
  i := chlPalette.Items.IndexOf(u8(s));
  if i >= 0 then chlPalette.FocusItem := i+1;
end;

{ TfrmProperties }

procedure TfrmProperties.AfterCreate;
var
  x, x2, w, y, gap : integer;
begin
  inherited;
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
  inc(y, gap+3);

  lstProps := TwgPropertyList.Create(self);
  lstProps.SetDimensions(0,y,Width,self.height-y-90);
  lstProps.Anchors := AllAnchors;
  lstProps.Props := PropList;
  lstProps.Props.Widget := edName;

  y := height - 90 + 1;

  l7 := CreateLabel(self, 0,y, 'Unknown lines:');
  l7.Anchors := [anLeft,anBottom];
  inc(y,16);

  edOther := TwgMemo.Create(self);
  edOther.SetDimensions(0,y,self.Width,self.Height-y);
  edOther.Anchors := [anLeft,anRight,anBottom];
  edOther.FontName := '#Edit2';

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
  //FontName := 'arial-10:antialias=false';
end;

procedure TwgPropertyList.OnRowChange(sender : TObject);
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

  Canvas.SetColor(clHilite1);
  inc(x,NameWidth);
  Canvas.DrawLine(x,rect.top,x,rect.bottom);
  inc(x,3);

  // Drawing the contents

  r.SetRect(x,y,rect.right-x,rect.height);

  prop.DrawValue(props.Widget,canvas,r,flags);

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

end.
