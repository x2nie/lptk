unit vfdprops;

{$ifdef FPC}
  {$mode delphi}
{$endif}
{$H+}

interface

uses Classes, SysUtils, gfxbase, gfxwidget, schar16, gfxstyle, vfdwidgetclass,
  wgLabel, wgEdit, wgButton, wgChoiceList;

type

  TPropertyString16 = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyString8 = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyInteger = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyEnum = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyStringList = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget; const ident : string) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;

    procedure OnExternalEdit(wg : TWidget); override;
  end;



  TGPEType = (gptInteger, gptString8, gptString16);

  TGeneralPropertyEditor = class(TVFDPropertyEditor)
  public
    etype : TGPEType;

    edit : TwgEdit;

    procedure CreateLayout; override;

    procedure LoadValue(wg : TWidget); override;
    procedure StoreValue(wg : TWidget); override;

    procedure LoadIntValue(wg : TWidget);
    procedure StoreIntValue(wg : TWidget);

    procedure LoadStrValue(wg : TWidget);
    procedure StoreStrValue(wg : TWidget);
  end;

  TChoicePropertyEditor = class(TVFDPropertyEditor)
  public
    chl : TwgChoiceList;

    procedure CreateLayout; override;

    procedure LoadValue(wg : TWidget); override;
    procedure StoreValue(wg : TWidget); override;
  end;

  TExternalPropertyEditor = class(TVFDPropertyEditor)
  public
    btnEdit : TwgButton;
    Widget  : TWidget;

    procedure CreateLayout; override;

    procedure RePaint; override;

    procedure LoadValue(wg : TWidget); override;
    procedure StoreValue(wg : TWidget); override;

    procedure OnEditClick(sender : TObject);
  end;

procedure EditStringList(sl : TStringList);

procedure GetEnumPropValueList(wg : TObject; const propname : string; sl : TStringList);

implementation

uses TypInfo, vfdformparser, vfdeditors;

procedure EditStringList(sl : TStringList);
var
  frmie : TItemEditorForm;
begin
  frmie := TItemEditorForm.Create(nil);
  //GfxGetAbsolutePosition(PropertyForm.btnEdit.WinHandle, PropertyForm.btnEdit.width, 0, ax,ay);
  //frmie.Left := ax;
  //frmie.Top := ay;

  frmie.edItems.Lines.Assign(sl);
  if frmie.ShowModal = 1 then
  begin
    sl.Assign(frmie.edItems.Lines);
  end;
  frmie.Free;
end;

procedure GetEnumPropValueList(wg : TObject; const propname : string; sl : TStringList);
var
  pi : PPropInfo;
  P: ^ShortString;
  T: PTypeData;
  n : integer;
begin
  pi := GetPropInfo(wg, propname);
{$ifdef FPC}
  T := GetTypeData(pi^.PropType);
{$else}
  T := GetTypeData(pi^.PropType^);
{$endif}
  P := @T^.NameList;

  for n := 0 to T^.MaxValue do
  begin
    sl.Add(P^);
    Inc(Integer(P), Length(P^) + 1);
  end;
end;

{ TPropertyString16 }

function TPropertyString16.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(result) do
  begin
    etype := gptString16;
  end;
end;

function TPropertyString16.GetPropertySource(wg: TWidget; const ident : string): string;
begin
  result := ident+Name+' := u8(' + QuotedStr(str16to8( GetStrProp(wg,Name) )) + ');'#10;
end;

function TPropertyString16.GetValueText(wg: TWidget): string;
begin
  result := GetStrProp(wg, Name);
end;

function TPropertyString16.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  s, sval : string;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then Exit;

  result := CheckSymbol(s, ':=');
  if result then
  begin
    sval := GetStringValue(s);
    result := CheckSymbol(s, ';');
  end;

  if result then SetStrProp(wg,Name,sval);
end;

{ TPropertyString8 }

function TPropertyString8.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(result) do
  begin
    etype := gptString8;
  end;
end;

function TPropertyString8.GetPropertySource(wg: TWidget; const ident : string): string;
begin
  result := ident+Name+' := '+QuotedStr( GetStrProp(wg,Name) ) + ';'#10;
end;

function TPropertyString8.GetValueText(wg: TWidget): string;
begin
  result := u8( GetStrProp(wg, Name) );
end;

function TPropertyString8.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  s, sval : string;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then Exit;

  result := CheckSymbol(s, ':=');
  if result then
  begin
    sval := GetStringValue(s);
    result := CheckSymbol(s, ';');
  end;

  if result then SetStrProp(wg,Name,sval);
end;


{ TPropertyInteger }

function TPropertyInteger.CreateEditor(AOwner: TComponent) : TVFDPropertyEditor;
begin
  result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(result) do
  begin
    etype := gptInteger;
  end;
end;

function TPropertyInteger.GetPropertySource(wg: TWidget; const ident : string): string;
begin
  result := ident+Name+' := '+IntToStr( GetOrdProp(wg,Name) ) + ';'#10;
end;

function TPropertyInteger.GetValueText(wg: TWidget): string;
begin
  result := u8( IntToStr( GetOrdProp(wg,Name) ) );
end;

function TPropertyInteger.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  s : string;
  ival : integer;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then Exit;

  result := CheckSymbol(s, ':=');
  if result then
  begin
    ival := GetIntValue(s);
    result := CheckSymbol(s, ';');
  end
  else ival := 0;

  if result then SetOrdProp(wg,Name,ival);
end;

{ TGeneralPropertyEditor }

procedure TGeneralPropertyEditor.CreateLayout;
begin
  self.Anchors := [anTop,anLeft,anRight];
  Edit := TwgEdit.Create(self);
  Edit.SetDimensions(0,0,width,Height);
  Edit.Anchors := self.Anchors;
  Edit.OnChange := UpdateProperty;
end;

procedure TGeneralPropertyEditor.LoadIntValue(wg: TWidget);
begin
  edit.Text8 := IntToStr( GetOrdProp(wg,prop.Name) );
end;

procedure TGeneralPropertyEditor.LoadStrValue(wg: TWidget);
var
  s : string;
begin
  s := GetStrProp(wg,prop.Name);
  if etype = gptString16 then edit.Text := s else edit.Text8 := s;
end;

procedure TGeneralPropertyEditor.LoadValue(wg: TWidget);
begin
  case etype of
    gptInteger : LoadIntValue(wg);
  else
    LoadStrValue(wg);
  end;
end;

procedure TGeneralPropertyEditor.StoreIntValue(wg: TWidget);
var
  i : integer;
begin
  try
    i := StrToInt( edit.Text8 );
    SetOrdProp(wg,Prop.Name,i);
  except
    // error
  end;
end;

procedure TGeneralPropertyEditor.StoreStrValue(wg: TWidget);
var
  s : string;
begin
  if etype = gptString16 then s := edit.Text else s := edit.Text8;
  SetStrProp(wg,prop.Name,s);
end;

procedure TGeneralPropertyEditor.StoreValue(wg: TWidget);
begin
  case etype of
    gptInteger : StoreIntValue(wg);
  else
    StoreStrValue(wg);
  end;
end;

{ TPropertyStringList }

function TPropertyStringList.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TExternalPropertyEditor.Create(AOwner,self);
end;

function TPropertyStringList.GetPropertySource(wg: TWidget; const ident : string): string;
var
  sl : TStringList;
  f : integer;
begin
  sl := TStringList(GetObjectProp(wg,Name,TStrings));

  result := '';

  for f := 0 to sl.Count - 1 do
  begin
    result := result + ident + Name + '.Add(u8('+QuotedStr(u16u8safe(sl.Strings[f]))+'));'#10;
  end;
end;

function TPropertyStringList.GetValueText(wg: TWidget): string;
var
  sl : TStringList;
begin
  sl := TStringList(GetObjectProp(wg,Name,TStrings));
  result := u8('['+IntToStr(sl.Count)+' lines]');
end;

procedure TPropertyStringList.OnExternalEdit(wg: TWidget);
var
  sl : TStringList;
begin
  sl := TStringList(GetObjectProp(wg,Name,TStrings));
  EditStringList(sl);
end;

function TPropertyStringList.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  s : string;
  sval : string;
  sl : TStringList;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then Exit;

  result := CheckSymbol(s, '.');
  result := result and (UpperCase(GetIdentifier(s)) = 'ADD');
  result := result and CheckSymbol(s, '(');
  if result then
  begin
    sval := GetStringValue(s);
    result := result and CheckSymbol(s, ')');
    result := result and CheckSymbol(s, ';');
  end;

  if result then
  begin
    sl := TStringList(GetObjectProp(wg,Name,TStrings));
    sl.Add(sval)
  end;
end;

{ TExternalPropertyEditor }

procedure TExternalPropertyEditor.CreateLayout;
begin
  inherited;
  Widget := nil;
  self.Anchors := [anTop,anLeft,anRight];

  btnEdit := TwgButton.Create(self);
  with btnEdit do
  begin
    Height := self.Height;
    Width := 24;
    Top := 0;
    Left := self.Width-width;
    Text8 := '...';
    UpdateWindowPosition;
    Anchors := [anTop,anRight];
    OnClick := OnEditClick;
  end;
end;

procedure TExternalPropertyEditor.LoadValue(wg: TWidget);
begin
  Widget := wg;
  RePaint;
end;

procedure TExternalPropertyEditor.OnEditClick(sender: TObject);
begin
  if widget = nil then Exit;
  prop.OnExternalEdit(widget);
  if widget.Windowed then widget.RePaint;
end;

procedure TExternalPropertyEditor.RePaint;
var
  r : TGfxRect;
begin
  if not Windowed then Exit;
  if widget = nil then Exit;
  canvas.Clear(clBoxColor);
  canvas.GetWinRect(r);
  canvas.SetTextColor(clText1);
  prop.DrawValue(Widget,canvas,r,0);
end;

procedure TExternalPropertyEditor.StoreValue(wg: TWidget);
begin
  // nothing
end;

{ TPropertyEnum }

function TPropertyEnum.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TChoicePropertyEditor.Create(AOwner, self);
end;

function TPropertyEnum.GetValueText(wg: TWidget): string;
begin
  result := u8(GetEnumProp(wg,Name));
end;

function TPropertyEnum.GetPropertySource(wg: TWidget; const ident: string): string;
begin
  result := ident + Name+' := '+GetEnumProp(wg,Name)+';'#10;
end;

function TPropertyEnum.ParseSourceLine(wg: TWidget; const line: string): boolean;
var
  s, sval : string;
begin
  s := line;
  result := false;
  if UpperCase(GetIdentifier(s)) <> UpperCase(Name) then Exit;

  result := CheckSymbol(s, ':=');
  if result then
  begin
    sval := GetIdentifier(s);
    result := CheckSymbol(s, ';');
  end;

  if result then
  begin
    try
      SetEnumProp(wg,Name,sval);
    except
      Writeln('invalid enum value: "'+sval+'" for '+Name); 
      result := false;
    end;
  end;
end;

{ TChoicePropertyEditor }

procedure TChoicePropertyEditor.CreateLayout;
begin
  self.Anchors := [anTop,anLeft,anRight];
  chl := TwgChoicelist.Create(self);
  chl.SetDimensions(0,0,width,Height);
  chl.Anchors := self.Anchors;
  chl.OnChange := UpdateProperty;
end;

procedure TChoicePropertyEditor.LoadValue(wg: TWidget);
var
  sv : string;
  i, fi : integer;
  sl : TStringList;
begin
  sv := GetEnumProp(wg, prop.Name);
  sl := TStringList.Create;
  GetEnumPropValueList(wg,prop.Name,sl);
  fi := 1;
  for i:=0 to sl.Count-1 do
  begin
    chl.Items.Add(u8(sl.Strings[i]));
    if UpperCase(sv) = UpperCase(sl.Strings[i]) then fi := i+1;
  end;
  chl.FocusItem := fi;
  sl.Free;
end;

procedure TChoicePropertyEditor.StoreValue(wg: TWidget);
begin
  SetEnumProp(wg,prop.Name,chl.Text8);
end;

end.
