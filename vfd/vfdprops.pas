unit vfdprops;

interface

uses Classes, SysUtils, gfxbase, gfxwidget, schar16, vfdwidgetclass,
  wgLabel, wgEdit, wgButton;

type

  TPropertyString16 = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyString8 = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
  end;

  TPropertyInteger = class(TVFDWidgetProperty)
  public
    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget) : string; override;

    function GetValueText(wg : TWidget) : string; override;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; override;
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

implementation

uses TypInfo, vfdformparser;

{ TPropertyString16 }

function TPropertyString16.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  result := TGeneralPropertyEditor.Create(AOwner, self);
  with TGeneralPropertyEditor(result) do
  begin
    etype := gptString16;
  end;
end;

function TPropertyString16.GetPropertySource(wg: TWidget): string;
begin
  result := Name+' := u8(' + QuotedStr(str16to8( GetStrProp(wg,Name) )) + ');';
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

function TPropertyString8.GetPropertySource(wg: TWidget): string;
begin
  result := Name+' := '+QuotedStr( GetStrProp(wg,Name) ) + ';';
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

function TPropertyInteger.GetPropertySource(wg: TWidget): string;
begin
  result := Name+' := '+IntToStr( GetOrdProp(wg,Name) ) + ';';
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
  self.Anchors := AllAnchors;
  Edit := TwgEdit.Create(self);
  Edit.SetDimensions(0,0,width,Height);
  Edit.Anchors := AllAnchors;
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

end.
