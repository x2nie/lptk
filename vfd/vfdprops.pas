unit vfdprops;

interface

uses Classes, SysUtils, gfxbase, gfxwidget, schar16, vfdwidgetclass,
  wgLabel, wgEdit, wgButton;

type

  // String16

  TPropertyString16 = class(TVFDWidgetProperty)
  public
    constructor Create; override;

    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget) : string; override;
  end;

  TPropEditorString16 = class(TVFDPropertyEditor)
  public
    Edit : TwgEdit;

    procedure CreateLayout; override;

    procedure SetWidgetProperty(wg : TWidget; propname : string); override;
    procedure GetWidgetProperty(wg : TWidget; propname : string); override;
  end;

  // String8

  TPropertyString8 = class(TVFDWidgetProperty)
  public
    constructor Create; override;

    function ParseSourceLine(wg : TWidget; const line : string) : boolean; override;
    function GetPropertySource(wg : TWidget) : string; override;
  end;

  TPropEditorString8 = class(TVFDPropertyEditor)
  public
    Edit : TwgEdit;

    procedure CreateLayout; override;

    procedure SetWidgetProperty(wg : TWidget; propname : string); override;
    procedure GetWidgetProperty(wg : TWidget; propname : string); override;
  end;

implementation

uses TypInfo, vfdformparser;

{ TPropEditorString16 }

procedure TPropEditorString16.CreateLayout;
begin
  Edit := TwgEdit.Create(self);
  Edit.SetDimensions(Left,Top,width,Height);
  Edit.Anchors := AllAnchors;
end;

procedure TPropEditorString16.GetWidgetProperty(wg: TWidget; propname: string);
begin
  Edit.Text := GetStrProp(wg, propname);
end;

procedure TPropEditorString16.SetWidgetProperty(wg: TWidget; propname: string);
begin
  SetStrProp(wg, propname, Edit.Text);
end;

{ TPropertyString16 }

constructor TPropertyString16.Create;
begin
  inherited;
  FEditorClass := TPropEditorString16;
end;

function TPropertyString16.GetPropertySource(wg: TWidget): string;
begin
  result := Name+' := u8(' + QuotedStr(str16to8( GetStrProp(wg,Name) )) + ');';
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

{ TPropEditorString8 }

procedure TPropEditorString8.CreateLayout;
begin
  Edit := TwgEdit.Create(self);
  Edit.SetDimensions(Left,Top,width,Height);
  Edit.Anchors := AllAnchors;
end;

procedure TPropEditorString8.GetWidgetProperty(wg: TWidget; propname: string);
begin
  Edit.Text8 := GetStrProp(wg, propname);
end;

procedure TPropEditorString8.SetWidgetProperty(wg: TWidget; propname: string);
begin
  SetStrProp(wg, propname, Edit.Text);
end;

{ TPropertyString8 }

constructor TPropertyString8.Create;
begin
  inherited;
  FEditorClass := TPropEditorString8;
end;

function TPropertyString8.GetPropertySource(wg: TWidget): string;
begin
  result := Name+' := '+QuotedStr( GetStrProp(wg,Name) ) + ';';
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


end.
