unit vfdwidgetclass;

{$ifdef FPC}
  {$mode delphi}
{$endif}
{$H+}

interface

uses Classes, SysUtils, gfxbase, gfxwidget, schar16,
  wgLabel, wgEdit;

type
  TWidgetClass = class of TWidget;

  TVFDWidgetProperty = class;

  TVFDPropertyEditor = class(TWidget)
  private
    FProp : TVFDWidgetProperty;
  public
    OnUpdate : TNotifyEvent;

    procedure UpdateProperty(sender : TObject);

    property Prop : TVFDWidgetProperty read FProp;

    constructor Create(AOwner : TComponent; aprop : TVFDWidgetProperty); reintroduce;
  public
    // virtuals
    procedure CreateLayout; virtual;

    procedure LoadValue(wg : TWidget); virtual;
    procedure StoreValue(wg : TWidget); virtual;

  end;

  TVFDWidgetProperty = class
  public
    Name : string;
    Description : string;
  public
    // Virtuals
    constructor Create(aName : string); virtual;

    function ParseSourceLine(wg : TWidget; const line : string) : boolean; virtual;
    function GetPropertySource(wg : TWidget; const ident : string) : string; virtual;

    // Property editing
    function GetValueText(wg : TWidget) : string; virtual;
    procedure DrawValue(wg : TWidget; canvas : TGfxCanvas; rect: TGfxRect; flags: integer); virtual;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor; virtual;

    procedure OnExternalEdit(wg : TWidget); virtual;
  end;

  TVFDPropertyClass = class of TVFDWidgetProperty;

  TVFDWidgetClass = class
  private
    FProps : TList;
  public
    WidgetClass : TWidgetClass;
    Description : string;
    WidgetIconName : string;
    NameBase : string;
    Container : boolean;

    constructor Create(aClass : TWidgetClass);
    destructor Destroy; override;

    function AddProperty(apropname : string; apropclass : TVFDPropertyClass; desc : string) : TVFDWidgetProperty;

    function PropertyCount : integer;
    function GetProperty(ind : integer) : TVFDWidgetProperty;

    function CreateWidget(AOwner : TComponent) : TWidget;

  end;

implementation

uses TypInfo, vfdformparser;

{ TVFDWidgetClass }

function TVFDWidgetClass.AddProperty(apropname : string; apropclass : TVFDPropertyClass;
  desc : string) : TVFDWidgetProperty;
begin
  result := apropclass.Create(apropname);
  result.Description := desc;
  FProps.Add(result);
end;

constructor TVFDWidgetClass.Create(aClass : TWidgetClass);
begin
  WidgetClass := aClass;
  FProps := TList.Create;
  Description := '';
  NameBase := 'Widget';
  Container := false;
end;

function TVFDWidgetClass.CreateWidget(AOwner : TComponent) : TWidget;
begin
  result := WidgetClass.Create(AOwner);
end;

destructor TVFDWidgetClass.Destroy;
var
  n : integer;
begin
  for n:=0 to FProps.Count-1 do TVFDWidgetProperty(FProps[n]).Free;
  FProps.Free;
  inherited;
end;

function TVFDWidgetClass.GetProperty(ind: integer): TVFDWidgetProperty;
begin
  result := TVFDWidgetProperty(FProps[ind-1]);
end;

function TVFDWidgetClass.PropertyCount: integer;
begin
  result := FProps.Count;
end;

{ TVFDWidgetProperty }

constructor TVFDWidgetProperty.Create(aName : string);
begin
  Name := aName;
  Description := '';
end;

function TVFDWidgetProperty.GetPropertySource(wg: TWidget; const ident : string): string;
begin

end;

function TVFDWidgetProperty.ParseSourceLine(wg: TWidget; const line: string): boolean;
begin
  result := false;
end;

function TVFDWidgetProperty.CreateEditor(AOwner : TComponent): TVFDPropertyEditor;
begin
  result := nil;
end;

procedure TVFDWidgetProperty.DrawValue(wg: TWidget; canvas: TGfxCanvas; rect: TGfxRect; flags: integer);
var
  x,y,fy : integer;
  s : string;
begin
  x := rect.left;
  y := rect.top;
  fy := y + rect.height div 2 - Canvas.Font.Height div 2;

  s := GetValueText(wg);
  Canvas.DrawString16(x+1,fy,s);
end;

function TVFDWidgetProperty.GetValueText(wg: TWidget): string;
begin
  result := u8('['+Name+']');
end;

procedure TVFDWidgetProperty.OnExternalEdit(wg: TWidget);
begin
  writeln('external edit');
end;

{ TVFDPropertyEditor }

constructor TVFDPropertyEditor.Create(AOwner: TComponent; aprop : TVFDWidgetProperty);
begin
  inherited Create(AOwner);
  OnUpdate := nil;
  FProp := aprop;
end;

procedure TVFDPropertyEditor.CreateLayout;
begin
  //abstract
end;

procedure TVFDPropertyEditor.LoadValue(wg: TWidget);
begin
  Writeln('abstract: editor.LoadValue');
end;

procedure TVFDPropertyEditor.UpdateProperty(sender: TObject);
begin
  if Assigned(OnUpdate) then OnUpdate(self);
end;

procedure TVFDPropertyEditor.StoreValue(wg: TWidget);
begin
  Writeln('abstract: editor.StoreValue');
  // check property type
  // the property must be published !
  // PPropInfo := GetPropInfo(object, 'propname');
  // if PPropInfo^.PropType^.name =
end;


end.
