unit vfdwidgetclass;

// the new widget editor interface.
// not finished and used yet.

interface

uses Classes, SysUtils, gfxbase, gfxwidget, schar16,
  wgLabel, wgEdit;

type
  TVFDPropertyEditor = class(TWidget)
  public
    OnChange : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;
  public
    // virtuals
    procedure CreateLayout; virtual;

    procedure SetWidgetProperty(wg : TWidget; propname : string); virtual;
    procedure GetWidgetProperty(wg : TWidget; propname : string); virtual;
  end;

  TVFDPropertyEditorClass = class of TVFDPropertyEditor;

  TWidgetClass = class of TWidget;

  TVFDWidgetProperty = class
  protected
    FEditorClass : TVFDPropertyEditorClass;
  public
    Name : string;
    Description : string;

    function CreateEditor(AOwner : TComponent) : TVFDPropertyEditor;

  public
    // Virtuals
    constructor Create; virtual;  // override to set EditorClass

    function ParseSourceLine(wg : TWidget; const line : string) : boolean; virtual;
    function GetPropertySource(wg : TWidget) : string; virtual;
  end;

  TVFDPropertyClass = class of TVFDWidgetProperty;

  TVFDWidgetClass = class
  private
    FProps : TList;
  public
    WidgetClass : TWidgetClass;
    Description : string;
    WidgetIconName : string;

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
  result := apropclass.Create;
  result.Name := apropname;
  result.Description := desc;
  FProps.Add(result);
end;

constructor TVFDWidgetClass.Create(aClass : TWidgetClass);
begin
  WidgetClass := aClass;
  FProps := TList.Create;
  Description := '';
end;

function TVFDWidgetClass.CreateWidget(AOwner : TComponent) : TWidget;
begin
  result := TWidgetClass.Create(AOwner);
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

constructor TVFDWidgetProperty.Create;
begin
  Name := '???';
  Description := '';
  FEditorClass := nil;
end;

function TVFDWidgetProperty.GetPropertySource(wg: TWidget): string;
begin

end;

function TVFDWidgetProperty.ParseSourceLine(wg: TWidget; const line: string): boolean;
begin
  result := false;
end;

function TVFDWidgetProperty.CreateEditor(AOwner : TComponent): TVFDPropertyEditor;
begin
  result := FEditorClass.Create(AOwner);
end;

{ TVFDPropertyEditor }

constructor TVFDPropertyEditor.Create(AOwner: TComponent);
begin
  inherited;
  OnChange := nil;
  CreateLayout;
end;

procedure TVFDPropertyEditor.CreateLayout;
begin
  //abstract
end;

procedure TVFDPropertyEditor.GetWidgetProperty(wg: TWidget; propname: string);
begin
  Writeln('abstract: GetWidgetProperty');
end;

procedure TVFDPropertyEditor.SetWidgetProperty(wg: TWidget; propname: string);
begin
  Writeln('abstract: SetWidgetProperty');
  // check property type
  // the property must be published !
  // PPropInfo := GetPropInfo(object, 'propname');
  // if PPropInfo^.PropType^.name =
end;


end.
