unit vfdwidgetclass;

// the new widget editor interface.
// not finished and used yet.

interface

uses Classes, SysUtils, gfxwidget;

type
  TVFDPropertyEditor = class(TGfxWidget)
  public
    procedure SetWidgetProperty(wg : TGfxWidget; propname : string); virtual;
    procedure GetWidgetProperty(wg : TGfxWidget; propname : string); virtual;

  public
    OnChage : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;
  end;

  TVFDPropertyEditorClass = class of TVFDPropertyEditor;

  TVFDWidgetProperty = class
  public
    Name : string;
    EditorClass : TVFDPropertyEditorClass;
    Description : string;

    constructor Create;

    // load / store
    function ParseSourceLine(wg : TGfxWidget; const line : string) : boolean; virtual;
    function GetPropertySource(wg : TGfxWidget) : string; virtual;

    // Editing
    procedure SetValue(wg : TGfxWidget; editorwg : TVFDPropertyEditor);
    procedure GetValue(wg : TGfxWidget; editorwg : TVFDPropertyEditor);
  end;

  TVFDWidgetClass = class
  private
    FProps : TList;
  public
    WidgetClassName : string;
    Description : string;

    constructor Create;
    destructor Destroy; override;

    procedure AddProperty(aprop : TVFDWidgetProperty);
  end;

implementation

uses TypInfo;

{ TVFDWidgetClass }

procedure TVFDWidgetClass.AddProperty(aprop: TVFDWidgetProperty);
begin
  FProps.Add(aprop);
end;

constructor TVFDWidgetClass.Create;
begin
  FProps := TList.Create;
  Description := '';
end;

destructor TVFDWidgetClass.Destroy;
var
  n : integer;
begin
  for n:=0 to FProps.Count-1 do TVFDWidgetProperty(FProps[n]).Free;
  FProps.Free;
  inherited;
end;

{ TVFDWidgetProperty }

constructor TVFDWidgetProperty.Create;
begin
  Name := '???';
  Description := '';
  EditorClass := nil;
end;

function TVFDWidgetProperty.GetPropertySource(wg: TGfxWidget): string;
begin

end;

procedure TVFDWidgetProperty.GetValue(wg, editorwg: TGfxWidget);
begin

end;

function TVFDWidgetProperty.ParseSourceLine(wg: TGfxWidget; const line: string): boolean;
begin

end;

procedure TVFDWidgetProperty.SetValue(wg, editorwg: TGfxWidget);
begin

end;

{ TVFDPropertyEditor }

constructor TVFDPropertyEditor.Create(AOwner: TComponent);
begin
  inherited;
  OnChange := nil;
end;

procedure TVFDPropertyEditor.GetWidgetProperty(wg: TGfxWidget; propname: string);
begin
  Writeln('abstract: GetWidgetProperty');
end;

procedure TVFDPropertyEditor.SetWidgetProperty(wg: TGfxWidget; propname: string);
begin
  Writeln('abstract: SetWidgetProperty');
  // check property type
  // the property must be published !
  // PPropInfo := GetPropInfo(object, 'propname');
  // if PPropInfo^.PropType^.name =
end;

end.
