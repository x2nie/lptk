unit hd_sprig;

interface

uses
  Windows, Messages, SysUtils, Classes, hd_defs, hd_main,
  hd_widget, hd_form,
  DesignIntf, DesignEditors, TypInfo, Contnrs, TreeIntf;

type
  {TControlSprig = class(TComponentSprig)
  public
    constructor Create(AItem: TPersistent); override;
    class function PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean; override;
    function DragOverTo(AParent: TSprig): Boolean; override;
    function DragDropTo(AParent: TSprig): Boolean; override;
  end;}

  TWidgetControlSprig = class(TComponentSprig)
  public
    constructor Create(AItem: TPersistent); override;
    function DragOver(AItem: TSprig): Boolean; override;
    function DragDrop(AItem: TSprig): Boolean; override;
    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; override;
  end;

  {TFrameSprig = class(TWidgetControlSprig)
  public
    constructor Create(AItem: TPersistent); override;
    procedure FigureChildren; override;
  end;  }

  TWidgetControlRootSprig = class(TRootSprig)
  public
    constructor Create(AItem: TPersistent); override;
    function DragOver(AItem: TSprig): Boolean; override;
    function DragDrop(AItem: TSprig): Boolean; override;
    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; override;
  end;

  TCustomFormRootSprig = class(TWidgetControlRootSprig)
  public
    constructor Create(AItem: TPersistent); override;
  end;

  TDataModuleRootSprig = class(TRootSprig)
  public
    constructor Create(AItem: TPersistent); override;
    function DragOver(AItem: TSprig): Boolean; override;
    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; override;
    function AcceptsClass(AClass: TClass): Boolean; override;
  end;
procedure Register;

implementation


procedure Register;
begin
  RegisterSprigType(TpgfWidget, TWidgetControlSprig);

  RegisterRootSprigType(TpgfWidget, TWidgetControlRootSprig);
  RegisterRootSprigType(TpgfForm, TCustomFormRootSprig);


end;

{ Sprigs and such }

{ TControlSprig }

{constructor TControlSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CControlSprigImage;
end;

function TControlSprig.DragDropTo(AParent: TSprig): Boolean;
begin
  Result := ((AParent is TControlSprig) or (AParent is TWidgetControlRootSprig))
      //and (TpgfWidget(AParent.Item).)
      ;
end;

function TControlSprig.DragOverTo(AParent: TSprig): Boolean;
begin
  Result := ((AParent is TControlSprig) or (AParent is TWidgetControlRootSprig))
    //and (csAcceptsControls in TpgfWidget(AParent.Item).ControlStyle)
    ;
end;

class function TControlSprig.PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean;
begin
  Result := ((AParent is TControlSprig) or (AParent is TWidgetControlRootSprig))
    //and            (csAcceptsControls in TpgfWidget(AParent.Item).ControlStyle)
    ;
end;
 }
{ TWidgetControlSprig & TWidgetControlRootSprig supports }

function WidgetControlDragDrop(Sender, AItem: TSprig): Boolean;
var
  LLeft, LTop: Integer;
begin
  Result := TWidgetControlSprig(AItem).Parent <> Sender;
  if Result then
    if AItem.DragDropTo(Sender) then
      with TpgfWidget(AItem.Item) do
      begin
        Parent := TpgfWidget(Sender.Item);
        {LLeft := Left;
        LTop := Top;
        if LLeft + Width > Parent.ClientWidth then
          LLeft := Parent.ClientWidth - Width;
        if LTop + Height > Parent.ClientHeight then
          LTop := Parent.ClientHeight - Height;
        SetBounds(LLeft, LTop, Width, Height);  }///x2nie
      end;
end;

function WidgetControlDragOver(Sender, AItem: TSprig): Boolean;
begin
  Result := (AItem is TWidgetControlSprig) and
            (wsContainer in TpgfWidget(Sender.Item).WidgetStyle) and
            AItem.DragOverTo(Sender);
end;

function WidgetControlPaletteOver(Sender: TSprig; ASprigClass: TSprigClass; AClass: TClass): Boolean;
begin
  Result := (ASprigClass.InheritsFrom(TWidgetControlSprig) or ASprigClass.InheritsFrom(TWidgetControlRootSprig)) and
            (wsContainer in TpgfWidget(Sender.Item).WidgetStyle) and
            ASprigClass.PaletteOverTo(Sender, AClass);
end;

{ TWidgetControlSprig }

constructor TWidgetControlSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CUIControlImageIndex[wsContainer in TpgfWidget(Item).WidgetStyle];
end;

function TWidgetControlSprig.DragDrop(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Owner) and WidgetControlDragDrop(Self, AItem);
end;

function TWidgetControlSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Owner) and WidgetControlDragOver(Self, AItem);
end;

function TWidgetControlSprig.PaletteOver(ASprigClass: TSprigClass;
  AClass: TClass): Boolean;
begin
  Result := WidgetControlPaletteOver(Self, ASprigClass, AClass);
end;

{ TWidgetControlRootSprig }

constructor TWidgetControlRootSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CUIControlImageIndex[wsContainer in TpgfWidget(Item).WidgetStyle];
end;

function TWidgetControlRootSprig.DragDrop(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Self) and
            ((AItem is TWidgetControlSprig) and WidgetControlDragDrop(Self, AItem)) or
            ((AItem is TComponentSprig) and inherited DragDrop(AItem));
end;

function TWidgetControlRootSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Self) and WidgetControlDragOver(Self, AItem);
end;

function TWidgetControlRootSprig.PaletteOver(ASprigClass: TSprigClass;
  AClass: TClass): Boolean;
begin
  Result := WidgetControlPaletteOver(Self, ASprigClass, AClass) or
            ASprigClass.InheritsFrom(TComponentSprig);
end;

{ TCustomFormRootSprig }

constructor TCustomFormRootSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CFormSprigImage;
end;

{ TDataModuleRootSprig }

function TDataModuleRootSprig.AcceptsClass(AClass: TClass): Boolean;
begin
  Result := not AClass.InheritsFrom(TpgfWidget) and
            inherited AcceptsClass(AClass);
end;

constructor TDataModuleRootSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CDataModuleSprigImage;
end;

function TDataModuleRootSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Self) and
            not (AItem is TWidgetControlSprig) and
            (AItem is TComponentSprig);
end;

function TDataModuleRootSprig.PaletteOver(ASprigClass: TSprigClass;
  AClass: TClass): Boolean;
begin
  Result := not ASprigClass.InheritsFrom(TWidgetControlSprig) and
            ASprigClass.InheritsFrom(TComponentSprig);
end;

{ TFrameSprig }
{
constructor TFrameSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CUIControlImageIndex[True];
end;

procedure TFrameSprig.FigureChildren;
var
  I: Integer;
  LChildItem: TComponent;
  LChild: TSprig;
  LChildClass: TComponentSprigClass;
begin
  // let it go first
  inherited;

  // now lets loop through the component items
  for I := 0 to TFrame(Item).ComponentCount - 1 do
  begin

    // find the best class
    LChildItem := TFrame(Item).Components[I];
    LChild := Root.Find(LChildItem);

    // if not then create it
    if LChild = nil then
    begin
      LChildClass := TComponentSprigClass(FindBestSprigClass(LChildItem.ClassType, TComponentSprig));
      if LChildClass <> nil then
      begin
        LChild := LChildClass.Create(LChildItem, Self);

        Add(LChild);
      end;
    end;
  end;
end;
     }
end.
