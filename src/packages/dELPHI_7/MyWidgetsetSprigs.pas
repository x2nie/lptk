unit MyWidgetsetSprigs;

interface

uses
  Windows, Messages, SysUtils, Classes,
  //Controls, Forms,
  MyWidgetset,
  DesignIntf, DesignEditors, TypInfo, Contnrs, TreeIntf;

type
  TMyWidgetSprig = class(TComponentSprig)
  public
    constructor Create(AItem: TPersistent); override;
    class function PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean; override;
    function DragOverTo(AParent: TSprig): Boolean; override;
    function DragDropTo(AParent: TSprig): Boolean; override;
  end;

  TMyWidgetFormRootSprig = class(TRootSprig)
  public
    constructor Create(AItem: TPersistent); override;
    function DragOver(AItem: TSprig): Boolean; override;
    function DragDrop(AItem: TSprig): Boolean; override;
    function PaletteOver(ASprigClass: TSprigClass; AClass: TClass): Boolean; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterSprigType(TMyWidget, TMyWidgetSprig);
  RegisterRootSprigType(TMyForm, TMyWidgetFormRootSprig);
end;

{ TControlSprig }

constructor TMyWidgetSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CUIControlImageIndex[TMyWidget(AItem).AcceptChildsAtDesignTime];
end;

function TMyWidgetSprig.DragDropTo(AParent: TSprig): Boolean;
begin
  Result := ((AParent is TMyWidgetSprig) or (AParent is TMyWidgetFormRootSprig)) and
            TMyWidget(AParent.Item).AcceptChildsAtDesignTime;
end;

function TMyWidgetSprig.DragOverTo(AParent: TSprig): Boolean;
begin
  Result := ((AParent is TMyWidgetSprig) or (AParent is TMyWidgetFormRootSprig)) and
            TMyWidget(AParent.Item).AcceptChildsAtDesignTime;
end;

class function TMyWidgetSprig.PaletteOverTo(AParent: TSprig; AClass: TClass): Boolean;
begin
  Result := ((AParent is TMyWidgetSprig) or (AParent is TMyWidgetFormRootSprig)) and
            TMyWidget(AParent.Item).AcceptChildsAtDesignTime;
end;


{ TWinControlSprig & TWinControlRootSprig supports }

function WinControlDragDrop(Sender, AItem: TSprig): Boolean;
var
  LLeft, LTop: Integer;
begin
  Result := TMyWidgetSprig(AItem).Parent <> Sender;
  if Result then
    if AItem.DragDropTo(Sender) then
      with TMyWidget(AItem.Item) do
      begin
        Parent := TMyWidget(Sender.Item);
        LLeft := Left;
        LTop := Top;
        if LLeft + Width > Parent.Width then
          LLeft := Parent.Width - Width;
        if LTop + Height > Parent.Height then
          LTop := Parent.Height - Height;
        //SetBounds(LLeft, LTop, Width, Height);
      end;
end;

function WinControlDragOver(Sender, AItem: TSprig): Boolean;
begin
  Result := (AItem is TMyWidgetSprig) and
            //(csAcceptsControls in TControl(Sender.Item).ControlStyle) and
            TMyWidget(Sender.Item).AcceptChildsAtDesignTime and
            AItem.DragOverTo(Sender);
end;

function WinControlPaletteOver(Sender: TSprig; ASprigClass: TSprigClass; AClass: TClass): Boolean;
begin
  Result := (ASprigClass.InheritsFrom(TMyWidgetSprig) or ASprigClass.InheritsFrom(TMyWidgetFormRootSprig)) and
            //(csAcceptsControls in TControl(Sender.Item).ControlStyle) and
            TMyWidget(Sender.Item).AcceptChildsAtDesignTime and
            ASprigClass.PaletteOverTo(Sender, AClass);
end;


{ TWinControlRootSprig }

constructor TMyWidgetFormRootSprig.Create(AItem: TPersistent);
begin
  inherited;
  ImageIndex := CFormSprigImage;
end;

function TMyWidgetFormRootSprig.DragDrop(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Self) and
            ((AItem is TMyWidgetSprig) and WinControlDragDrop(Self, AItem)) or
            ((AItem is TComponentSprig) and inherited DragDrop(AItem));
end;

function TMyWidgetFormRootSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Self) and WinControlDragOver(Self, AItem);
end;

function TMyWidgetFormRootSprig.PaletteOver(ASprigClass: TSprigClass;
  AClass: TClass): Boolean;
begin
  Result := WinControlPaletteOver(Self, ASprigClass, AClass) or
            ASprigClass.InheritsFrom(TComponentSprig);
end;
end.
