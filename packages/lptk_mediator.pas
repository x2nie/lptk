unit lptk_mediator;

{$mode objfpc}{$H+}

interface


uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, Graphics,
  ProjectIntf,
  gfxwidget, gfxform //TWidget //wgbevel, wgbutton
  ;

type

  { TLptkWidgetMediator }

  TLptkWidgetMediator = class(TDesignerMediator{, IMyWidgetDesigner})
  private
    FMyForm: TGfxForm;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    // needed by the Lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
      override;
    class function FormClass: TComponentClass; override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    procedure SetFormBounds(RootComponent: TComponent; NewBounds, ClientRect: TRect); override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    {procedure GetClientArea(AComponent: TComponent; out
            CurClientArea: TRect; out ScrollOffset: TPoint); override;}
    procedure GetFormBounds(RootComponent: TComponent; out
  CurBounds, CurClientRect: TRect);
    procedure Paint; override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent;
                Child: TComponentClass): boolean; override;
  public
    // needed by TMyWidget
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property MyForm: TGfxForm read FMyForm;
  public
    procedure GetObjInspNodeImageIndex(APersistent: TPersistent; var AIndex: integer); override;
  end;

implementation

{ TLptkWidgetMediator }

constructor TLptkWidgetMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLptkWidgetMediator.Destroy;
begin
  //if FMyForm<>nil then FMyForm.Designer:=nil;
  FMyForm:=nil;
  inherited Destroy;
end;

procedure TLptkWidgetMediator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if FMyForm=AComponent then
    begin
      //FMyForm.Designer:=nil;
      FMyForm:=nil;
    end;
  end;
end;

class function TLptkWidgetMediator.CreateMediator(TheOwner, aForm: TComponent
  ): TDesignerMediator;
var
  Mediator: TLptkWidgetMediator;
begin
  Result:=inherited CreateMediator(TheOwner,aForm);
  Mediator:=TLptkWidgetMediator(Result);
  Mediator.FMyForm:=aForm as TGfxForm;
  Mediator.FMyForm.FreeNotification(Mediator);
  //Mediator.FMyForm.Designer:=Mediator;
end;

class function TLptkWidgetMediator.FormClass: TComponentClass;
begin
  Result:=TGfxForm;
end;

procedure TLptkWidgetMediator.GetBounds(AComponent: TComponent; out
  CurBounds: TRect);
var
  w: TWidget;
begin
  if AComponent is TWidget then begin
    w:=TWidget(AComponent);
    CurBounds:=Bounds(w.Left,w.Top,w.Width,w.Height);
  end else
    inherited GetBounds(AComponent,CurBounds);
end;

procedure TLptkWidgetMediator.InvalidateRect(Sender: TObject; ARect: TRect;
  Erase: boolean);
begin
  if (LCLForm=nil) or (not LCLForm.HandleAllocated) then exit;
  LCLIntf.InvalidateRect(LCLForm.Handle,@ARect,Erase);
end;

procedure TLptkWidgetMediator.GetObjInspNodeImageIndex(APersistent: TPersistent;
  var AIndex: integer);
begin
  if Assigned(APersistent) then
  begin
    {if (APersistent is TWidget) and (TWidget(APersistent).AcceptChildrenAtDesignTime) then
      AIndex := FormEditingHook.GetCurrentObjectInspector.ComponentTree.ImgIndexBox
    else}
    if (APersistent is TWidget) then
      AIndex := FormEditingHook.GetCurrentObjectInspector.ComponentTree.ImgIndexControl
    else
      inherited;
  end
end;

procedure TLptkWidgetMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is TWidget then with TWidget(AComponent) do begin
    {TWidget(AComponent).SetDimensions(NewBounds.Left,NewBounds.Top,
      NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
    TrySetOrdProp(AComponent,'width', TWidget(AComponent).Width);}
    Left := NewBounds.Left;
    Top := NewBounds.Top;
    Width:=NewBounds.Right-NewBounds.Left;
    Height:=NewBounds.Bottom-NewBounds.Top;

  end else
    inherited SetBounds(AComponent,NewBounds);
end;

procedure TLptkWidgetMediator.SetFormBounds(RootComponent: TComponent;
  NewBounds, ClientRect: TRect);
var
  r: TRect;
begin
  r:=Bounds(NewBounds.Left,NewBounds.Top,
            NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
  //debugln(['TDesignerMediator.SetFormBounds NewBounds=',dbgs(NewBounds),' ClientRect=',dbgs(ClientRect),' r=',dbgs(r)]);
  SetBounds(RootComponent,r);
  //inherited SetFormBounds(RootComponent, NewBounds, ClientRect);
end;

procedure TLptkWidgetMediator.GetFormBounds(RootComponent: TComponent; out
  CurBounds, CurClientRect: TRect);
begin
  CurBounds := Bounds(FMyForm.Left, FMyForm.top, FMyForm.width, FMyForm.height);
{  GetBounds(RootComponent,CurBounds);
  //debugln(['TDesignerMediator.GetFormBounds ',dbgs(CurBounds)]);
  CurClientRect:=Rect(0,0,CurBounds.Right-CurBounds.Left,
                      CurBounds.Bottom-CurBounds.Top);
  CurBounds.Right:=CurBounds.Left;
  CurBounds.Bottom:=CurBounds.Top;
  //debugln(['TDesignerMediator.GetFormBounds ',dbgs(CurBounds),' ',dbgs(CurClientRect)]);}
end;

{procedure TLptkWidgetMediator.GetClientArea(AComponent: TComponent; out
  CurClientArea: TRect; out ScrollOffset: TPoint);
var
  Widget: TWidget;
begin
  if AComponent is TWidget then begin
    Widget:=TWidget(AComponent);
    CurClientArea:=Rect(Widget.BorderLeft,Widget.BorderTop,
                        Widget.Width-Widget.BorderRight,
                        Widget.Height-Widget.BorderBottom);
    ScrollOffset:=Point(0,0);
  end else
    inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;}

procedure TLptkWidgetMediator.Paint;

  procedure PaintWidget(AWidget: TWidget);
  var
    i: Integer;
    Child: TWidget;
  begin
    with LCLForm.Canvas do begin
      // fill background
      Brush.Style:=bsSolid;
      Brush.Color:=clLtGray;
      FillRect(0,0,AWidget.Width,AWidget.Height);
      // outer frame
      Pen.Color:=clRed;
      Rectangle(0,0,AWidget.Width,AWidget.Height);
      // inner frame
      {if AWidget.AcceptChildrenAtDesignTime then begin
        Pen.Color:=clMaroon;
        Rectangle(AWidget.BorderLeft-1,AWidget.BorderTop-1,
                  AWidget.Width-AWidget.BorderRight+1,
                  AWidget.Height-AWidget.BorderBottom+1);
      end;}
      // caption
//      TextOut(5,2,AWidget.Caption);
      // children
      {if AWidget.ChildCount>0 then begin
        SaveHandleState;
        // clip client area
        MoveWindowOrgEx(Handle,AWidget.BorderLeft,AWidget.BorderTop);
        if IntersectClipRect(Handle, 0, 0, AWidget.Width-AWidget.BorderLeft-AWidget.BorderRight,
                             AWidget.Height-AWidget.BorderTop-AWidget.BorderBottom)<>NullRegion
        then begin
          for i:=0 to AWidget.ChildCount-1 do begin
            SaveHandleState;
            Child:=AWidget.Children[i];
            // clip child area
            MoveWindowOrgEx(Handle,Child.Left,Child.Top);
            if IntersectClipRect(Handle,0,0,Child.Width,Child.Height)<>NullRegion then
              PaintWidget(Child);
            RestoreHandleState;
          end;
        end;
        RestoreHandleState;
      end;}
    end;
  end;

begin
  PaintWidget(MyForm);
  inherited Paint;
end;

function TLptkWidgetMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result:=not (AComponent is TWidget);
end;

function TLptkWidgetMediator.ParentAcceptsChild(Parent: TComponent;
  Child: TComponentClass): boolean;
begin
  Result:=(Parent is TWidget) and (Child.InheritsFrom(TWidget))
    {and (TWidget(Parent).AcceptChildrenAtDesignTime)};
end;

end.

