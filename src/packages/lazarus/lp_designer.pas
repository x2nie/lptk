unit lp_designer;

{$mode objfpc}{$H+}

interface

uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, Graphics,
  ProjectIntf, lp_defs, lp_main, lp_widget, lp_form{, hd_edit, hd_memo}
  ,LResources, LCLVersion;

{.$IF LCL_FULLVERSION >= 1030000} //for laz 1.3.0.0 (major,minor,release,patch)
     {.$DEFINE HasOiNodeGetImageIndex}
{.$endif}
type

  { TpgfMediator }

  TpgfMediator = class(TDesignerMediator)
  private
    FlpForm: TlpForm;
  public
    // needed by the lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
      override;
    class function FormClass: TComponentClass; override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    {procedure GetClientArea(AComponent: TComponent; out
            CurClientArea: TRect; out ScrollOffset: TPoint); override;}
    procedure Paint; override;
    procedure InitComponent(AComponent, NewParent: TComponent; NewBounds: TRect); override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent;
                Child: TComponentClass): boolean; override;
  public
    {$IF LCL_FULLVERSION >= 1030000} //for laz 1.3.0.0 (major,minor,release,patch)
    {.$ifdef HasOiNodeGetImageIndex}
    procedure OiNodeGetImageIndex(APersistent: TPersistent; var AIndex: integer); override;
    {$endif}
  public
    // needed by TpgfWidget
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property pgfForm: TlpForm read FlpForm;
  end;



procedure Register;

implementation
uses Controls, PropEdits, lp_propedits,
  lp_button,lp_progressbar, lp_trackbar, lp_edit, lp_memo,
  lp_listbox, //lp_combobox,
  lp_menu

  ;

procedure Register;
begin
  FormEditingHook.RegisterDesignerMediator(TpgfMediator);
  RegisterComponents('Standard',[TlpTImer, TlpButton, TlpMemo, TlpEdit, TlpListBox,
  TlpProgressbar, TlpTrackbar, //TlpCombobox
  TlpMenuBar, TlpPopupMenu
  ]);

  RegisterPropertyEditor(TypeInfo(widestring), TpgfWidget, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(widestring), TpgfWidget, 'Text', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(lp_main.TCursor), TpgfWidget, 'Cursor', TCursorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TpgfWidget, 'FontDesc', TFontDescPropertyEditor);

end;






{ TpgfMediator }
type
    TpgfWidgetAccess = class(TpgfWidget)
    end;

class function TpgfMediator.CreateMediator(TheOwner, aForm: TComponent
  ): TDesignerMediator;

var
  Mediator: TpgfMediator;
begin
  pgfOpenDisplay('');
  pgfDesigning := true;

  Result:=inherited CreateMediator(TheOwner, aForm);
  Mediator:=TpgfMediator(Result);
  Mediator.FlpForm:=aForm as TlpForm;
  //Mediator.m_pgfForm.show();//allocate windowhandle

  //Mediator.m_pgfForm.FormDesigner:=Mediator;
end;

class function TpgfMediator.FormClass: TComponentClass;
begin
  Result := TlpForm;
end;

procedure TpgfMediator.GetBounds(AComponent: TComponent; out CurBounds: TRect);
var
  w: TpgfWidget;
begin
  if AComponent is TpgfWidget then
  begin
    w:=TpgfWidget(AComponent);
    CurBounds:=Bounds(w.Left,w.Top,{w.Left +} w.Width, {w.Top +} w.Height);
  end else
    inherited GetBounds(AComponent,CurBounds);
end;

procedure TpgfMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is TpgfWidget then begin
    TpgfWidget(AComponent).SetPosition(NewBounds.Left,NewBounds.Top,
      NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
  end else
  begin
    if ComponentIsIcon(AComponent) then
       SetComponentLeftTopOrDesignInfo(AComponent,NewBounds.Left,NewBounds.Top)
    else
    inherited SetBounds(AComponent,NewBounds);
  end;
end;

{procedure TpgfMediator.GetClientArea(AComponent: TComponent; out
  CurClientArea: TRect; out ScrollOffset: TPoint);
begin
  inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;}

procedure TpgfMediator.Paint;
var Bmp : TBitmap;

  procedure PaintFormGrid(AWidget: TpgfWidget);
  var x, y : integer;
  const
    gx = 8; //TODO: Link to: EnvironmentOptions.GridSizeX
    gy = 8;
  begin
    for y := 0 to (Awidget.Height -1) div gy do
      for x := 0 to (Awidget.Width -1) div gx do
      begin
        {LCLForm}BMP.Canvas.Pixels[x *gx, y *gy] := clBlack;
      end;
  end;

  procedure PaintWidget(AWidget: TpgfWidget);
  var
    i: Integer;
    Child: TpgfWidget;
    msgp : TpgfMessageParams;
    r : TRect;
    p : TPoint;
  begin
    //with LCLForm.Canvas do
    With Bmp.Canvas do
    begin
      // fill background
      Brush.Style:=bsSolid;
      Brush.Color:= clBtnFace;
      FillRect(0,0,AWidget.Width,AWidget.Height);

      // outer frame
      {Pen.Color:=clGray;
      Rectangle(0,0,AWidget.Width,AWidget.Height);
      }
      {// inner frame
      if AWidget.AcceptChildsAtDesignTime then begin
        Pen.Color:=clMaroon;
        Rectangle(AWidget.BorderLeft-1,AWidget.BorderTop-1,
                  AWidget.Width-AWidget.BorderRight+1,
                  AWidget.Height-AWidget.BorderBottom+1);
      end;
      // caption
      TextOut(5,2,AWidget.Caption);}

      AWidget.Canvas.BeginDraw;
      //TpgfWidgetAccess(AWidget).HandlePaint;
      //fillchar(msgp,sizeof(msgp),0);
      //pgfSendMessage(self, AWidget, PGFM_PAINT, msgp);
      AWidget.RePaint;

      //test canvas
      {AWidget.Canvas.DrawControlFrame(0,0,AWidget.Width, AWidget.Height);
      AWidget.Canvas.SetColor(clRed);
      AWidget.Canvas.DrawLine(0,AWidget.Height,AWidget.Width,0);

      if AWidget.Canvas.PaintTo(LCLForm.Canvas.Handle, 0,0, AWidget.Width, AWidget.Height) then
        TextOut(5,2,format('OK %d',[AWidget.WinHandle]) )
      else
        TextOut(5,2,'failpaint');

      bmp := TBitmap.Create;
      bmp.SetSize(AWidget.Width, AWidget.Height);
      AWidget.Canvas.PaintTo(bmp.Canvas.Handle, 0,0, AWidget.Width, AWidget.Height);
      bmp.SaveToFile('c:\'+AWidget.Name+'.bmp' );
      bmp.Free;}
      AWidget.Canvas.PaintTo({LCLForm.Canvas.}Handle, 0,0, AWidget.Width, AWidget.Height);

      AWidget.Canvas.EndDraw;

      //if csDesigning in AWidget.ComponentState then  TextOut(5,2,'design');
      //self.GetClientArea(Awidget, r, p );
      //Pen.Color:=clRed;
      //Rectangle(r);
      if AWidget is TlpForm then
         PaintFormGrid(AWidget);


      //TextOut(5,2,format('has%d,@%s',[AWidget.ChildCount, AWidget.ParentName]) ) ;

      // children
      if AWidget.ChildCount>0 then
      begin
        SaveHandleState;
        // clip client area
        //MoveWindowOrgEx(Handle,AWidget.BorderLeft,AWidget.BorderTop);
        MoveWindowOrgEx(Handle,0,0);
        //if IntersectClipRect(Handle, 0, 0, AWidget.Width-AWidget.BorderLeft-AWidget.BorderRight,
        //                     AWidget.Height-AWidget.BorderTop-AWidget.BorderBottom)<>NullRegion
        //then
        begin
          //for i:=0 to AWidget.ComponentCount-1 do
          //if (AWidget.Components[i] is TpgfWidget) and (TpgfWidget(AWidget.Components[i]).Parent = Awidget)  then
          for i:=0 to AWidget.ChildCount-1 do
          begin
            SaveHandleState;
            //Child:=TpgfWidget(AWidget.Components[i]);
            Child:=AWidget.Children[i];
            // clip child area
            MoveWindowOrgEx(Handle,Child.Left,Child.Top);
            if IntersectClipRect(Handle,0,0,Child.Width,Child.Height)<>NullRegion then
              PaintWidget(Child);
            RestoreHandleState;
          end;
        end;
        RestoreHandleState;
      end;
    end;
  end;

begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(LCLForm.Width, LCLForm.Height);
  //Bmp.BeginUpdate;
  FlpForm.show();//allocate windowhandle
  PaintWidget(FlpForm);
  FlpForm.Hide();

  //Bmp.EndUpdate;
  LCLForm.Canvas.Draw(0,0,Bmp);
  //lclForm.Canvas.Pen.Color:= clRed;
  //LCLForm.Canvas.TextOut(10,10, format('W=%d, H=%d',[bmp.Width, bmp.Height]));
  Bmp.Free;

//  m_pgfForm.Invalidate;
  inherited Paint;
end;

procedure TpgfMediator.InitComponent(AComponent, NewParent: TComponent;
  NewBounds: TRect);
begin
  if AComponent is TpgfWidget then
  begin
    if (NewBounds.Right - NewBounds.Left = 50) and //set by customformeditor.pas #1384 - 1385
       (NewBounds.Bottom - NewBounds.Top = 50) then
     newBounds := Bounds(newBounds.Left, newBounds.Top,
             TpgfWidget(AComponent).Width, TpgfWidget(AComponent).Height);

  end;
  inherited InitComponent(AComponent, NewParent, NewBounds);
end;

function TpgfMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result:=not (AComponent is TpgfWidget);
end;

function TpgfMediator.ParentAcceptsChild(Parent: TComponent;
  Child: TComponentClass): boolean;
begin
  //result := true;
  Result:=(Parent is TpgfWidget) and ( wsAcceptsChildren in TpgfWidget(Parent).WidgetStyle)
    and Child.InheritsFrom(TlpComponent)
    //or (not Child.InheritsFrom(TControl))
    //and (TpgfWidget(Parent).AcceptChildsAtDesignTime);
end;

{$IF LCL_FULLVERSION >= 1030000} //for laz 1.3.0.0 (major,minor,release,patch)
procedure TpgfMediator.OiNodeGetImageIndex(APersistent: TPersistent;
  var AIndex: integer);
begin
  if Assigned(APersistent) then
    begin
      {if (APersistent is TControl) and (csAcceptsControls in TControl(APersistent).ControlStyle) then
        Result := 3
      else
      if (APersistent is TControl) then
        Result := 2
      else
      if (APersistent is TComponent) then
        Result := 1
      else
      if (APersistent is TCollection) then
        Result := 4
      else
      if (APersistent is TCollectionItem) then
        Result := 5;}
      if APersistent is TpgfWidget then
         AIndex := 2;
    end;
end;
{$endif}

constructor TpgfMediator.Create(AOwner: TComponent);
begin
  //pgfApplication.Initialize;
  pgfOpenDisplay('');
  pgfDesigning := true;
  inherited Create(AOwner);
end;

destructor TpgfMediator.Destroy;
begin
  //if FMyForm<>nil then FMyForm.Designer:=nil;
  //FMyForm:=nil;

  inherited Destroy;
end;

initialization
{$I lp_designtime.lrs}
end.

