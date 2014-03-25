unit hd_designer;

{$mode objfpc}{$H+}

interface

uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, Graphics,
  ProjectIntf, hd_defs, hd_main, hd_widget, hd_form{, hd_edit, hd_memo};

type

  { TpgfMediator }

  TpgfMediator = class(TDesignerMediator)
  private
    m_pgfForm: TpgfForm;
  public
    // needed by the lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
      override;
    class function FormClass: TComponentClass; override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    procedure GetClientArea(AComponent: TComponent; out
            CurClientArea: TRect; out ScrollOffset: TPoint); override;
    procedure Paint; override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent;
                Child: TComponentClass): boolean; override;
  public
    // needed by TpgfWidget
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property pgfForm: TpgfForm read m_pgfForm;
  end;


  { TFileDescPascalUnitWithPgfForm }

  TFileDescPascalUnitWithPgfForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;


procedure Register;

implementation
uses wgbutton;

procedure Register;
begin
  FormEditingHook.RegisterDesignerMediator(TpgfMediator);
  RegisterComponents('Standard',[TwgButton{, TpgfMemo}]);
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithPgfForm.Create,
                                FileDescGroupName);
end;

{ TFileDescPascalUnitWithPgfForm }

constructor TFileDescPascalUnitWithPgfForm.Create;
begin
  inherited Create;
  Name:='pgfForm';
  ResourceClass:=TpgfForm;
  UseCreateFormStatements:=false;
end;

function TFileDescPascalUnitWithPgfForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, hd_base, hd_main, hd_form';
end;

function TFileDescPascalUnitWithPgfForm.GetLocalizedName: string;
begin
  Result:='pgfForm';
end;

function TFileDescPascalUnitWithPgfForm.GetLocalizedDescription: string;
begin
  Result:='Create a new pgfForm for hdGUI Application';
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
  Mediator.m_pgfForm:=aForm as TpgfForm;
  //Mediator.m_pgfForm.show();//allocate windowhandle

  //Mediator.m_pgfForm.FormDesigner:=Mediator;
end;

class function TpgfMediator.FormClass: TComponentClass;
begin
  Result := TpgfForm;
end;

procedure TpgfMediator.GetBounds(AComponent: TComponent; out CurBounds: TRect);
var
  w: TpgfWidget;
begin
  if AComponent is TpgfWidget then
  begin
    w:=TpgfWidget(AComponent);
    CurBounds:=Bounds(w.Left,w.Top,w.Width,w.Height);
  end else
    inherited GetBounds(AComponent,CurBounds);
end;

procedure TpgfMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is TpgfWidget then begin
    TpgfWidget(AComponent).SetPosition(NewBounds.Left,NewBounds.Top,
      NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
  end else
    inherited SetBounds(AComponent,NewBounds);
end;

procedure TpgfMediator.GetClientArea(AComponent: TComponent; out
  CurClientArea: TRect; out ScrollOffset: TPoint);
begin
  inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;

procedure TpgfMediator.Paint;

  procedure PaintWidget(AWidget: TpgfWidget);
  var
    i: Integer;
    Child: TpgfWidget;
    msgp : TpgfMessageParams;
  begin
    with LCLForm.Canvas do
    begin
      // fill background
      Brush.Style:=bsSolid;
      Brush.Color:= clBtnFace;
      FillRect(0,0,AWidget.Width,AWidget.Height);
      // outer frame
      Pen.Color:=clGray;
      Rectangle(0,0,AWidget.Width,AWidget.Height);
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
      fillchar(msgp,sizeof(msgp),0);
      //pgfSendMessage(self, AWidget, PGFM_PAINT, msgp);
      AWidget.RePaint;

      //test canvas
      AWidget.Canvas.DrawControlFrame(0,0,AWidget.Width, AWidget.Height);
      AWidget.Canvas.SetColor(clRed);
      AWidget.Canvas.DrawLine(0,AWidget.Height,AWidget.Width,0);

      if AWidget.Canvas.PaintTo(LCLForm.Canvas.Handle, 0,0, AWidget.Width, AWidget.Height) then
        TextOut(5,2,'painted')
      else
        TextOut(5,2,'failpaint');
      AWidget.Canvas.EndDraw;


      // children
      if AWidget.ComponentCount>0 then
      begin
        SaveHandleState;
        // clip client area
        //MoveWindowOrgEx(Handle,AWidget.BorderLeft,AWidget.BorderTop);
        //MoveWindowOrgEx(Handle,0,0);
        //if IntersectClipRect(Handle, 0, 0, AWidget.Width-AWidget.BorderLeft-AWidget.BorderRight,
        //                     AWidget.Height-AWidget.BorderTop-AWidget.BorderBottom)<>NullRegion
        //then
        begin
          for i:=0 to AWidget.ComponentCount-1 do
          if (AWidget.Components[i] is TpgfWidget) and (TpgfWidget(AWidget.Components[i]).Parent = Awidget)  then;
          begin
            SaveHandleState;
            Child:=TpgfWidget(AWidget.Components[i]);
            // clip child area
            //MoveWindowOrgEx(Handle,Child.Left,Child.Top);
            //if IntersectClipRect(Handle,0,0,Child.Width,Child.Height)<>NullRegion then
              PaintWidget(Child);
            RestoreHandleState;
          end;
        end;
        RestoreHandleState;
      end;
    end;
  end;

begin
  m_pgfForm.show();//allocate windowhandle
  PaintWidget(m_pgfForm);
  m_pgfForm.Hide();

//  m_pgfForm.Invalidate;
  inherited Paint;
end;

function TpgfMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result:=not (AComponent is TpgfWidget);
end;

function TpgfMediator.ParentAcceptsChild(Parent: TComponent;
  Child: TComponentClass): boolean;
begin
  result := true;
  //Result:=(Parent is TpgfWidget) and TpgfWidget(Parent).IsContainer
    // and Child.InheritsFrom(TpgfComponent);
    //and (TpgfWidget(Parent).AcceptChildsAtDesignTime);
end;

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

end.

