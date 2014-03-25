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
  pgfDesigning := True;
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
begin
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

