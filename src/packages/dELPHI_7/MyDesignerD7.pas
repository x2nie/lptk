{
  some code taken from:
  * http://www.borlandtalk.com/custom-form-designer-using-icustomdesignform-vt105527.html
}


unit MyDesignerD7;

interface

{.$DEFINE REGISTERINIT}

uses
  Classes, SysUtils, Windows, Forms, Controls, Graphics, ExtCtrls,
  ToolIntf, ExptIntf,  EditIntf, DesignIntf, DesignEditors, VCLEditors;

type
  TMyWidgetAdapter = class(TPanel)
  private

  protected

  public
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
  published 

  end;


  TMyDesignModule = class(TCustomModule, ICustomModule, IDesignNotification,
    ICustomDesignForm )
  private
    FMyWidgetAdapter : TMyWidgetAdapter;
  protected

  public
    // TCustomModule //
    constructor Create(ARoot: TComponent; const ADesigner: IDesigner); override;
    destructor Destroy; override;
    procedure ValidateComponent(Component: TComponent); override;


    // ICustomDesignForm //
    procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
      out DesignForm: TCustomForm; out ComponentContainer: TWinControl);

    // IDesignNotification //
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  published 

  end;



  TMyForm_Expert = class (TIExpert)
  public
    function GetStyle: TExpertStyle; override;
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;

procedure Register;


implementation

uses MyWidgetSet;


var
  MyNotify:TMyDesignModule;
{ TMyNotify }

procedure Register;
begin
  RegisterCustomModule (TMyForm, TMyDesignModule);
  RegisterLibraryExpert(TMyForm_Expert.Create);
end;

constructor TMyDesignModule.Create(ARoot: TComponent;
  const ADesigner: IDesigner);
begin
  inherited;
  RegisterDesignNotification(self);
end;

procedure TMyDesignModule.CreateDesignerForm(const Designer: IDesigner;
  Root: TComponent; out DesignForm: TCustomForm;
  out ComponentContainer: TWinControl);

begin
  //AddMsg('Designer Constructor');
  DesignForm:=TForm.CreateNew(nil);
  FMyWidgetAdapter := TMyWidgetAdapter.Create(DesignForm);
  ComponentContainer := FMyWidgetAdapter;
   
  ComponentContainer.Align:=alClient; 
  ComponentContainer.Parent:=DesignForm; 

  RegisterDesignNotification(self); 

  try 
    //TForm(root).Designer := Designer;
  except
    //on e : exception do
    //AddMsg('Create Button:%s', [e.message]);
  end;
end;  

procedure TMyDesignModule.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = ADesigner then
    Close;
end;

procedure TMyDesignModule.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin

end;

destructor TMyDesignModule.Destroy;
begin
  UnregisterDesignNotification(self);
  inherited;
end;

procedure TMyDesignModule.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TMyDesignModule.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TMyDesignModule.ItemsModified(const ADesigner: IDesigner);
begin
  FMyWidgetAdapter.Invalidate;
end;

procedure TMyDesignModule.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

procedure TMyDesignModule.ValidateComponent(Component: TComponent);
begin
  if not (Component is TMyWidget) then
    raise Exception.Create ('This form can host only TMyWidget component.');
end;

{ TmyPanelEditExpert }

procedure TMyForm_Expert.Execute;
var
  ModuleName, FormName, FileName: string;
  ModIntf: TIModuleInterface;
begin
  ToolServices.GetNewModuleAndClassName (
    'MyForm', ModuleName, FormName, FileName);
  ModIntf := ToolServices.CreateModuleEx (FileName, FormName,
    'MyForm', '', nil, nil,
  [cmNewForm, cmAddToProject, cmUnNamed]);
  ModIntf.ShowSource;
  ModIntf.ShowForm;
  ModIntf.Release;
end;

function TMyForm_Expert.GetAuthor: string;
begin
  result := 'x2nie';
end;

function TMyForm_Expert.GetComment: string;
begin
  result := 'My toolkit expert get comment';
end;

function TMyForm_Expert.GetGlyph: HICON;
begin
  result := 0;
end;

function TMyForm_Expert.GetIDString: string;
begin
   result := 'my_toolkit.2.expert';
end;

function TMyForm_Expert.GetMenuText: string;
begin
  result := 'my_toolkit-menuText';
end;

function TMyForm_Expert.GetName: string;
begin
  result := 'TMyForm';
end;

function TMyForm_Expert.GetPage: string;
begin
  result := 'New';
end;

function TMyForm_Expert.GetState: TExpertState;
begin
  result :=  [esEnabled];
end;

function TMyForm_Expert.GetStyle: TExpertStyle;
begin
  result := esForm;
end;

{ TMyWidgetAdapter }

constructor TMyWidgetAdapter.Create(AOwner: TComponent);
begin
  inherited;
  //Panel.Color := clRed;
  BevelOuter := bvNone;
end;


end.

{$IFDEF REGISTERINIT}


initialization
  try
    MyNotify:=TMyDesignModule.Create;
    MyNotify._AddRef; //http://www.delphigroups.info/2/7/797982.html
  except
    MyNotify:=nil;
    Raise;
  end;

  RegisterDesignNotification(MyNotify);

finalization
  UnRegisterDesignNotification(MyNotify);
  MyNotify.Free;
  MyNotify:=nil;
  
{$ENDIF}

