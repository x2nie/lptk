unit lp_form_designer;

interface

uses
Classes, Windows, Dialogs, ExptIntf, ToolIntf,
//FileCtrl,
 SysUtils, EditIntf, DesignIntf, DesignEditors;

type
  TlpFormEditExpert = class (TIExpert)
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

// custom module for the panel

  TlpFormModule = class (TCustomModule)
  public
    constructor Create(ARoot: TComponent; const ADesigner: IDesigner); override;  
    class function DesignClass: TComponentClass; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function GetAttributes: TCustomModuleAttributes; override;    
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
    function Nestable: Boolean; override;
  end;

  TpgfCustomModule = class(TBaseCustomModule)
  public
    //constructor Create(ARoot: TComponent; const Designer: IDesigner); override;
    class function DesignClass: TComponentClass; override;
  end;

procedure Register;

implementation
uses lp_defs, lp_main, lp_form, ComponentDesigner, lp_DesignWindows;

procedure Register;
begin


//  ComponentDesigner.IDesignEnvironment
  RegisterCustomModule (TlpForm, TlpFormModule);
  //RegisterCustomModule (TlpForm, TpgfCustomModule);
//  RegisterCustomModule (TlpForm, TCustomModule);
  //RegisterCustomModule (TPanel, TPanelModule);
  RegisterLibraryExpert(TlpFormEditExpert.Create);

end;

procedure TlpFormEditExpert.Execute;
var
  ModuleName, FormName, FileName: string;
  ModIntf: TIModuleInterface;
begin
  pgfOpenDisplay('');
  pgfDesigning := True;  

  ToolServices.GetNewModuleAndClassName (
    'lpForm', ModuleName, FormName, FileName);
  ModIntf := ToolServices.CreateModuleEx (FileName, FormName,
    'lpForm', '', nil, nil,
  [cmNewForm, cmAddToProject, cmUnNamed, cmShowForm]);

  ModIntf.ShowSource;

  ModIntf.ShowForm;
  ModIntf.Release;
end;

function TlpFormEditExpert.GetAuthor: string;
begin
  result := 'x2nie';
end;

function TlpFormEditExpert.GetComment: string;
begin
  result := 'LP toolkit expert get comment';
end;

function TlpFormEditExpert.GetGlyph: HICON;
begin
  result := 0;
end;

function TlpFormEditExpert.GetIDString: string;
begin
   result := 'lp_toolkit.1.expert';
end;

function TlpFormEditExpert.GetMenuText: string;
begin
  result := 'lp_toolkit-menuText';
end;

function TlpFormEditExpert.GetName: string;
begin
  result := 'lpForm';
end;

function TlpFormEditExpert.GetPage: string;
begin
  result := 'New';
end;

function TlpFormEditExpert.GetState: TExpertState;
begin
  result :=  [esEnabled];
end;

function TlpFormEditExpert.GetStyle: TExpertStyle;
begin
  result := esForm;
end;

{ ThdFormModule }

constructor TlpFormModule.Create(ARoot: TComponent;
  const ADesigner: IDesigner);
begin
  if ARoot = nil then
    ShowMessage('Aroot= is nill!')
  else
  begin
    ShowMessage('Aroot='+Aroot.ClassName+ '>');
    inherited;
  end;
  //if ARoot.Owner <> nil then ShowMessage('owner='+ARoot.Owner.ClassName);
end;

class function TlpFormModule.DesignClass: TComponentClass;
begin
  Result := TDesignWindow;
end;

procedure TlpFormModule.ExecuteVerb(Index: Integer);
begin
  inherited;

end;

function TlpFormModule.GetAttributes: TCustomModuleAttributes;
begin
  Result := [cmaVirtualSize];
end;

function TlpFormModule.GetVerb(Index: Integer): string;
begin
  result := 'lpFormVerb';
end;

function TlpFormModule.GetVerbCount: Integer;
begin
  result := 1;
end;



function TlpFormModule.Nestable: Boolean;
begin
  Result := False;
end;

function TlpFormModule.ValidateComponentClass(
  ComponentClass: TComponentClass): Boolean;
begin
  //result := inherited ValidateComponentClass(ComponentClass);
  //if result then
    result := ComponentClass.InheritsFrom(TlpComponent);
end;

{ TpgfCustomModule }

class function TpgfCustomModule.DesignClass: TComponentClass;
begin
  Result := TDesignWindow;
end;

end.

