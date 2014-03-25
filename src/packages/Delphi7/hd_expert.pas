unit hd_expert;

interface

uses
Classes, Forms, Windows, Dialogs, ExptIntf, ToolIntf,
FileCtrl, SysUtils, EditIntf, DesignIntf, DesignEditors;

type
  TPanelEditExpert = class (TIExpert)
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

  ThdFormModule = class (TCustomModule)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
  end;

procedure Register;

implementation
uses hd_defs, hd_main, hd_form;

procedure Register;
begin
//  RegisterCustomModule (TpgfForm, ThdFormModule);
  RegisterCustomModule (TpgfForm, TCustomModule);
  //RegisterCustomModule (TPanel, TPanelModule);
  RegisterLibraryExpert(TPanelEditExpert.Create);

end;

procedure TPanelEditExpert.Execute;
var
  ModuleName, FormName, FileName: string;
  ModIntf: TIModuleInterface;
begin
  ToolServices.GetNewModuleAndClassName (
    'pgfForm', ModuleName, FormName, FileName);
  ModIntf := ToolServices.CreateModuleEx (FileName, FormName,
    'pgfForm', '', nil, nil,
  [cmNewForm, cmAddToProject, cmUnNamed]);
  ModIntf.ShowSource;
  ModIntf.ShowForm;
  ModIntf.Release;
end;

function TPanelEditExpert.GetAuthor: string;
begin
  result := 'x2nie';
end;

function TPanelEditExpert.GetComment: string;
begin
  result := 'hd toolkit expert get comment';
end;

function TPanelEditExpert.GetGlyph: HICON;
begin
  result := 0;
end;

function TPanelEditExpert.GetIDString: string;
begin
   result := 'hd_toolkit.1.expert';
end;

function TPanelEditExpert.GetMenuText: string;
begin
  result := 'hd_toolkit-menuText';
end;

function TPanelEditExpert.GetName: string;
begin
  result := 'HD_Toolkit_Name';
end;

function TPanelEditExpert.GetPage: string;
begin
  result := 'New';
end;

function TPanelEditExpert.GetState: TExpertState;
begin
  result :=  [esEnabled];
end;

function TPanelEditExpert.GetStyle: TExpertStyle;
begin
  result := esForm;
end;

{ ThdFormModule }

procedure ThdFormModule.ExecuteVerb(Index: Integer);
begin
  inherited;

end;

function ThdFormModule.GetVerb(Index: Integer): string;
begin
  result := '';
end;

function ThdFormModule.GetVerbCount: Integer;
begin
  result := 0;
end;



function ThdFormModule.ValidateComponentClass(
  ComponentClass: TComponentClass): Boolean;
begin
  result := inherited ValidateComponentClass(ComponentClass);
  if result then
    result := ComponentClass.InheritsFrom(ThdComponent);
end;

end.
