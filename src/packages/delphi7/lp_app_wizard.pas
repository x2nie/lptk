unit lp_app_wizard;

interface
uses
  Classes, SysUtils, Windows, lp_defs, lp_main, lp_form, lp_widget,
  ExptIntf, ToolIntf, EditIntf, ToolsApi;

{$R PGFAPPRES.res}

type
  TSourceIndex = (siPGFAppProject, siPGFAppModule);

  { TPGFAppProjectExpert }
  TPGFAppProjectExpert = class(TIExpert)
  private
    ModuleText: PChar;
    ModuleSources: array[TSourceIndex] of PChar;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName: string; override;
    function GetComment: string; override;
    function GetGlyph: HICON; override;
    function GetStyle: TExpertStyle; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetAuthor: string; override;
    function GetPage: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;

  { TPGFAppProject }
  TPGFAppProject = class(TIProjectCreatorEx)
  public
    function Existing: Boolean; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function NewProjectSource(const ProjectName: string): string; override;
    procedure NewDefaultModule; override;
    procedure NewProjectResource(Module: TIModuleInterface); override;
    function GetOptionName: string; override;
    function NewOptionSource(const ProjectName: string): string; override;
  end;

  { TXPAppModule }
  TPGFAppModule = class(TIModuleCreatorEx)
  public
    function Existing: Boolean; override;
    function GetAncestorName: string; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function GetFormName: string; override;
    function NewModuleSource(const UnitIdent, FormIdent, AncestorIdent: string): string; override;
    procedure FormCreated(Form: TIFormInterface); override;
    function GetIntfName: string; override;
    function NewIntfSource(const UnitIdent, FormIdent, AncestorIdent: string): string; override;
  end;

var
  PGFAppProjectExpert : TPGFAppProjectExpert;
  PGFAppProject: TPGFAppProject;
  PGFAppModule : TPGFAppModule;

procedure Register;
  
implementation

uses Dialogs;
{ TXPAppProjectExpert }

constructor TPGFAppProjectExpert.Create;
var
  ResInstance: THandle;
  ResName: PChar;
  P: PChar;
  SourceIndex: TSourceIndex;
begin
  pgfDesigning := True;
  pgfOpenDisplay('');
  PGFAppProjectExpert:= Self;
  ResInstance:= FindResourceHInstance(HInstance);
  ResName:= 'PGFAPPCODE';
  ModuleText:= StrNew(PChar(LockResource(LoadResource(ResInstance,
    FindResource(ResInstance, ResName, RT_RCDATA)))));
  P:= ModuleText;
  for SourceIndex:= Low(TSourceIndex) to High(TSourceIndex) do
  begin
    ModuleSources[SourceIndex]:= P;
    while P^ <> '|' do
    begin
      if P^ in LeadBytes then Inc(P);
      Inc(P);
    end;
    P^:= #0;
    Inc(P);
  end;
end;

destructor TPGFAppProjectExpert.Destroy;
begin
  PGFAppProjectExpert:= nil;
  StrDispose(ModuleText);
  inherited;
end;

procedure TPGFAppProjectExpert.Execute;
begin
  ToolServices.ProjectCreate(PGFAppProject, [cpApplication]);
end;

function TPGFAppProjectExpert.GetAuthor: string;
begin
  Result:= 'x2nie';
end;

function TPGFAppProjectExpert.GetComment: string;
begin
  Result:= 'Creates a new PGF application';
end;

function TPGFAppProjectExpert.GetGlyph: HICON;
begin
  Result:= LoadIcon(HInstance, 'PGFAPP');
end;

function TPGFAppProjectExpert.GetIDString: string;
begin
  Result:= 'x2nie.pgfProject';
end;

function TPGFAppProjectExpert.GetMenuText: string;
begin
  Result:= '';
end;

function TPGFAppProjectExpert.GetName: string;
begin
  Result:= 'LP Application';
end;

function TPGFAppProjectExpert.GetPage: string;
begin
  Result:= 'New';
end;

function TPGFAppProjectExpert.GetState: TExpertState;
begin
  Result:= [];
end;

function TPGFAppProjectExpert.GetStyle: TExpertStyle;
begin
  Result:= esProject;
end;


{ TXPAppProject }

function TPGFAppProject.Existing: Boolean;
begin
  Result:= False;
end;

function TPGFAppProject.GetFileName: string;
begin
  Result:= '';
end;

function TPGFAppProject.GetFileSystem: string;
begin
  Result:= '';
end;

function TPGFAppProject.GetOptionName: string;
begin
   Result:= '';
end;

procedure TPGFAppProject.NewDefaultModule;
var i : TIModuleInterface;
begin
  i := ToolServices.ModuleCreateEx(PGFAppModule, [cmAddToProject, cmShowSource,
    //cmShowForm,
     cmUnNamed, cmNewFile]);

end;

function TPGFAppProject.NewOptionSource(const ProjectName: string): string;
begin
  Result:= '';
end;

procedure TPGFAppProject.NewProjectResource(Module: TIModuleInterface);
begin
  { Do nothing }
end;

function TPGFAppProject.NewProjectSource(const ProjectName: string): string;
begin
  Result:= Format(PGFAppProjectExpert.ModuleSources[siPGFAppProject],
    [ProjectName]);
end;


{ TXPAppModule }

function TPGFAppModule.Existing: Boolean;
begin
  Result:= False;
end;

procedure TPGFAppModule.FormCreated(Form: TIFormInterface);
begin
  if Form = nil then
    ShowMessage('Apgfmodule form= is nill!')
  else
      ShowMessage('Apgfmodule form='+Form.ClassName);
      
  if Form.GetFormComponent <> nil then
  begin
    //TlpForm( Form.GetFormComponent).Hide;
    //TlpForm( Form.GetFormComponent).Show;
     ShowMessage('Apgfmodule form='+Form.ClassName);
  end;

end;

function TPGFAppModule.GetAncestorName: string;
begin
  Result:= 'lpForm';
end;

function TPGFAppModule.GetFileName: string;
begin
  Result:= '';
end;

function TPGFAppModule.GetFileSystem: string;
begin
  Result:= '';
end;

function TPGFAppModule.GetFormName: string;
begin
  Result:= '';
end;

function TPGFAppModule.GetIntfName: string;
begin
  Result:= '';
end;

function TPGFAppModule.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= '';
end;

function TPGFAppModule.NewModuleSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= Format(PGFAppProjectExpert.ModuleSources[siPGFAppModule],
    [UnitIdent, FormIdent, AncestorIdent]);
end;

procedure Register;
var
  S: TResourceStream;
  Services: IOTAServices;
  TargetFile: string;
begin
  RegisterLibraryExpert(TPGFAppProjectExpert.Create);
  PGFAppModule:= TPGFAppModule.Create;
  PGFAppProject:= TPGFAppProject.Create;
  {S := TResourceStream.Create(HInstance, 'DELPHIXP', RT_RCDATA);
  try
    if Supports(BorlandIDEServices, IOTAServices, Services) then
    begin
      TargetFile := Services.GetBinDirectory + 'DelphiXP.res';
      if not FileExists(TargetFile) then
        S.SaveToFile(TargetFile);
    end;
  finally
    S.Free;
  end;}
end;

initialization
finalization
  PGFAppModule.Free;
  PGFAppProject.Free;
end.

