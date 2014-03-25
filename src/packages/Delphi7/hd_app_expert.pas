unit hd_app_expert;

interface

procedure Register;

implementation

uses
  Classes, SysUtils, Windows, hd_defs, hd_main, hd_form, hd_widget,
  ExptIntf, ToolIntf, EditIntf, ToolsApi;

{$R HDAPPRES.res}

type
  TSourceIndex = (siHDAppProject, siHDAppModule);

  { THDAppProjectExpert }
  THDAppProjectExpert = class(TIExpert)
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

  { THDAppProject }
  THDAppProject = class(TIProjectCreatorEx)
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
  THDAppModule = class(TIModuleCreatorEx)
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
  HDAppProjectExpert : THDAppProjectExpert;
  HDAppProject: THDAppProject;
  HDAppModule : THDAppModule;


{ TXPAppProjectExpert }

constructor THDAppProjectExpert.Create;
var
  ResInstance: THandle;
  ResName: PChar;
  P: PChar;
  SourceIndex: TSourceIndex;
begin
//  pgfOpenDisplay('');
  HDAppProjectExpert:= Self;
  ResInstance:= FindResourceHInstance(HInstance);
  ResName:= 'HDAPPCODE';
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

destructor THDAppProjectExpert.Destroy;
begin
  HDAppProjectExpert:= nil;
  StrDispose(ModuleText);
  inherited;
end;

procedure THDAppProjectExpert.Execute;
begin
  ToolServices.ProjectCreate(HDAppProject, [cpApplication]);
end;

function THDAppProjectExpert.GetAuthor: string;
begin
  Result:= 'x2nie';
end;

function THDAppProjectExpert.GetComment: string;
begin
  Result:= 'Creates a new HD application';
end;

function THDAppProjectExpert.GetGlyph: HICON;
begin
  Result:= LoadIcon(HInstance, 'HDAPP');
end;

function THDAppProjectExpert.GetIDString: string;
begin
  Result:= 'x2nie.hdProject';
end;

function THDAppProjectExpert.GetMenuText: string;
begin
  Result:= '';
end;

function THDAppProjectExpert.GetName: string;
begin
  Result:= 'HD Application';
end;

function THDAppProjectExpert.GetPage: string;
begin
  Result:= 'New';
end;

function THDAppProjectExpert.GetState: TExpertState;
begin
  Result:= [];
end;

function THDAppProjectExpert.GetStyle: TExpertStyle;
begin
  Result:= esProject;
end;


{ TXPAppProject }

function THDAppProject.Existing: Boolean;
begin
  Result:= False;
end;

function THDAppProject.GetFileName: string;
begin
  Result:= '';
end;

function THDAppProject.GetFileSystem: string;
begin
  Result:= '';
end;

function THDAppProject.GetOptionName: string;
begin
   Result:= '';
end;

procedure THDAppProject.NewDefaultModule;
begin
  ToolServices.ModuleCreateEx(HDAppModule, [cmAddToProject, cmShowSource,
    cmShowForm, cmUnNamed, cmNewFile]);
end;

function THDAppProject.NewOptionSource(const ProjectName: string): string;
begin
  Result:= '';
end;

procedure THDAppProject.NewProjectResource(Module: TIModuleInterface);
begin
  { Do nothing }
end;

function THDAppProject.NewProjectSource(const ProjectName: string): string;
begin
  Result:= Format(HDAppProjectExpert.ModuleSources[siHDAppProject],
    [ProjectName]);
end;


{ TXPAppModule }

function THDAppModule.Existing: Boolean;
begin
  Result:= False;
end;

procedure THDAppModule.FormCreated(Form: TIFormInterface);
begin
end;

function THDAppModule.GetAncestorName: string;
begin
  Result:= 'pgfForm';
end;

function THDAppModule.GetFileName: string;
begin
  Result:= '';
end;

function THDAppModule.GetFileSystem: string;
begin
  Result:= '';
end;

function THDAppModule.GetFormName: string;
begin
  Result:= '';
end;

function THDAppModule.GetIntfName: string;
begin
  Result:= '';
end;

function THDAppModule.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= '';
end;

function THDAppModule.NewModuleSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= Format(HDAppProjectExpert.ModuleSources[siHDAppModule],
    [UnitIdent, FormIdent, AncestorIdent]);
end;

procedure Register;
var
  S: TResourceStream;
  Services: IOTAServices;
  TargetFile: string;
begin
  RegisterLibraryExpert(THDAppProjectExpert.Create);
  HDAppModule:= THDAppModule.Create;
  HDAppProject:= THDAppProject.Create;
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
  HDAppModule.Free;
  HDAppProject.Free;
end.
