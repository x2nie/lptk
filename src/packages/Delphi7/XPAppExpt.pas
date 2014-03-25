unit XPAppExpt;

interface

procedure Register;

implementation

uses
  Classes, SysUtils, Windows, ExptIntf, ToolIntf, EditIntf, ToolsApi;

{$R XPAPPRES.res}

type
  TSourceIndex = (siXPAppProject, siXPAppModule);

  { TXPAppProjectExpert }
  TXPAppProjectExpert = class(TIExpert)
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

  { TXPAppProject }
  TXPAppProject = class(TIProjectCreatorEx)
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
  TXPAppModule = class(TIModuleCreatorEx)
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
  XPAppProjectExpert : TXPAppProjectExpert;
  XPAppProject: TXPAppProject;
  XPAppModule : TXPAppModule;


{ TXPAppProjectExpert }

constructor TXPAppProjectExpert.Create;
var
  ResInstance: THandle;
  ResName: PChar;
  P: PChar;
  SourceIndex: TSourceIndex;
begin
  XPAppProjectExpert:= Self;
  ResInstance:= FindResourceHInstance(HInstance);
  ResName:= 'XPAPPCODE';
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

destructor TXPAppProjectExpert.Destroy;
begin
  XPAppProjectExpert:= nil;
  StrDispose(ModuleText);
  inherited;
end;

procedure TXPAppProjectExpert.Execute;
begin
  ToolServices.ProjectCreate(XPAppProject, [cpApplication]);
end;

function TXPAppProjectExpert.GetAuthor: string;
begin
  Result:= 'Borland';
end;

function TXPAppProjectExpert.GetComment: string;
begin
  Result:= 'Creates a new XP enabled application';
end;

function TXPAppProjectExpert.GetGlyph: HICON;
begin
  Result:= LoadIcon(HInstance, 'XPAPP');
end;

function TXPAppProjectExpert.GetIDString: string;
begin
  Result:= 'Borland.XPProject';
end;

function TXPAppProjectExpert.GetMenuText: string;
begin
  Result:= '';
end;

function TXPAppProjectExpert.GetName: string;
begin
  Result:= 'XP Application';
end;

function TXPAppProjectExpert.GetPage: string;
begin
  Result:= 'New';
end;

function TXPAppProjectExpert.GetState: TExpertState;
begin
  Result:= [];
end;

function TXPAppProjectExpert.GetStyle: TExpertStyle;
begin
  Result:= esProject;
end;


{ TXPAppProject }

function TXPAppProject.Existing: Boolean;
begin
  Result:= False;
end;

function TXPAppProject.GetFileName: string;
begin
  Result:= '';
end;

function TXPAppProject.GetFileSystem: string;
begin
  Result:= '';
end;

function TXPAppProject.GetOptionName: string;
begin
   Result:= '';
end;

procedure TXPAppProject.NewDefaultModule;
begin
  ToolServices.ModuleCreateEx(XPAppModule, [cmAddToProject, cmShowSource,
    cmShowForm, cmUnNamed, cmNewFile]);
end;

function TXPAppProject.NewOptionSource(const ProjectName: string): string;
begin
  Result:= '';
end;

procedure TXPAppProject.NewProjectResource(Module: TIModuleInterface);
begin
  { Do nothing }
end;

function TXPAppProject.NewProjectSource(const ProjectName: string): string;
begin
  Result:= Format(XPAppProjectExpert.ModuleSources[siXPAppProject],
    [ProjectName]);
end;


{ TXPAppModule }

function TXPAppModule.Existing: Boolean;
begin
  Result:= False;
end;

procedure TXPAppModule.FormCreated(Form: TIFormInterface);
begin
end;

function TXPAppModule.GetAncestorName: string;
begin
  Result:= 'Form';
end;

function TXPAppModule.GetFileName: string;
begin
  Result:= '';
end;

function TXPAppModule.GetFileSystem: string;
begin
  Result:= '';
end;

function TXPAppModule.GetFormName: string;
begin
  Result:= '';
end;

function TXPAppModule.GetIntfName: string;
begin
  Result:= '';
end;

function TXPAppModule.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= '';
end;

function TXPAppModule.NewModuleSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result:= Format(XPAppProjectExpert.ModuleSources[siXPAppModule],
    [UnitIdent, FormIdent, AncestorIdent]);
end;

procedure Register;
var
  S: TResourceStream;
  Services: IOTAServices;
  TargetFile: string;
begin
  RegisterLibraryExpert(TXPAppProjectExpert.Create);
  XPAppModule:= TXPAppModule.Create;
  XPAppProject:= TXPAppProject.Create;
  S := TResourceStream.Create(HInstance, 'DELPHIXP', RT_RCDATA);
  try
    if Supports(BorlandIDEServices, IOTAServices, Services) then
    begin
      TargetFile := Services.GetBinDirectory + 'DelphiXP.res';
      if not FileExists(TargetFile) then
        S.SaveToFile(TargetFile);
    end;
  finally
    S.Free;
  end;
end;

initialization
finalization
  XPAppModule.Free;
  XPAppProject.Free;
end.
