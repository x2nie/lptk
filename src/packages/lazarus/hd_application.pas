unit hd_application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms,
  hd_form;
type
  { TProjectApplicationDescriptor }

  THDApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

{ TFileDescPascalUnitWithPgfForm }

  TFileDescPascalUnitWithPgfForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; override;
    //function GetImplementationSource(const Filename, SourceName,
       //                              ResourceName: string): string; override;
  end;


procedure Register;

implementation
uses hd_designer;

procedure Register;
begin
  //FileDescPascalUnitWithPgfForm := TFileDescPascalUnitWithPgfForm.Create();
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithPgfForm.Create,
                                FileDescGroupName);
  RegisterProjectDescriptor(THDApplicationDescriptor.Create);
end;

function FileDescriptorHDForm() : TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName('pgfForm');
end;

{ TFileDescPascalUnitWithPgfForm }

constructor TFileDescPascalUnitWithPgfForm.Create;
begin
  inherited Create;
  Name:='pgfForm';
  ResourceClass:=TpgfForm;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithPgfForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, hd_defs, hd_main, hd_form';
end;

function TFileDescPascalUnitWithPgfForm.GetLocalizedName: string;
begin
  Result:='pgfForm';
end;

function TFileDescPascalUnitWithPgfForm.GetLocalizedDescription: string;
begin
  Result:='Create a new pgfForm for HD Application';
end;

function TFileDescPascalUnitWithPgfForm.GetUnitDirectives: string;
begin
  result := inherited GetUnitDirectives();
  result := '{$ifdef fpc}'+ LineEnding
           +result + LineEnding
           +'{$endif}';
end;

{function TFileDescPascalUnitWithPgfForm.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:='{$R *.dfm}'+LineEnding+LineEnding;
end;}

{ TProjectApplicationDescriptor }

constructor THDApplicationDescriptor.Create;
begin
  inherited;
  Name := 'HD Application';  
end;

function THDApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorHDForm,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

function THDApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'HD Toolkit Application'+LineEnding+LineEnding
           +'An application based on the HD Toolkit.'+LineEnding
           +'The program file is automatically maintained by Lazarus.';
end;

function THDApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'HD Toolkit Application';
end;

function THDApplicationDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.dpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.LoadDefaultIcon;

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$ifdef fpc}'+LineEnding
    +'{$mode delphi}{$H+}'+LineEnding
    +'{$endif}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    //+'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+LineEnding
    //+'  cthreads,'+LineEnding
    //+'  {$ENDIF}{$ENDIF}'+LineEnding
    //+'  Interfaces, // this includes the LCL widgetset'+LineEnding
    +'  hd_defs, hd_main, hd_form '+LineEnding
    +'  { you can add units after this };'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    //+'  RequireDerivedFormResource := True;'+LineEnding
    +'  Application.Initialize;'+LineEnding
    +'  Application.Run;'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  // add lcl pp/pas dirs to source search path
  AProject.AddPackageDependency('FCL');
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
end;

end.

