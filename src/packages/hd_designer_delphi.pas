unit hd_designer_delphi;

interface

uses SysUtils, Classes, hd_defs, hd_main, hd_form, DesignIntf, ComponentDesigner;

type
  ThdDesignerForm = class(TpgfForm, IUnknown, IDesignWindow, IDesignNotification,
    IEditHandler, IActivatable
{$IFDEF LINUX}
    , IDesignerThreadAffinity
{$ENDIF}
    )
  private
    FSelection: IDesignerSelections;
    FOwner: TComponent;
    FDesigner: IDesigner;
    FComponentDesigner: IComponentDesigner;
    FActive: Boolean;
    procedure ComponentRead(Component: TComponent);
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var Name: string);
  protected
    procedure Activated; dynamic;
    procedure ActivateInspector(Ch: Char);
    function ClipboardComponents: Boolean;
    procedure CopyComponents(Root: TComponent;
      const Components: IDesignerSelections);
    procedure PasteComponents(AOwner, AParent: TComponent;
      const Components: IDesignerSelections);
    procedure SetSelection(const Components: IDesignerSelections);
    function UniqueName(Component: TComponent): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // IEditHandler
    function GetEditState: TEditState; virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;

    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean); virtual;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); virtual;
    procedure ItemsModified(const Designer: IDesigner); virtual;

    // IDesignWindowActions
    procedure WindowHide; virtual;
    procedure WindowShow; virtual;

    // IActivatable
    procedure IActivatable.Activate = ForceActive;
    procedure ForceActive;
{$IFDEF LINUX}
    // IDesignerThreadAffinity
    function GetThreadAffinity: TThreadAffinity;
{$ENDIF}
    property Active: Boolean read FActive;
    property Designer: IDesigner read FDesigner write FDesigner;
    property ComponentDesigner: IComponentDesigner read FComponentDesigner;
  end;


procedure Register;

implementation

uses {QClipbrd,} DesignEditors{, ClxEditors}, math;

procedure Register;
begin
//  RegisterCustomModule (TpgfForm, ThdFormModule);
  RegisterCustomModule (TpgfForm, TCustomModule);
  //RegisterCustomModule (TPanel, TPanelModule);
//  RegisterLibraryExpert(TPanelEditExpert.Create);
//  RegisterDesignNotification(ThdDesignerForm as IDesignNotification);

end;


constructor ThdDesignerForm.Create(AOwner: TComponent);
var h, w: integer;
begin
  inherited Create(AOwner);
  FComponentDesigner := ActiveDesigner;
  RegisterDesignNotification(Self);
  w := max(Width, 100);
  h := max(Height, 100);
  SetPosition(200, ComponentDesigner.Environment.GetMainWindowSize.Bottom + 2, w, h);
end;

destructor ThdDesignerForm.Destroy;
begin
  FDesigner := nil;
  FComponentDesigner := nil;
  UnregisterDesignNotification(Self);
  inherited Destroy;
end;

procedure ThdDesignerForm.Activated;
begin
end;

(*procedure TClxDesignWindow.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  FActive := Msg.Active <> 0;
  if FActive then
    Activated;
end;*)

procedure ThdDesignerForm.ActivateInspector(Ch: Char);
begin
  ComponentDesigner.Environment.ModalEdit(Ch, Self);
end;

function ThdDesignerForm.ClipboardComponents: Boolean;
begin
  Result := False;
  try
///    Result := PossibleStream(Clipboard.AsText);
(*    Result := Clipboard.HasFormat(CF_COMPONENTS) or
      (Clipboard.HasFormat(CF_TEXT) and PossibleStream(Clipboard.AsText)); *)
  except
    Result := False;
  end;
end;

procedure ThdDesignerForm.CopyComponents(Root: TComponent;
  const Components: IDesignerSelections);
var
  S: TMemoryStream;
  W: TWriter;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    W := TWriter.Create(S, 1024);
    try
      W.Root := Root;
      for I := 0 to Components.Count - 1 do
      begin
        W.WriteSignature;
        W.WriteComponent(TComponent(Components[I]));
      end;
      W.WriteListEnd;
    finally
      W.Free;
    end;
    ///CopyStreamToClipboard(S); { TODO -ox2nie : clipboard copy component }
  finally
    S.Free;
  end;
end;

function ThdDesignerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function ThdDesignerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure ThdDesignerForm.ForceActive;
begin
  //QWidget_setActiveWindow(Handle);
  HandleSetFocus();
end;

procedure ThdDesignerForm.WindowHide;
begin
  if Visible then
    HandleHide();
end;

procedure ThdDesignerForm.WindowShow;
begin
  if Visible then
    HandleShow();
end;

procedure ThdDesignerForm.ComponentRead(Component: TComponent);
begin
  FSelection.Add(Component);
end;

procedure ThdDesignerForm.ReaderSetName(Reader: TReader; Component: TComponent;
  var Name: string);
begin
  if (Reader.Root = FOwner) and (FOwner.FindComponent(Name) <> nil) then
    Name := UniqueName(Component);
end;

procedure ThdDesignerForm.PasteComponents(AOwner, AParent: TComponent;
  const Components: IDesignerSelections);
var
  S: TStream;
  R: TReader;
begin
  {S := GetClipboardStream;
  try
    R := TReader.Create(S, 1024);
    try
      R.OnSetName := ReaderSetName;
      FOwner := AOwner;
      FSelection := Components;
      R.ReadComponents(AOwner, AParent, ComponentRead);
    finally
      R.Free;
    end;
  finally
    S.Free;
  end;}///x2nie
end;

procedure ThdDesignerForm.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
end;

procedure ThdDesignerForm.SetSelection(const Components: IDesignerSelections);
begin
  ComponentDesigner.SetSelection(Designer, Self, Components);
end;

procedure ThdDesignerForm.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin

end;

procedure ThdDesignerForm.ItemInserted(const ADesigner: IDesigner; Item: TPersistent);
begin

end;

procedure ThdDesignerForm.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if FDesigner = Designer then
  begin
    FDesigner := nil;
    FComponentDesigner := nil;
  end
  else if FDesigner = nil then {  Designer already "closed" }
    FComponentDesigner := nil; { Release the reference to the FComponentDesigner to not AV on shutdown }
end;

procedure ThdDesignerForm.DesignerOpened(const Designer: IDesigner; AResurrecting: Boolean);
begin
  FDesigner := Designer;
  FComponentDesigner := ActiveDesigner;
end;

procedure ThdDesignerForm.ItemsModified(const Designer: IDesigner);
begin

end;

{$IFDEF LINUX}
function ThdDesignerForm.GetThreadAffinity: TThreadAffinity;
begin
  Result := taQT;
end;
{$ENDIF}

function ThdDesignerForm.UniqueName(Component: TComponent): string;
var i : integer;
begin
  i := 1;


  Result := Component.ClassName+'_';// + Now,'yyymmddhhmmss')
end;

end.
