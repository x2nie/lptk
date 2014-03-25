unit hd_form;

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils, hd_defs, hd_widget;
  
type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter);

  TpgfForm = class(TpgfWidget)
  protected
    FPrevModalForm : TpgfForm;
    FModalResult : integer;

    FParentForm : TpgfForm;

    FWindowPosition : TWindowPosition;
    FCaption : widestring;
    FSizeable : boolean;

    FBackgroundColor : TpgfColor;

    procedure AdjustWindowStyle; override;
    procedure SetWindowParameters; override;

    procedure SetCaption(const AValue: widestring);

  protected
    procedure MsgActivate(var msg : TpgfMessageRec); message PGFM_ACTIVATE;
    procedure MsgDeActivate(var msg : TpgfMessageRec); message PGFM_DEACTIVATE;

    procedure MsgClose(var msg : TpgfMessageRec); message PGFM_CLOSE;

  protected
    procedure HandlePaint; override;

    procedure HandleClose; virtual;

  public
    constructor Create(aowner : TComponent); override;

    procedure AfterCreate; virtual;

    procedure Show;
    procedure Hide;
    function ShowModal : integer;
    procedure Close;

    property Sizeable : boolean read FSizeable write FSizeable;
    property WindowPosition : TWindowPosition read FWindowPosition write FWindowPosition;

    property ModalResult : integer read FModalResult write FModalResult;
  published
    property Caption : widestring read FCaption write SetCaption;
  end;


var
  pgfMainForm : TpgfForm;

  pgfTopModalForm : TpgfForm;

function WidgetParentForm(wg : TpgfWidget) : TpgfForm;

implementation

uses hd_main;

function WidgetParentForm(wg : TpgfWidget) : TpgfForm;
var
  w : TpgfWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TpgfForm then
    begin
      Result := TpgfForm(w);
      Exit;
    end;
    w := w.Parent;
  end;
  result := nil;
end;

{ TpgfForm }

procedure TpgfForm.SetCaption(const AValue: widestring);
begin
  FCaption := avalue;
  inherited DoSetWindowTitle(FCaption);
end;

procedure TpgfForm.HandlePaint;
begin
  canvas.BeginDraw;
  //canvas.Clear(FBackgroundColor);
  canvas.Clear($009955);
  canvas.EndDraw(0,0,FWidth,FHeight);
end;

procedure TpgfForm.AdjustWindowStyle;
begin
  if pgfMainForm = nil then pgfMainForm := self;
  
{
  if (FocusRoot <> nil) and (FocusRoot is TptkForm)
    then FParentForm := TptkForm(FocusRoot)
    else FParentForm := nil;
}

  if FWindowPosition = wpAuto
    then Include(FWindowAttributes, waAutoPos)
    else Exclude(FWindowAttributes, waAutoPos);

  if FWindowPosition = wpScreenCenter
    then Include(FWindowAttributes, waScreenCenterPos)
    else Exclude(FWindowAttributes, waScreenCenterPos);

  if FSizeable
    then Include(FWindowAttributes, waSizeable)
    else Exclude(FWindowAttributes, waSizeable);
end;

procedure TpgfForm.SetWindowParameters;
begin
  inherited;

  DoSetWindowTitle(FCaption);
end;

constructor TpgfForm.Create(aowner: TComponent);
begin
  inherited;
  FWindowPosition := wpAuto;
  FCaption := '';
  FSizeable := true;
  FParentForm := nil;
  FBackgroundColor := clWindowBackground;
  FMinWidth := 32;
  FMinHeight := 32;

  FModalResult := 0;

  FPrevModalForm := nil;
  
  AfterCreate;
end;

procedure TpgfForm.AfterCreate;
begin
  // for the user
end;

procedure TpgfForm.Show;
begin
  HandleShow;
end;

function TpgfForm.ShowModal: integer;
begin
  FPrevModalForm := pgfTopModalForm;
  pgfTopModalForm := self;

  ModalResult := 0;

  Show;

  // processing messages until this form ends.

  // delivering the remaining messages
  pgfProcessMessages;

  repeat
    pgfWaitWindowMessage;
  until (ModalResult <> 0) or (not Visible);


  pgfTopModalForm := FPrevModalForm;

  Result := ModalResult;
end;

procedure TpgfForm.MsgActivate(var msg: TpgfMessageRec);
begin
  if (pgfTopModalForm = nil) or (pgfTopModalForm = self) then
  begin
    FocusRootWidget := self;
{
    if FFormDesigner <> nil then
    begin
      FFormDesigner.Dispatch(msg);
      Exit;
    end;
}
    if ActiveWidget = nil then ActiveWidget := FindFocusWidget(nil, fsdFirst);
    if ActiveWidget <> nil then ActiveWidget.SetFocus;
  end;
end;

procedure TpgfForm.MsgDeActivate(var msg: TpgfMessageRec);
begin
  if ActiveWidget <> nil then ActiveWidget.KillFocus;
end;

procedure TpgfForm.MsgClose(var msg: TpgfMessageRec);
begin
  HandleClose;
end;

procedure TpgfForm.HandleClose;
begin
  Close;
end;

procedure TpgfForm.Hide;
begin
  if (pgfTopModalForm = self) then pgfTopModalForm := self.FPrevModalForm;
  HandleHide;
  if ModalResult = 0 then ModalResult := -1;
end;

procedure TpgfForm.Close;
begin
  Hide;
  if pgfMainForm = self then
  begin
    Halt(0);
  end;
end;

initialization
begin
  pgfMainForm := nil;
  pgfTopModalForm := nil;
end;

end.

