unit lp_form;

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils, lp_defs, lp_widget;
  
type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter);

  { TpgfForm }

  TlpForm = class(TpgfWidget)
  private
    FText: widestring;
  protected
    FPrevModalForm : TlpForm;
    FModalResult : integer;

    FParentForm : TlpForm;

    FWindowPosition : TWindowPosition;
    //FCaption : widestring;
    FSizeable : boolean;

    //FBackgroundColor : TpgfColor;

    procedure AdjustWindowStyle; override;
    procedure SetWindowParameters; override;

    //procedure SetCaption(const AValue: widestring);
    procedure SetText(AValue: widestring); override;

  protected
    procedure MsgActivate(var msg : TpgfMessageRec); message PGFM_ACTIVATE;
    procedure MsgDeActivate(var msg : TpgfMessageRec); message PGFM_DEACTIVATE;

    procedure MsgClose(var msg : TpgfMessageRec); message PGFM_CLOSE;

  protected
    procedure HandlePaint; override;

    procedure HandleClose; virtual;
    procedure   HandleMove(x, y: TpgfCoord); override;
    procedure   HandleResize(awidth, aheight: TpgfCoord); override;
    procedure HandleShortcut(const AOrigin: TpgfWidget; const keycode: word; const shiftstate: TShiftState; var Handled: boolean; const IsChildOfOrigin: boolean = False); override;
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
    property Caption : widestring read FText write SetText;
  end;


var
  pgfMainForm : TlpForm;

  pgfTopModalForm : TlpForm;

function WidgetParentForm(wg : TpgfWidget) : TlpForm;




implementation

uses lp_main, lp_popupwindow, lp_menu;

type
  TlpMenuBarAccess = class(TlpMenuBar);
  TpgfWidgetAccess = class(TpgfWidget);

function WidgetParentForm(wg : TpgfWidget) : TlpForm;
var
  w : TpgfWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TlpForm then
    begin
      Result := TlpForm(w);
      Exit;
    end;
    w := w.Parent;
  end;
  result := nil;
end;

{ TpgfForm }

procedure TlpForm.SetText(AValue: widestring);
begin
  inherited SetText(AValue);
  inherited DoSetWindowTitle(FText);
end;

procedure TlpForm.HandlePaint;
begin
  canvas.BeginDraw;
  canvas.Clear(FBackgroundColor);
  //canvas.Clear($009955);
  canvas.EndDraw(0,0,FWidth,FHeight);
end;

procedure TlpForm.AdjustWindowStyle;
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

procedure TlpForm.SetWindowParameters;
begin
  inherited;

  DoSetWindowTitle(Text);
end;

constructor TlpForm.Create(aowner: TComponent);
begin
  inherited;
  FWindowPosition := wpAuto;
  //FCaption := '';
  FSizeable := true;
  FParentForm := nil;
  //FBackgroundColor := clWindowBackground;
  FMinWidth := 32;
  FMinHeight := 32;
  FWidth := 320;
  FHeight := 240;

  FModalResult := 0;

  FPrevModalForm := nil;
  try
    InitInheritedComponent (self, TlpForm);
  except
  end;
  AfterCreate;
end;

procedure TlpForm.AfterCreate;
begin
  // for the user
end;

procedure TlpForm.Show;
begin
  HandleShow;
end;

function TlpForm.ShowModal: integer;
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

procedure TlpForm.MsgActivate(var msg: TpgfMessageRec);
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

procedure TlpForm.MsgDeActivate(var msg: TpgfMessageRec);
begin
  ClosePopups;
  if ActiveWidget <> nil then ActiveWidget.KillFocus;
end;

procedure TlpForm.MsgClose(var msg: TpgfMessageRec);
begin
  HandleClose;
end;

procedure TlpForm.HandleClose;
begin
  Close;
end;

procedure TlpForm.Hide;
begin
  if (pgfTopModalForm = self) then pgfTopModalForm := self.FPrevModalForm;
  HandleHide;
  if ModalResult = 0 then ModalResult := -1;
end;

procedure TlpForm.Close;
begin
  Hide;
  if pgfMainForm = self then
  begin
    Halt(0);
  end;
end;

procedure TlpForm.HandleMove(x, y: TpgfCoord);
begin
  ClosePopups;
  inherited HandleMove(x, y);

end;

procedure TlpForm.HandleResize(awidth, aheight: TpgfCoord);
begin
  ClosePopups;
  inherited HandleResize(awidth, aheight);

end;

procedure TlpForm.HandleShortcut(const AOrigin: TpgfWidget;
  const keycode: word; const shiftstate: TShiftState; var Handled: boolean;
  const IsChildOfOrigin: boolean);
var
  wg: TpgfWidget;
  menu: TlpMenuBar;
  i: integer;

  c: TComponent;

  function AskMenuBars(AWidget: TpgfWidget): Boolean;
  var
    n: integer;
    w: TpgfWidget;
    m: TlpMenuBarAccess;
    ss: TShiftState;
    key: word;
  begin
    Result := False;
    for n := 0 to AWidget.ChildCount-1 do
    begin
      w := AWidget.Children[n];
      if (w <> nil) and (w <> self) and (w <> AOrigin) and (w is TlpMenuBar) then
      begin
        m := TlpMenuBarAccess(w);
        key := KeyCode;
        ss := ShiftState;
        m.HandleKeyPress(key, ss, Handled);
        if Handled then
        begin
          Result := True;
          exit;
        end;
      end;
      if w.ChildCount > 0 then
        Result := AskMenuBars(w);
      if Result then
        exit;
    end;
  end;

begin
  //writeln('TlpForm.HandleShortcut ',ShortCutToText(keycode, shiftstate));
  // find the first TpgfMenuBar - if it exits
  AskMenuBars(self);
  {if (wg <> nil) then
  begin
    menu := wg as TlpMenuBar;
    key := keycode;
    ss := shiftstate;
    TpgfMenuBarFriend(wg).HandleKeyPress(key, ss, Handled);
  end;}

  if Handled then
    Exit;
  // now send to each widget on the form - excluding AOrigin and MenuBar widgets
  for i := 0 to ComponentCount-1 do
  begin
    c := TComponent(Components[i]);
    if c is TpgfWidget then
      wg := TpgfWidget(c)
    else
      wg := nil;
    if (wg <> nil) and (wg <> self) and (wg <> AOrigin) and (wg <> menu) and (not (wg is TlpPopupMenu)) then
    begin
      if (not wg.Visible) or (not wg.Enabled) then
        continue
      else
      begin
        TpgfWidgetAccess(wg).HandleShortcut(AOrigin, keycode, shiftstate, Handled);
        if Handled then
          Exit;
      end;
    end;
  end;
end;

initialization
begin
  pgfMainForm := nil;
  pgfTopModalForm := nil;
end;

end.

