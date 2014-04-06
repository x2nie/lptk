unit lp_popup;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_widget;

type

  { TpgfPopupWindow }

  TpgfPopupWindow = class(TpgfWidget)
  protected
    DontCloseWidget : TpgfWidget;

    procedure MsgClose(var msg : TpgfMessageRec); message PGFM_CLOSE;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ShowAt(atwindow : TpgfWindow; x,y : integer);
    procedure HandleHide; override;

    procedure HandlePaint; override;

    procedure SetDontCloseWidget(awg : TpgfWidget);

    procedure Hide;
    procedure Close;

  end;

function PopupListFirst : TpgfWidget;
function PopupListFind(wg : TpgfWidget) : boolean;
function PopupListNext(awg : TpgfWidget) : TpgfWidget;
function PopupDontCloseWidget(awg : TpgfWidget) : boolean;

procedure ClosePopups;

implementation

var
  OriginalFocusRoot : TpgfWidget;

type
  PPopupListRec = ^PopupListRec;
  PopupListRec = record
    wg   : TpgfPopupWindow;
    Next : PPopupListRec;
  end;

var
  pgfFirstPopup, pgfLastPopup : PPopupListRec;

procedure PopupListAdd(pwg : TpgfPopupWindow);
var
  p : PPopupListRec;
begin
  if pwg = nil then Exit;

  if pgfFirstPopup = nil then
  begin
    OriginalFocusRoot := FocusRootWidget;
  end;

  FocusRootWidget := pwg;

  New(p);
  p^.wg := pwg;
  p^.Next := nil;
  if pgfFirstPopup = nil then pgfFirstPopup := p
                         else pgfLastPopup^.Next := p;
  pgfLastPopup := p;
end;

procedure PopupListRemove(wg : TpgfWidget);
var
  prevp, p, px : PPopupListRec;
begin
  p := pgfFirstPopup;
  prevp := nil;

  while p <> nil do
  begin
    if p^.wg = wg then
    begin
      if prevp = nil then pgfFirstPopup := p^.Next
                     else prevp^.Next := p^.Next;
      if pgfLastPopup = p then pgfLastPopup := prevp;
      px := p;
      p := p^.Next;
      Dispose(px);
    end
    else
    begin
      prevp := p;
      p := p^.Next;
    end;
  end;

  if pgfLastPopup <> nil then FocusRootWidget := pgfLastPopup^.wg
                         else FocusRootWidget := OriginalFocusRoot;
end;


function PopupListFirst : TpgfWidget;
begin
  if pgfFirstPopup <> nil then result := pgfFirstPopup^.wg
                          else result := nil;

end;

function PopupListFind(wg : TpgfWidget) : boolean;
var
  p : PPopupListRec;
begin
  p := pgfFirstPopup;
  while p <> nil do
  begin
    if p^.wg = wg then
    begin
      Result := true;
      Exit;
    end;
    p := p^.Next;
  end;
  result := false;
end;

function PopupListNext(awg : TpgfWidget) : TpgfWidget;
var
  p : PPopupListRec;
begin
  p := pgfFirstPopup;
  while p <> nil do
  begin
    if p^.wg = awg then
    begin
      if p^.Next <> nil then Result := p^.Next^.wg
                        else Result := nil;
      Exit;
    end;
    p := p^.Next;
  end;
  result := nil;
end;

function PopupDontCloseWidget(awg : TpgfWidget) : boolean;
var
  p : PPopupListRec;
begin
  result := false;
  if awg = nil then Exit;

  p := pgfFirstPopup;
  while p <> nil do
  begin
    if p^.wg.DontCloseWidget = awg then
    begin
      Result := true;
      Exit;
    end;
    p := p^.Next;
  end;
end;

procedure ClosePopups;
begin
  while pgfFirstPopup <> nil do
  begin
    TpgfPopupWindow(pgfFirstPopup^.wg).Close;
  end;
end;

{ TpgfPopupWindow }

procedure TpgfPopupWindow.MsgClose(var msg : TpgfMessageRec);
begin
  HandleHide;
end;

constructor TpgfPopupWindow.Create(AOwner : TComponent);
begin
  inherited;
  WindowType := wtPopup;
  DontCloseWidget := nil;
end;

destructor TpgfPopupWindow.Destroy;
begin
  inherited;
end;

procedure TpgfPopupWindow.ShowAt(atwindow : TpgfWindow; x, y : integer);
begin
  if HasHandle then Hide;

  pgfDisp.GetScreenCoordinates(atwindow,x,y, FLeft, FTop);

  HandleShow;

  pgfDisp.GrabPointer(self);

  PopupListAdd(self);
  DontCloseWidget := nil;
end;

procedure TpgfPopupWindow.HandleHide;
begin
  pgfDisp.UnGrabPointer;

  inherited;

  PopupListRemove(self);

  if pgfFirstPopup <> nil then
  begin
    pgfDisp.GrabPointer(pgfFirstPopup^.wg);
  end;
end;

procedure TpgfPopupWindow.HandlePaint;
begin
  canvas.BeginDraw;
  canvas.Clear(clWindowBackground);
  canvas.EndDraw(0,0,FWidth,FHeight);
end;

procedure TpgfPopupWindow.SetDontCloseWidget(awg : TpgfWidget);
begin
  DontCloseWidget := awg;
end;

procedure TpgfPopupWindow.Hide;
begin
  HandleHide;
end;

procedure TpgfPopupWindow.Close;
begin
  HandleHide;
end;

initialization
begin
  pgfFirstPopup := nil;
  pgfLastPopup := nil;
end;


end.

