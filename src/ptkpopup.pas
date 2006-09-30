{ popupwindow.pas: Popup window base, mouse capturing etc. Strong couplings to gfxbase.
  File maintainer: nvitya@freemail.hu

History:
}

unit ptkpopup;

{$include lptk_config.inc}

interface

uses
  Classes, SysUtils, lptk, ptkstyle, ptkwidget;

type

  TPopupWindow = class(TptkWidget)
  protected
    DontCloseWidget : TptkWidget;
    
    procedure MsgClose(var msg : TptkMessageRec); message MSG_CLOSE;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ShowAt(wh : TptkWinHandle; x,y : integer);
    procedure Close; virtual;
    
    procedure SetDontCloseWidget(awg : TptkWidget);

  end;

function PopupListFirst : TptkWidget;
function PopupListFind(wh : TptkWinHandle) : TptkWidget;
function PopupListNext(awg : TptkWidget) : TptkWidget;
function PopupDontCloseWidget(awg : TptkWidget) : boolean;

procedure ClosePopups;

implementation

uses
{$ifdef Win32}
  windows,
{$else}
  X, Xlib, Xutil,
{$endif}
  ptkform;

var
  OriginalFocusRoot : TptkWidget;

type
  PPopupListRec = ^PopupListRec;
  PopupListRec = record
    wg   : TPopupWindow;
    Next : PPopupListRec;
  end;

var
  ptkFirstPopup, ptkLastPopup : PPopupListRec;

procedure ClosePopups;
begin
  while ptkFirstPopup <> nil do
  begin
    TPopupWindow(ptkFirstPopup^.wg).Close;
  end;
end;

procedure PopupListAdd(pwg : TPopupWindow);
var
  p : PPopupListRec;
begin
  if pwg = nil then Exit;
  
  if ptkFirstPopup = nil then
  begin
    OriginalFocusRoot := FocusRoot;
  end;
  
  FocusRoot := pwg;

  New(p);
  p^.wg := pwg;
  p^.Next := nil;
  if ptkFirstPopup = nil then ptkFirstPopup := p
                         else ptkLastPopup^.Next := p;
  ptkLastPopup := p;
end;

procedure PopupListRemove(wg : TptkWidget);
var
  prevp, p, px : PPopupListRec;
begin
  p := ptkFirstPopup;
  prevp := nil;

  while p <> nil do
  begin
    if p^.wg = wg then
    begin
      if prevp = nil then ptkFirstPopup := p^.Next
                     else prevp^.Next := p^.Next;
      if ptkLastPopup = p then ptkLastPopup := prevp;
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
  
  if ptkLastPopup <> nil then FocusRoot := ptkLastPopup^.wg
                         else FocusRoot := OriginalFocusRoot;
end;

function PopupListFirst : TptkWidget;
begin
  if ptkFirstPopup <> nil then result := ptkFirstPopup^.wg
                          else result := nil;
end;

function PopupListFind(wh : TptkWinHandle) : TptkWidget;
var
  p : PPopupListRec;
begin
  p := ptkFirstPopup;
  while p <> nil do
  begin
    if p^.wg.WinHandle = wh then
    begin
      Result := p^.wg;
      Exit;
    end;
    p := p^.Next;
  end;
  result := nil;
end;

function PopupDontCloseWidget(awg : TptkWidget) : boolean;
var
  p : PPopupListRec;
begin
  result := false;
  if awg = nil then Exit;
  
  p := ptkFirstPopup;
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

function PopupListNext(awg : TptkWidget) : TptkWidget;
var
  p : PPopupListRec;
begin
  p := ptkFirstPopup;
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

{ TPopupWindow }

procedure TPopupWindow.MsgClose(var msg: TptkMessageRec);
begin
  Close;
end;

constructor TPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWPOverride := true;
  FParent := nil;
  DontCloseWidget := nil;
end;

destructor TPopupWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TPopupWindow.ShowAt(wh : TptkWinHandle; x,y : integer);
{$ifdef Win32}
var
  pt : TPoint;
begin
  PopupListAdd(self);
  DontCloseWidget := nil;

  pt.X := x;
  pt.Y := y;
  ClientToScreen(wh, pt);

  Left := pt.x;
  Top  := pt.y;
  DoShow;

  SetCapture(FWinHandle);
end;
{$else}
var
  dx,dy : integer;
  cw : TptkWinHandle;
begin
  XTranslateCoordinates(display, wh, ptkRootWindow, x, y, @dx, @dy, @cw);

  Left := dx;
  Top  := dy;
  DoShow;

  if ptkMainForm = nil then
  begin
    Writeln('No popups available if mainform is not set.');
  end
  else
  begin
    XGrabPointer(display, ptkMainForm.WinHandle,
         true,
         ButtonPressMask or ButtonReleaseMask or ButtonMotionMask or PointerMotionMask,
         GrabModeAsync,
         GrabModeAsync,
         None,
         0,
         0 //fl_event_time
         );

    PopupListAdd(self);
    DontCloseWidget := nil;
  end;

  //XAllowEvents(display, ReplayPointer, 0);
end;
{$endif}

procedure TPopupWindow.Close;
begin
  DoHide;
  PopupListRemove(self);
{$ifdef Win32}
  ReleaseCapture;
  if ptkFirstPopup <> nil then SetCapture(ptkFirstPopup^.wg.WinHandle);
{$else}
  if ptkFirstPopup = nil then XUngrabPointer(display, 0);
{$endif}
end;

procedure TPopupWindow.SetDontCloseWidget(awg: TptkWidget);
begin
  DontCloseWidget := awg;
end;

procedure TPopupWindow.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
begin
  inherited;
  //Writeln('mouse x=',x,' y=',y);
end;

initialization      
begin
  ptkFirstPopup := nil;
  ptkLastPopup := nil;
end;

end.

