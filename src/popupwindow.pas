{ popupwindow.pas: Popup window base, mouse capturing etc. Strong couplings to gfxbase.
  File maintainer: nvitya@freemail.hu

History:
}

unit popupwindow;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, messagequeue, gfxbase, GfxStyle, GfxWidget;

type

  TPopupWindow = class(TWidget)
  protected
    procedure MsgClose(var msg : TMessageRec); message MSG_CLOSE;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ShowAt(wh : TWinHandle; x,y : integer);
    procedure Close;

  end;

function PopupListFirst : TWidget;
function PopupListFind(wh : TWinHandle) : TWidget;
function PopupListNext(awg : TWidget) : TWidget;

procedure ClosePopups;

implementation

uses
{$ifdef Win32}
  windows,
{$else}
  X, Xlib, Xutil,
{$endif}
  gfxform;

var
  OriginalFocusRoot : TWidget;

type
  PPopupListRec = ^PopupListRec;
  PopupListRec = record
    wg   : TWidget;
    Next : PPopupListRec;
  end;

var
  GfxFirstPopup, GfxLastPopup : PPopupListRec;

procedure ClosePopups;
begin
  while GfxFirstPopup <> nil do
  begin
    TPopupWindow(GfxFirstPopup^.wg).Close;
  end;
end;

procedure PopupListAdd(wg : TWidget);
var
  p : PPopupListRec;
begin
  if wg = nil then Exit;
  
  if GfxFirstPopup = nil then
  begin
    OriginalFocusRoot := FocusRoot;
  end;
  
  FocusRoot := wg;

  New(p);
  p^.wg := wg;
  p^.Next := nil;
  if GfxFirstPopup = nil then GfxFirstPopup := p
                         else GfxLastPopup^.Next := p;
  GfxLastPopup := p;
end;

procedure PopupListRemove(wg : TWidget);
var
  prevp, p, px : PPopupListRec;
begin
  p := GfxFirstPopup;
  prevp := nil;

  while p <> nil do
  begin
    if p^.wg = wg then
    begin
      if prevp = nil then GfxFirstPopup := p^.Next
                     else prevp^.Next := p^.Next;
      if GfxLastPopup = p then GfxLastPopup := prevp;
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
  
  if GfxLastPopup <> nil then FocusRoot := GfxLastPopup^.wg
                         else FocusRoot := OriginalFocusRoot;
end;

function PopupListFirst : TWidget;
begin
  if GfxFirstPopup <> nil then result := GfxFirstPopup^.wg
                          else result := nil;
end;

function PopupListFind(wh : TWinHandle) : TWidget;
var
  p : PPopupListRec;
begin
  p := GfxFirstPopup;
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

function PopupListNext(awg : TWidget) : TWidget;
var
  p : PPopupListRec;
begin
  p := GfxFirstPopup;
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

procedure TPopupWindow.MsgClose(var msg: TMessageRec);
begin
  Close;
end;

constructor TPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWPOverride := true;
  FParent := nil;
end;

destructor TPopupWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TPopupWindow.ShowAt(wh : TWinHandle; x,y : integer);
{$ifdef Win32}
var
  pt : TPoint;
begin
  pt.X := x;
  pt.Y := y;
  ClientToScreen(wh, pt);

  Left := pt.x;
  Top  := pt.y;
  DoShow;

  SetCapture(FWinHandle);

  PopupListAdd(self);
end;
{$else}
var
  dx,dy : integer;
  cw : TWinHandle;
begin
  XTranslateCoordinates(display, wh, GfxRootWindow, x, y, @dx, @dy, @cw);

  Left := dx;
  Top  := dy;
  DoShow;

  if GfxMainForm = nil then
  begin
    Writeln('No popups available if mainform is not set.');
  end
  else
  begin
    XGrabPointer(display, GfxMainForm.WinHandle,
         true,
         ButtonPressMask or ButtonReleaseMask or ButtonMotionMask or PointerMotionMask,
         GrabModeAsync,
         GrabModeAsync,
         None,
         0,
         0 //fl_event_time
         );

    PopupListAdd(self);
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
{$else}
  if GfxFirstPopup = nil then XUngrabPointer(display, 0);
{$endif}
end;

procedure TPopupWindow.HandleMouseDown(x, y: integer; button, shiftstate: word);
begin
  inherited;
  //Writeln('mouse x=',x,' y=',y);
end;

initialization
begin
  GfxFirstPopup := nil;
  GfxLastPopup := nil;
end;

end.

