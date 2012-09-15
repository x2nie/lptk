{ gfxwidget.pas: Widget base definition
  File maintainer: nvitya@freemail.hu

History:
  2003-05-15: Container type widgets focus handling corrected (Focus propagation)
}

unit gfxwidget;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, messagequeue, gfxbase;

type
  TSearchDirection = (sdFirst, sdLast, sdNext, sdPrev);

  TWidget = class(TComponent)
  private
    FAnchors: TAnchors;
    FEnabled: boolean;
    procedure SetActiveWidget(const AValue: TWidget);
    procedure SetAnchors(const AValue: TAnchors);
    procedure SetEnabled(const AValue: boolean);

    procedure SetMouseCursor(cur : integer);

  protected
    FFormDesigner : TObject;

  protected
    FWinHandle : TWinHandle;
    FOnScreen : boolean;
    FParent    : TWidget;

    FFocusable : boolean;
    FFocused   : boolean;
    FTabOrder  : integer;

    FActiveWidget : TWidget;

    FTop, FLeft, FWidth, FHeight : TGfxCoord;
    FBackgroundColor : TGfxColor;

    FVisible   : boolean;

    FMouseCursor  : integer;

    FCanvas : TGfxCanvas;

    FSizeParams : TSizeParams;

    FWPOverride : boolean;

    function GetWindowed : boolean;
    procedure SetVisible(const AValue : boolean);

    function GetWindowName : string; virtual;

    procedure AllocateWindow;
    procedure SetWindowParameters; virtual;  // for X
    procedure AdjustWindowStyle(var ws,es : longword; var pwh : TWinHandle); virtual; // for win32
    procedure ReleaseWindow;

    function GetCanvas : TGfxCanvas;
    procedure FreeCanvas;

    procedure SetBackgroundColor(color : TGfxColor);

    procedure DoShow; virtual;
    procedure DoHide; virtual;

    procedure DoKillFocus; virtual;
    procedure DoSetFocus; virtual;

    procedure MsgPaint(var msg : TMessageRec); message MSG_PAINT;

    procedure MsgKeyPress(var msg : TMessageRec); message MSG_KEYPRESS;
    procedure MsgKeyRelease(var msg : TMessageRec); message MSG_KEYRELEASE;

    procedure MsgMouseDown(var msg : TMessageRec); message MSG_MOUSEDOWN;
    procedure MsgMouseUp(var msg : TMessageRec); message MSG_MOUSEUP;
    procedure MsgMouseMove(var msg : TMessageRec); message MSG_MOUSEMOVE;
    procedure MsgDoubleClick(var msg : TMessageRec); message MSG_DOUBLECLICK;

    procedure MsgMouseEnter(var msg : TMessageRec); message MSG_MOUSEENTER;
    procedure MsgMouseExit(var msg : TMessageRec); message MSG_MOUSEEXIT;

    procedure MsgScroll(var msg : TMessageRec); message MSG_SCROLL;

    procedure MsgResize(var msg : TMessageRec); message MSG_RESIZE;

    procedure MsgMove(var msg : TMessageRec); message MSG_MOVE;


  protected

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); virtual;
    procedure HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed : boolean); virtual;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); virtual;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); virtual;
    procedure HandleMouseMove(x,y : integer; btnstate : word; shiftstate : word); virtual;
    procedure HandleDoubleClick(x,y : integer; button : word; shiftstate : word); virtual;

    procedure HandleWindowScroll(direction, amount : integer); virtual;

    procedure HandleResize(dwidth, dheight : integer); virtual;

    procedure HandleMove(x,y : integer); virtual;

    procedure HandleMouseEnter; virtual;
    procedure HandleMouseExit; virtual;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var msg); override;

    procedure SetDimensions(x,y,w,h : TGfxCoord);
    procedure MoveResizeBy(dx,dy,dw,dh : TGfxCoord);
    procedure UpdateWindowPosition;

    procedure AfterCreate; virtual;

    function FindFocusWidget(startwg : TWidget; direction : TSearchDirection) : TWidget;

    function FindNextFocusWidget(startwg : TWidget; searchforward : boolean) : TWidget;

    procedure RePaint; virtual;
    procedure RePaintChildren;

    procedure ShowWidget;

    procedure SetFocus;
    procedure KillFocus;

    property Parent : TWidget read FParent write FParent;

    property Visible : boolean read FVisible write SetVisible;
    property Enabled : boolean read FEnabled write SetEnabled;
    property TabOrder  : integer read FTabOrder write FTabOrder;

    property Windowed : boolean read GetWindowed;  // true if window is allocated

    function Right : TGfxCoord;
    function Bottom : TGfxCoord;

    property SizeParams : TSizeParams read FSizeParams write FSizeParams;

    property BackgroundColor : TGfxColor read FBackgroundColor write SetBackgroundColor;

    property Focusable : boolean read FFocusable write FFocusable;
    property Focused   : boolean read FFocused write FFocused;

    property ActiveWidget : TWidget read FActiveWidget write SetActiveWidget;

    property WinHandle : TWinHandle read FWinHandle;

    property Canvas : TGfxCanvas read GetCanvas;

    property MouseCursor : integer read FMouseCursor write SetMouseCursor;

  public
    property FormDesigner : TObject read FFormDesigner write FFormDesigner;

  public

    OnKeyPress : TKeyPressNotifyEvent;

  published

    property Left : TGfxCoord read FLeft write FLeft;
    property Top : TGfxCoord read FTop write FTop;
    property Width : TGfxCoord read FWidth write FWidth;
    property Height : TGfxCoord read FHeight write FHeight;

    property Anchors : TAnchors read FAnchors write SetAnchors;

  end;

function FindWidget(wh : TWinHandle) : TWidget;

var
  FocusRoot : TWidget;

implementation

uses
  {$ifdef Win32}windows,{$else}X, Xlib, Xutil,{$endif}
  gfxstyle;

type
  PWidgetLookupRec = ^WidgetLookupRec;
  WidgetLookupRec = record
    wg   : TWidget;
    Next : PWidgetLookupRec;
  end;

var
  FirstWidgetLookupRec, LastWidgetLookupRec : PWidgetLookupRec;

procedure AddWidgetLookup(wg : TWidget);
var
  p : PWidgetLookupRec;
begin
  if wg = nil then Exit;

  New(p);
  p^.wg := wg;
  p^.Next := nil;
  if FirstWidgetLookupRec = nil then FirstWidgetLookupRec := p
                                else LastWidgetLookupRec^.Next := p;
  LastWidgetLookupRec := p;
end;

procedure RemoveWidgetLookup(wg : TWidget);
var
  prevp, p, px : PWidgetLookupRec;
begin
  p := FirstWidgetLookupRec;
  prevp := nil;

  while p <> nil do
  begin
    if p^.wg = wg then
    begin
      if prevp = nil then FirstWidgetLookupRec := p^.Next
                     else prevp^.Next := p^.Next;
      if LastWidgetLookupRec = p then LastWidgetLookupRec := prevp;
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
end;

function FindWidget(wh : TWinHandle) : TWidget;
var
  p : PWidgetLookupRec;
begin
  p := FirstWidgetLookupRec;
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

{ TWidget }

procedure TWidget.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  if Windowed then RePaint;
end;

procedure TWidget.SetActiveWidget(const AValue: TWidget);
begin
  if FActiveWidget = AValue then exit;

  if FFormDesigner <> nil then Exit;

  if FActiveWidget <> nil then FActiveWidget.DoKillFocus;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then FActiveWidget.DoSetFocus;
end;

procedure TWidget.SetAnchors(const AValue: TAnchors);
begin
  if FAnchors=AValue then exit;
  FAnchors:=AValue;
end;

function TWidget.GetWindowed : boolean;
begin
  result := (WinHandle > 0);
end;

procedure TWidget.SetVisible(const AValue : boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if FOnScreen then
  begin
    if FVisible then DoShow
    else
    begin
      DoHide;
      FOnScreen := True;
    end;
  end;
end;

function TWidget.Right : TGfxCoord;
begin
  result := FLeft + FWidth - 1;
end;

function TWidget.Bottom : TGfxCoord;
begin
  result := FTop + FHeight - 1;
end;

constructor TWidget.Create(AOwner : TComponent);
begin
  inherited;
  FWinHandle := 0;
  FOnScreen := false;

  FFormDesigner := nil;

  FTop    := 0;
  FLeft   := 0;
  FWidth  := 16;
  FHeight := 16;

  FActiveWidget := nil;
  FVisible   := true;
  FEnabled   := true;
  FCanvas    := nil;

  FFocusable := false;
  FFocused   := false;
  FTabOrder  := 0;

  FBackgroundColor := clWindowBackground;

  FWPOverride := false;

  FSizeParams.min_width := 0;
  FSizeParams.max_width := 0;
  FSizeParams.min_height := 0;
  FSizeParams.max_height := 0;

  FAnchors := [anLeft,anTop];

  FMouseCursor := CUR_DEFAULT;

  if (Owner <> nil) and (Owner is TWidget) then FParent := TWidget(Owner)
                                           else FParent := nil;

  RegisterValidDest(self);

  OnKeyPress := nil;

  AfterCreate;
end;

destructor TWidget.Destroy;
begin
  FreeCanvas;
  DoHide;
  UnRegisterValidDest(self);
  inherited Destroy;
end;

procedure TWidget.SetDimensions(x, y, w, h : TGfxCoord);
var
  dw,dh : integer;
begin
  left := x;
  top := y;
  if w > 0 then
  begin
    dw := w - width;
    width := w;
  end else dw := 0;
  if h > 0 then
  begin
    dh := h - height;
    height := h;
  end else dh := 0;

  if FWinHandle > 0 then
  begin
    HandleResize(dw,dh);
    UpdateWindowPosition;
  end;
end;

procedure TWidget.MoveResizeBy(dx,dy,dw,dh : TGfxCoord);
begin
  FLeft := FLeft + dx;
  FTop := FTop + dy;
  FWidth := FWidth + dw;
  FHeight := FHeight + dh;

  if FWinHandle > 0 then
    Canvas.MoveResizeWindow(FLeft,FTop,FWidth,FHeight);

  if (dw <> 0) or (dh <> 0) then HandleResize(dw,dh);
end;

procedure TWidget.UpdateWindowPosition;
begin
  if FWinHandle > 0 then
    Canvas.MoveResizeWindow(Left, Top, Width, Height);
end;

procedure TWidget.AfterCreate;
begin
  //
end;

function TWidget.FindFocusWidget(startwg : TWidget; direction : TSearchDirection) : TWidget;
var
  w : TWidget;
  n : integer;
  FoundIt : boolean;
  lasttaborder : integer;
begin
  result := nil;
  FoundIt := false;
  if direction in [sdLast,sdPrev] then lasttaborder := -999999
                                  else lasttaborder := 999999;

  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TWidget then
    begin
      w := TWidget(Components[n]);
      if w.Visible and w.Enabled and w.Focusable then
      begin
        case direction of
          sdFirst:
            begin
              if w.TabOrder < lasttaborder then
              begin
                Result := w;
                lasttaborder := w.TabOrder;
              end;
            end;

          sdLast:
            begin
              if lasttaborder <= w.TabOrder then
              begin
                Result := w;
                lasttaborder := w.TabOrder;
              end;
            end;

          sdNext:
            begin
              if startwg = w then FoundIt := true
              else if w.TabOrder < lasttaborder then
              begin
                if (startwg = nil) or
                   (w.TabOrder > startwg.TabOrder) or
                   (FoundIt and (w.TabOrder = startwg.TabOrder)) then
                begin
                  result := w;
                  lasttaborder := w.TabOrder;
                end;
              end;
            end;

          sdPrev:
            begin
              if startwg = w then FoundIt := true
              else if w.TabOrder >= lasttaborder then
              begin
                if (startwg = nil) or
                   (w.TabOrder < startwg.TabOrder) or
                   (not FoundIt and (w.TabOrder = startwg.TabOrder)) then
                begin
                  result := w;
                  lasttaborder := w.TabOrder;
                end;
              end;

            end;

        end;
      end;
    end;
  end;
end;

function TWidget.FindNextFocusWidget(startwg: TWidget; searchforward : boolean): TWidget;
var
  wg : TWidget;
begin
  if searchforward
    then wg := FindFocusWidget(startwg,sdNext)
    else wg := FindFocusWidget(startwg,sdPrev);

  if wg = nil then
  begin
    if searchforward
      then wg := FindFocusWidget(startwg,sdFirst)
      else wg := FindFocusWidget(startwg,sdLast);
  end;
  result := wg;
end;

procedure TWidget.RePaint;
begin
  canvas.Clear(FBackgroundColor);
end;

procedure TWidget.RePaintChildren;
var
  n : integer;
begin
  for n:=0 to ComponentCount-1 do
    if (Components[n] is TWidget) and (TWidget(Components[n]).WinHandle > 0)
      then TWidget(Components[n]).RePaint;
end;

procedure TWidget.ShowWidget;
begin
  DoShow;
end;

procedure TWidget.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  wg : TWidget;
begin
  if Assigned(OnKeyPress) then OnKeyPress(self, keycode, shiftstate, consumed);

  if consumed then Exit;

  case keycode of
    KEY_ENTER, KEY_DOWN, KEY_RIGHT, KEY_TAB:
        begin
          // forward
          wg := FindFocusWidget(ActiveWidget,sdNext);
          ActiveWidget := wg;
          if wg <> nil then
          begin
            consumed := true;
          end
          else
          begin
            if Parent = nil then
            begin
              wg := FindFocusWidget(ActiveWidget,sdFirst);
              ActiveWidget := wg;
              consumed := true;
            end;
          end;
        end;

    KEY_UP, KEY_LEFT, KEY_STAB:
        begin
          // backward
          wg := FindFocusWidget(ActiveWidget,sdPrev);
          ActiveWidget := wg;
          if wg <> nil then
          begin
            consumed := true;
            // we must find the last one!
            while wg <> nil do
            begin
              wg.ActiveWidget := wg.FindFocusWidget(ActiveWidget,sdLast);
              wg := wg.ActiveWidget;
            end;
          end
          else
          begin
            if Parent = nil then
            begin
              wg := FindFocusWidget(ActiveWidget,sdLast);
              ActiveWidget := wg;
              consumed := true;
            end;
          end;

        end;
  end;
end;

procedure TWidget.HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  //
end;

procedure TWidget.HandleMouseDown(x, y : integer; button : word; shiftstate : word);
var
  pw : TWidget;
  w : TWidget;
begin
  pw := Parent;
  w := self;
  if (button <= 3) then
  begin
    while pw <> nil do
    begin
      if w.Visible and w.Enabled and w.Focusable then pw.ActiveWidget := w;
      w := pw;
      pw := pw.Parent;
    end;
  end;
end;

procedure TWidget.HandleMouseUp(x, y : integer; button : word; shiftstate : word);
begin
  //
end;

procedure TWidget.HandleMouseMove(x,y : integer; btnstate : word; shiftstate : word);
begin
  //
end;

procedure TWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: word);
begin
  //
end;

procedure TWidget.HandleWindowScroll(direction, amount: integer);
begin
  //
end;

procedure TWidget.HandleResize(dwidth, dheight: integer);
var
  n : integer;
  wg : TWidget;
  dx,dy,dw,dh : integer;
begin
  for n:=0 to ComponentCount-1 do
  begin
    if (Components[n] is TWidget) then
    begin
      wg := TWidget(Components[n]);

      if (anBottom in wg.Anchors) or (anRight in wg.Anchors) then
      begin
        // we must alter the window
        dx := 0; dy := 0; dw := 0; dh := 0;

        if (anLeft in wg.Anchors) and (anRight in wg.Anchors) then
        begin
          dw := dwidth;
        end else if anRight in wg.Anchors then
        begin
          dx := dwidth;
        end;

        if (anTop in wg.Anchors) and (anBottom in wg.Anchors) then
        begin
          dh := dheight;
        end else if anBottom in wg.Anchors then
        begin
          dy := dheight;
        end;

        wg.MoveResizeBy(dx,dy,dw,dh);

      end;
    end;
  end;

  if WinHandle > 0 then repaint;

end;

procedure TWidget.HandleMove(x, y: integer);
begin
  //
end;

procedure TWidget.HandleMouseEnter;
begin
  //
end;

procedure TWidget.HandleMouseExit;
begin
  //
end;

procedure TWidget.SetMouseCursor(cur: integer);
begin
  if FMouseCursor = cur then Exit;
  FMouseCursor := cur;
  GfxSetMouseCursor(FWinHandle, FMouseCursor);  // checks window handle
end;

procedure TWidget.AllocateWindow;
{$ifdef Win32}
var
  pwh : TWinHandle;
  wh  : TWinHandle;
  wcname, wname : string;
  ws, es, mid : dword;

  rwidth, rheight : integer;

  r : TRect;
begin
  if WinHandle > 0 then Exit;

  ws := WS_OVERLAPPEDWINDOW;
  es := WS_EX_APPWINDOW;
  mid := 0;
  wcname := 'LPTKWIN';

  if FParent <> nil then
  begin
    pwh := FParent.WinHandle;
    ws := WS_CHILD;
    es := 0;
    mid := 1;
    wcname := 'LPTKWIDGET';
  end
  else pwh := 0;

  if FWPOverride then
  begin
    ws := WS_POPUP;
    es := WS_EX_TOOLWINDOW;
  end;

  AdjustWindowStyle(ws, es, pwh);

  ws := ws or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;

  wname := GetWindowName;

  rwidth := FWidth;
  rheight := FHeight;

  if (ws and WS_CHILD) = 0 then
  begin
    r.Left := FLeft;
    r.Top  := FTop;
    r.Right := FLeft + FWidth;
    r.Bottom := FTop + FHeight;
    AdjustWindowRectEx(r, ws, false, es);
    rwidth := r.Right - r.Left;
    rheight := r.Bottom - r.Top;
  end;

  wh := Windows.CreateWindowEx(
    es,			// extended window style
    PChar(wcname),				// registered class name
    PChar(wname),			// window name
    ws,			// window style
    FLeft,			// horizontal position of window
    FTop,			// vertical position of window
    rwidth,			// window width
    rheight,			// window height
    pwh,			// handle to parent or owner window
    mid,					// menu handle or child identifier
    MainInstance,			// handle to application instance
    Self      // window-creation data
    );

  FWinHandle := wh;

  SetWindowParameters;

  BringWindowToTop(wh);

  if FWPOverride then Windows.ShowWindow(wh, SW_SHOWNOACTIVATE)
                 else Windows.ShowWindow(wh, SW_SHOWNORMAL);

  Windows.UpdateWindow(wh);

{$else}
var
  pwh : TWinHandle;
  wh  : TWinHandle;
  attr : TXSetWindowAttributes;
  mask : longword;
  bcolor : longword;
begin
  if WinHandle > 0 then Exit;

  if FParent <> nil then pwh := FParent.WinHandle else pwh := GfxRootWindow;

  bcolor := GfxColorToX(BackgroundColor);

  if FWPOverride then
  begin

    attr.Override_Redirect := longint(1);
    attr.background_pixel := bColor;
    mask := CWOverrideRedirect; // or CWBackPixel;

    wh := XCreateWindow(Display, pwh,
          Left, Top, Width, Height, 0,
          CopyFromParent,
          InputOutput,
           gfxDefaultVisual,
           mask,
           @attr);
  end
  else
  begin
    //attr.Override_Redirect := longbool(1);
    //attr.background_pixel := bColor;
    mask := 0; //CWOverrideRedirect; // or CWBackPixel;

    wh := XCreateWindow(Display, pwh,
          Left, Top, Width, Height, 0,
          CopyFromParent,
          InputOutput,
           gfxDefaultVisual,
           mask,
           @attr);
{
    wh := XCreateSimpleWindow(Display, pwh,
                                Left, Top, Width, Height, 0,
  			      0,   // border color !
  			      bColor );
}
    XMoveWindow(Display, wh, Left, Top);
  end;

  FWinHandle := wh;

  SetWindowParameters;

  XSelectInput(Display, wh, KeyPressMask or KeyReleaseMask or ButtonPressMask or
    ButtonReleaseMask or EnterWindowMask or LeaveWindowMask or
    PointerMotionMask or ExposureMask or FocusChangeMask or StructureNotifyMask);

  XMapWindow(Display, wh);

  AddWidgetLookup(self);
{$endif}

// OS independent part:

  GfxSetMouseCursor(wh, FMouseCursor);

end;

procedure TWidget.SetWindowParameters;
begin
  //
end;

procedure TWidget.AdjustWindowStyle(var ws,es : longword; var pwh : TWinHandle);  // for win32
begin
  //
end;

procedure TWidget.ReleaseWindow;
begin
  if FWinHandle <= 0 then Exit;

  FreeCanvas;

{$ifdef Win32}
  windows.DestroyWindow(FWinHandle);
{$else}
  RemoveWidgetLookup(self);
  XDestroyWindow(Display, FWinHandle);
{$endif}

  FWinHandle := 0;
end;

function TWidget.GetCanvas : TGfxCanvas;
begin
  if FCanvas = nil then
  begin
    if FWinHandle > 0 then FCanvas := TGfxCanvas.Create(FWinHandle);
  end;
  result := FCanvas;
end;

procedure TWidget.FreeCanvas;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TWidget.DefaultHandler(var msg);
begin
  //Writeln('DefaultHandler: Window ',FWinHandle,' msg: ',integer(msg));
//  inherited DefaultHandler(msg);
end;

procedure TWidget.SetBackgroundColor(color : TGfxColor);
begin
  FBackgroundColor := color;
{$ifdef Win32}{$else}
  if FWinHandle > 0 then XSetWindowBackground(Display, FWinHandle, GfxColorToX(color));
{$endif}
end;

procedure TWidget.DoShow;
var
  n : integer;
  c : TComponent;
begin
  FOnScreen := True;
  if FVisible then
  begin
    AllocateWindow;

    for n := 0 to ComponentCount-1 do
    begin
      c := Components[n];
      if (c is TWidget) and (TWidget(c).Parent = self) then TWidget(c).DoShow;
    end;
  end;
end;

procedure TWidget.DoHide;
var
  n : integer;
  c : TComponent;
begin
  for n := 0 to ComponentCount-1 do
  begin
    c := Components[n];
    if (c is TWidget) and (TWidget(c).Parent = self) then TWidget(c).DoHide;
  end;
  FOnScreen := False;
  ReleaseWindow;
end;

procedure TWidget.DoKillFocus;
begin
  //Writeln('DoKillFocus: ',self.ClassName);

  FFocused := false;
  if Windowed then RePaint;

  if ActiveWidget <> nil then ActiveWidget.KillFocus;

end;

procedure TWidget.DoSetFocus;
var
  awg : TWidget;
begin
  //Writeln('DoSetFocus: ',self.ClassName);

  if not FFocused then
  begin
    FFocused := true;

    if Windowed then RePaint;

    // focusing a child

    if ActiveWidget <> nil then
    begin
      ActiveWidget.SetFocus;
    end
    else
    begin
      // try to find it for the first time.
      awg := FindFocusWidget(nil, sdFirst);
      if awg <> nil then ActiveWidget := awg;
    end;
  end;

  if Parent <> nil then
  begin
    Parent.ActiveWidget := self;
    Parent.SetFocus;
  end;

end;

procedure TWidget.MsgPaint(var msg: TMessageRec);
{$ifdef Win32}
var
  PaintStruct: TPaintStruct;
//  dc : HDC;
begin
  Windows.BeginPaint(FWinHandle, {$ifdef FPC}@{$endif} PaintStruct);

  RePaint;

  Windows.EndPaint(FWinHandle, {$ifdef FPC}@{$endif} PaintStruct);
end;
{$else}
begin
  RePaint;
end;
{$endif}

procedure TWidget.MsgKeyPress(var msg: TMessageRec);
var
  key, ss : word;
  consumed : boolean;
  wg : TWidget;
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  key := msg.Param1 and $FFFF;
  ss := msg.Param2 and $FFFF;
  consumed := false;
  HandleKeyPress(key, ss, consumed);
  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TWidget.MsgKeyRelease(var msg: TMessageRec);
var
  key, ss : Word;
  consumed : boolean;
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  key := msg.Param1 and $FFFF;
  ss := msg.Param2 and $FFFF;
  consumed := false;
  HandleKeyRelease(key, ss, consumed);
  if not consumed then
  begin
    if Parent <> nil then Parent.HandleKeyRelease(key, ss, consumed);
  end;
end;

procedure TWidget.MsgMouseDown(var msg : TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseDown(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TWidget.MsgMouseUp(var msg : TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseUp(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TWidget.MsgMouseMove(var msg: TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseMove(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TWidget.MsgDoubleClick(var msg: TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleDoubleClick(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TWidget.MsgMouseEnter(var msg: TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseEnter;
end;

procedure TWidget.MsgMouseExit(var msg: TMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseExit;
end;

procedure TWidget.MsgScroll(var msg: TMessageRec);
begin
  HandleWindowScroll(msg.Param1, msg.Param2);
end;

procedure TWidget.MsgResize(var msg: TMessageRec);
begin
  FWidth  := FWidth + msg.Param1;
  FHeight := FHeight + msg.Param2;

  HandleResize(msg.Param1, msg.Param2);

  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
  end;

end;

procedure TWidget.MsgMove(var msg: TMessageRec);
begin
  FLeft := msg.Param1;
  FTop  := msg.Param2;

  HandleMove(msg.Param1, msg.Param2);

  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
  end;
end;

function TWidget.GetWindowName: string;
begin
  result := '';
end;

procedure TWidget.KillFocus;
begin
  DoKillFocus;
end;

procedure TWidget.SetFocus;
begin
  DoSetFocus;
end;

initialization
begin
  FirstWidgetLookupRec := nil;
  LastWidgetLookupRec := nil;

  FocusRoot := nil;
end;

end.

