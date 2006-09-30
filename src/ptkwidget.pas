{ gfxwidget.pas: Widget base definition
  File maintainer: nvitya@freemail.hu
  
History:
  2003-05-15: Container type widgets focus handling corrected (Focus propagation)
}

unit ptkwidget;

{$include lptk_config.inc}

interface

uses
  Classes, SysUtils, lptk;

type
  TSearchDirection = (sdFirst, sdLast, sdNext, sdPrev);

  TptkWidget = class(TComponent)
  private
    FAnchors: TAnchors;
    FEnabled: boolean;
    procedure SetActiveWidget(const AValue: TptkWidget);
    procedure SetAnchors(const AValue: TAnchors);
    procedure SetEnabled(const AValue: boolean);

    procedure SetMouseCursor(cur : integer);
    
  protected
    FFormDesigner : TObject;

  protected
    FWinHandle : TptkWinHandle;
    FOnScreen : boolean;
    FParent    : TptkWidget;

    FFocusable : boolean;
    FFocused   : boolean;
    FTabOrder  : integer;

    FActiveWidget : TptkWidget;

    FTop, FLeft, FWidth, FHeight : TptkCoord;
    FBackgroundColor : TptkColor;

    FVisible   : boolean;
    
    FMouseCursor  : integer;

    FCanvas : TptkCanvas;

    FSizeParams : TSizeParams;

    FWPOverride : boolean;

    function GetWindowed : boolean;
    procedure SetVisible(const AValue : boolean);

    function GetWindowName : string; virtual;

    procedure AllocateWindow;
    procedure SetWindowParameters; virtual;  // for X
    procedure AdjustWindowStyle(var ws,es : longword; var pwh : TptkWinHandle); virtual; // for win32
    procedure ReleaseWindow;
    
    function GetCanvas : TptkCanvas;
    procedure FreeCanvas;

    procedure SetBackgroundColor(color : TptkColor);
    
    procedure DoShow; virtual;
    procedure DoHide; virtual;

    procedure DoKillFocus; virtual;
    procedure DoSetFocus; virtual;

    procedure MsgPaint(var msg : TptkMessageRec); message MSG_PAINT;

    procedure MsgKeyPress(var msg : TptkMessageRec); message MSG_KEYPRESS;
    procedure MsgKeyRelease(var msg : TptkMessageRec); message MSG_KEYRELEASE;
    
    procedure MsgMouseDown(var msg : TptkMessageRec); message MSG_MOUSEDOWN;
    procedure MsgMouseUp(var msg : TptkMessageRec); message MSG_MOUSEUP;
    procedure MsgMouseMove(var msg : TptkMessageRec); message MSG_MOUSEMOVE;
    procedure MsgDoubleClick(var msg : TptkMessageRec); message MSG_DOUBLECLICK;

    procedure MsgMouseEnter(var msg : TptkMessageRec); message MSG_MOUSEENTER;
    procedure MsgMouseExit(var msg : TptkMessageRec); message MSG_MOUSEEXIT;

    procedure MsgScroll(var msg : TptkMessageRec); message MSG_SCROLL;

    procedure MsgResize(var msg : TptkMessageRec); message MSG_RESIZE;

    procedure MsgMove(var msg : TptkMessageRec); message MSG_MOVE;


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

    procedure SetDimensions(x,y,w,h : TptkCoord);
    procedure MoveResizeBy(dx,dy,dw,dh : TptkCoord);
    procedure UpdateWindowPosition;

    procedure AfterCreate; virtual;

    function FindFocusWidget(startwg : TptkWidget; direction : TSearchDirection) : TptkWidget;

    function FindNextFocusWidget(startwg : TptkWidget; searchforward : boolean) : TptkWidget;

    procedure RePaint; virtual;
    procedure RePaintChildren;

    procedure ShowWidget;

    procedure SetFocus;
    procedure KillFocus;

    property Parent : TptkWidget read FParent write FParent;

    property Visible : boolean read FVisible write SetVisible;
    property Enabled : boolean read FEnabled write SetEnabled;
    property TabOrder  : integer read FTabOrder write FTabOrder;

    property Windowed : boolean read GetWindowed;  // true if window is allocated

    function Right : TptkCoord;
    function Bottom : TptkCoord;

    property SizeParams : TSizeParams read FSizeParams write FSizeParams;

    property BackgroundColor : TptkColor read FBackgroundColor write SetBackgroundColor;

    property Focusable : boolean read FFocusable write FFocusable;
    property Focused   : boolean read FFocused write FFocused;

    property ActiveWidget : TptkWidget read FActiveWidget write SetActiveWidget;

    property WinHandle : TptkWinHandle read FWinHandle;

    property Canvas : TptkCanvas read GetCanvas;

    property MouseCursor : integer read FMouseCursor write SetMouseCursor;

  public
    property FormDesigner : TObject read FFormDesigner write FFormDesigner;

  public

    OnKeyPress : TKeyPressNotifyEvent;

  published

    property Left : TptkCoord read FLeft write FLeft;
    property Top : TptkCoord read FTop write FTop;
    property Width : TptkCoord read FWidth write FWidth;
    property Height : TptkCoord read FHeight write FHeight;

    property Anchors : TAnchors read FAnchors write SetAnchors;
    
  end;

function FindWidget(wh : TptkWinHandle) : TptkWidget;

var
  FocusRoot : TptkWidget;

implementation

uses
  {$ifdef Win32}windows,{$else}X, Xlib, Xutil,{$endif}
  ptkstyle;

type
  PWidgetLookupRec = ^WidgetLookupRec;
  WidgetLookupRec = record
    wg   : TptkWidget;
    Next : PWidgetLookupRec;
  end;

var
  FirstWidgetLookupRec, LastWidgetLookupRec : PWidgetLookupRec;

procedure AddWidgetLookup(wg : TptkWidget);
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

procedure RemoveWidgetLookup(wg : TptkWidget);
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

function FindWidget(wh : TptkWinHandle) : TptkWidget;
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

{ TptkWidget }

procedure TptkWidget.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  if Windowed then RePaint;
end;

procedure TptkWidget.SetActiveWidget(const AValue: TptkWidget);
begin
  if FActiveWidget = AValue then exit;
  
  if FFormDesigner <> nil then Exit;
  
  if FActiveWidget <> nil then FActiveWidget.DoKillFocus;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then FActiveWidget.DoSetFocus;
end;

procedure TptkWidget.SetAnchors(const AValue: TAnchors);
begin
  if FAnchors=AValue then exit;
  FAnchors:=AValue;
end;

function TptkWidget.GetWindowed : boolean;
begin
  result := (WinHandle > 0);
end;

procedure TptkWidget.SetVisible(const AValue : boolean);
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

function TptkWidget.Right : TptkCoord;
begin
  result := FLeft + FWidth - 1;
end;

function TptkWidget.Bottom : TptkCoord;
begin
  result := FTop + FHeight - 1;
end;

constructor TptkWidget.Create(AOwner : TComponent);
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

  if (Owner <> nil) and (Owner is TptkWidget) then FParent := TptkWidget(Owner)
                                           else FParent := nil;
                                           
  ptkRegisterValidMsgDest(self);
  
  OnKeyPress := nil;

  AfterCreate;
end;

destructor TptkWidget.Destroy;
begin
  FreeCanvas;
  DoHide;
  ptkUnRegisterValidMsgDest(self);
  inherited Destroy;
end;

procedure TptkWidget.SetDimensions(x, y, w, h : TptkCoord);
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

procedure TptkWidget.MoveResizeBy(dx,dy,dw,dh : TptkCoord);
begin
  FLeft := FLeft + dx;
  FTop := FTop + dy;
  FWidth := FWidth + dw;
  FHeight := FHeight + dh;

  if FWinHandle > 0 then
    Canvas.MoveResizeWindow(FLeft,FTop,FWidth,FHeight);

  if (dw <> 0) or (dh <> 0) then HandleResize(dw,dh);
end;

procedure TptkWidget.UpdateWindowPosition;
begin
  if FWinHandle > 0 then
    Canvas.MoveResizeWindow(Left, Top, Width, Height);
end;

procedure TptkWidget.AfterCreate;
begin
  //
end;

function TptkWidget.FindFocusWidget(startwg : TptkWidget; direction : TSearchDirection) : TptkWidget;
var
  w : TptkWidget;
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
    if Components[n] is TptkWidget then
    begin
      w := TptkWidget(Components[n]);
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

function TptkWidget.FindNextFocusWidget(startwg: TptkWidget; searchforward : boolean): TptkWidget;
var
  wg : TptkWidget;
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

procedure TptkWidget.RePaint;
begin
  canvas.Clear(FBackgroundColor);
end;

procedure TptkWidget.RePaintChildren;
var
  n : integer;
begin
  for n:=0 to ComponentCount-1 do
    if (Components[n] is TptkWidget) and (TptkWidget(Components[n]).WinHandle > 0)
      then TptkWidget(Components[n]).RePaint;
end;

procedure TptkWidget.ShowWidget;
begin
  DoShow;
end;

procedure TptkWidget.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  wg : TptkWidget;
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

procedure TptkWidget.HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  //
end;

procedure TptkWidget.HandleMouseDown(x, y : integer; button : word; shiftstate : word);
var
  pw : TptkWidget;
  w : TptkWidget;
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

procedure TptkWidget.HandleMouseUp(x, y : integer; button : word; shiftstate : word);
begin
  //
end;

procedure TptkWidget.HandleMouseMove(x,y : integer; btnstate : word; shiftstate : word);
begin
  //
end;

procedure TptkWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: word);
begin
  //
end;

procedure TptkWidget.HandleWindowScroll(direction, amount: integer);
begin
  //
end;

procedure TptkWidget.HandleResize(dwidth, dheight: integer);
var
  n : integer;
  wg : TptkWidget;
  dx,dy,dw,dh : integer;
begin
  for n:=0 to ComponentCount-1 do
  begin
    if (Components[n] is TptkWidget) then
    begin
      wg := TptkWidget(Components[n]);

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

procedure TptkWidget.HandleMove(x, y: integer);
begin
  //
end;

procedure TptkWidget.HandleMouseEnter;
begin
  //
end;

procedure TptkWidget.HandleMouseExit;
begin
  //
end;

procedure TptkWidget.SetMouseCursor(cur: integer);
begin
  if FMouseCursor = cur then Exit;
  FMouseCursor := cur;
  ptkSetMouseCursor(FWinHandle, FMouseCursor);  // checks window handle
end;

procedure TptkWidget.AllocateWindow;
{$ifdef Win32}
var
  pwh : TptkWinHandle;
  wh  : TptkWinHandle;
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
  pwh : TptkWinHandle;
  wh  : TptkWinHandle;
  attr : TXSetWindowAttributes;
  mask : longword;
  bcolor : longword;
begin
  if WinHandle > 0 then Exit;

  if FParent <> nil then pwh := FParent.WinHandle else pwh := ptkRootWindow;

  bcolor := ptkColorToX(BackgroundColor);

  if FWPOverride then
  begin

    attr.Override_Redirect := longbool(1);
    attr.background_pixel := bColor;
    mask := CWOverrideRedirect; // or CWBackPixel;

    wh := XCreateWindow(Display, pwh,
          Left, Top, Width, Height, 0,
          CopyFromParent,
          InputOutput,
           ptkDefaultVisual,
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
           ptkDefaultVisual,
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

  ptkSetMouseCursor(wh, FMouseCursor);

end;

procedure TptkWidget.SetWindowParameters;
begin
  //
end;

procedure TptkWidget.AdjustWindowStyle(var ws,es : longword; var pwh : TptkWinHandle);  // for win32
begin
  //
end;

procedure TptkWidget.ReleaseWindow;
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

function TptkWidget.GetCanvas : TptkCanvas;
begin
  if FCanvas = nil then
  begin
    if FWinHandle > 0 then FCanvas := TptkCanvas.Create(FWinHandle);
  end;
  result := FCanvas;
end;

procedure TptkWidget.FreeCanvas;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TptkWidget.DefaultHandler(var msg);
begin
  //Writeln('DefaultHandler: Window ',FWinHandle,' msg: ',integer(msg));
//  inherited DefaultHandler(msg);
end;

procedure TptkWidget.SetBackgroundColor(color : TptkColor);
begin
  FBackgroundColor := color;
{$ifdef Win32}{$else}
  if FWinHandle > 0 then XSetWindowBackground(Display, FWinHandle, ptkColorToX(color));
{$endif}  
end;

procedure TptkWidget.DoShow;
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
      if (c is TptkWidget) and (TptkWidget(c).Parent = self) then TptkWidget(c).DoShow;
    end;
  end;
end;

procedure TptkWidget.DoHide;
var
  n : integer;
  c : TComponent;
begin
  for n := 0 to ComponentCount-1 do
  begin
    c := Components[n];
    if (c is TptkWidget) and (TptkWidget(c).Parent = self) then TptkWidget(c).DoHide;
  end;
  FOnScreen := False;
  ReleaseWindow;
end;

procedure TptkWidget.DoKillFocus;
begin
  //Writeln('DoKillFocus: ',self.ClassName);

  FFocused := false;
  if Windowed then RePaint;

  if ActiveWidget <> nil then ActiveWidget.KillFocus;

end;

procedure TptkWidget.DoSetFocus;
var
  awg : TptkWidget;
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

procedure TptkWidget.MsgPaint(var msg: TptkMessageRec);
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

procedure TptkWidget.MsgKeyPress(var msg: TptkMessageRec);
var
  key, ss : word;
  consumed : boolean;
  wg : TptkWidget;
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

procedure TptkWidget.MsgKeyRelease(var msg: TptkMessageRec);
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

procedure TptkWidget.MsgMouseDown(var msg : TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseDown(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TptkWidget.MsgMouseUp(var msg : TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseUp(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TptkWidget.MsgMouseMove(var msg: TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseMove(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TptkWidget.MsgDoubleClick(var msg: TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleDoubleClick(msg.Param1, msg.Param2, (msg.Param3 and $FF00) shr 8, msg.Param3 and $FF);
end;

procedure TptkWidget.MsgMouseEnter(var msg: TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseEnter;
end;

procedure TptkWidget.MsgMouseExit(var msg: TptkMessageRec);
begin
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
  HandleMouseExit;
end;

procedure TptkWidget.MsgScroll(var msg: TptkMessageRec);
begin
  HandleWindowScroll(msg.Param1, msg.Param2);
end;

procedure TptkWidget.MsgResize(var msg: TptkMessageRec);
begin
  FWidth  := FWidth + msg.Param1;
  FHeight := FHeight + msg.Param2;

  HandleResize(msg.Param1, msg.Param2);
  
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
  end;

end;

procedure TptkWidget.MsgMove(var msg: TptkMessageRec);
begin
  FLeft := msg.Param1;
  FTop  := msg.Param2;

  HandleMove(msg.Param1, msg.Param2);

  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
  end;
end;

function TptkWidget.GetWindowName: string;
begin
  result := '';
end;

procedure TptkWidget.KillFocus;
begin
  DoKillFocus;
end;

procedure TptkWidget.SetFocus;
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

