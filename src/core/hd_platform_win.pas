unit hd_platform_win;

// platform dependent workout for the pgfdefs
// only the very necessary code
// one big source file

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils,
  windows {$ifndef FPC},messages{$endif},
  lp_defs;

type
  TpgfWinHandle   = HWND;
  TpgfGContext    = HDC;

type
  TpgfWindowImpl = class;

  TpgfFontResourceImpl = class(TpgfFontResourceBase)
  private
    FFontData : HFONT;
    FMetrics : Windows.TEXTMETRIC;
  protected
    function OpenFontByDesc(const desc : string) : HFONT;

    property Handle : HFONT read FFontData;

  public
    constructor Create(const afontdesc : string);
    destructor Destroy; override;

    function HandleIsValid : boolean;

  public
    function GetAscent  : integer;
    function GetDescent : integer;
    function GetHeight  : integer;

    function GetTextWidth(const txt : widestring) : integer;
  end;

  TpgfFontImpl = class(TpgfFontBase)
  end;

  TpgfImageImpl = class(TpgfImageBase)
  private
    FBMPHandle : HBITMAP;
    FMaskHandle : HBITMAP;

    FIsTwoColor : boolean;             

    property BMPHandle : HBITMAP read FBMPHandle;
    property MaskHandle : HBITMAP read FMaskHandle;

  protected
    procedure DoFreeImage;

    procedure DoInitImage(acolordepth, awidth, aheight : integer; aimgdata : pointer);
    procedure DoInitImageMask(awidth, aheight : integer; aimgdata : pointer);

  public
    constructor Create;

  end;

  TpgfCanvasImpl = class(TpgfCanvasBase)
  private
    FDrawing : boolean;
    FBufferBitmap : HBitmap;
    FDrawWindow : TpgfWindowImpl;

    Fgc, FWinGC : TpgfGContext;
    FColorText : TpgfColor;
    FColor     : TpgfColor;
    FBackgroundColor : TpgfColor;
    FCurFontRes : TpgfFontResourceImpl;
    FClipRect  : TpgfRect;
    FClipRectSet : Boolean;
    FLineStyle : integer;
    FLineWidth : integer;

    FWindowsColor : longword;

    FBrush : HBRUSH;
    FPen   : HPEN;
    FClipRegion   : HRGN;

    FIntLineStyle, FIntLineWidth : integer;

  protected
    procedure DoSetFontRes(fntres : TpgfFontResourceImpl);
    procedure DoSetTextColor(cl : TpgfColor);
    procedure DoSetColor(cl : TpgfColor);
    procedure DoSetLineStyle(awidth: integer; astyle : TpgfLineStyle);

    procedure DoDrawString(x,y : TpgfCoord; const txt : widestring);

    procedure DoGetWinRect(var r : TpgfRect);

    procedure DoFillRectangle(x,y, w,h : TpgfCoord);
    procedure DoXORFillRectangle(col : TpgfColor; x, y, w, h : TpgfCoord);

    procedure DoFillTriangle(x1,y1, x2,y2, x3,y3 : TpgfCoord);

    procedure DoDrawRectangle(x,y, w,h : TpgfCoord);

    procedure DoDrawLine(x1,y1,x2,y2 : TpgfCoord);

    procedure DoDrawArc(x,y, w,h : TpgfCoord; a1, a2 : double);
    procedure DoFillArc(x,y, w,h : TpgfCoord; a1, a2 : double);

    procedure DoSetClipRect(const rect : TpgfRect);
    function DoGetClipRect : TpgfRect;
    procedure DoAddClipRect(const rect : TpgfRect);
    procedure DoClearClipRect;

    procedure DoDrawImagePart(x,y : TpgfCoord; img : TpgfImageImpl; xi,yi,w,h : integer);

    procedure DoBeginDraw(awin : TpgfWindowImpl; buffered : boolean);
    procedure DoPutBufferToScreen(x,y, w,h : TpgfCoord);
    procedure DoEndDraw;

  public
    constructor Create;
    destructor Destroy; override;
    function PaintTo(Dst: LongWord; X,Y, W, H: integer): boolean; virtual;
  end;

  TpgfWindowImpl = class(TpgfWindowBase)
  protected
    FWinHandle : TpgfWinHandle;
    FModalForWin : TpgfWindowImpl;

    FWinStyle, FWinStyleEx : longword;
    FParentWinHandle : TpgfWinHandle;

    //debug property WinHandle : TpgfWinHandle read FWinHandle;

  public
    property WinHandle : TpgfWinHandle read FWinHandle;
    function    WindowToScreen(ASource: TpgfWindowBase; const AScreenPos: TPoint): TPoint; override;
  protected
    procedure DoAllocateWindowHandle(aparent : TpgfWindowImpl);
    procedure DoReleaseWindowHandle;

    function HandleIsValid : boolean;

    procedure DoUpdateWindowPosition(aleft,atop,awidth,aheight : TpgfCoord);

    procedure DoMoveWindow(x,y : TpgfCoord);
    //procedure MoveToScreenCenter; override;

    procedure DoSetWindowTitle(const atitle : widestring);
    procedure DoSetCursor;override;

  public
    constructor Create(aowner : TComponent); override;
  end;

  { TpgfDisplayImpl }

  TpgfDisplayImpl = class(TpgfDisplayBase)
  protected
    FDisplay : HDC;

    WindowClass : TWndClass;
    WidgetClass : TWndClass;

    hcr_default : HCURSOR;
    hcr_dir_ew  : HCURSOR;
    hcr_dir_ns  : HCURSOR;
    hcr_edit    : HCURSOR;

    hcr_dir_nwse,
    hcr_dir_nesw,
    hcr_move,
    hcr_wait,
    hcr_hand,
    hcr_crosshair : HCURSOR;

    FFocusedWindow : THANDLE;

    // double click generation
    LastClickWindow  : TpgfWinHandle;
    LastWinClickTime : longword;

    FInitialized : boolean;

    FTimerWnd : HWND;

  public
    constructor Create(const aparams : string); override;
    destructor Destroy; override;

  public
    function DoMessagesPending : boolean;
    procedure DoWaitWindowMessage(atimeoutms : integer);

    procedure DoFlush;

    function GetScreenWidth : TpgfCoord;
    function GetScreenHeight : TpgfCoord;

    procedure GetScreenCoordinates(atwindow : TpgfWindowImpl; x,y : TpgfCoord; out sx, sy : TpgfCoord);
    procedure GrabPointer(awin : TpgfWindowImpl);
    procedure UnGrabPointer;

    property PlatformInitialized : boolean read FInitialized;

  public
    property Display : HDC read FDisplay;
  end;

implementation

uses lp_main, lp_widget, lp_form, lp_popup;

var
  wdisp : TpgfDisplay;

  MouseFocusedWH : HWND;

{$ifndef FPC}
type
  WndProc = TFNWndProc;
{$endif}

function pgfColorToWin(col : TpgfColor) : TpgfColor;
var
  c : dword;
begin
  c := pgfColorToRGB(col);

  //swapping bytes
  result := ((c and $FF0000) shr 16) or ((c and $0000FF) shl 16) or (c and $00FF00);
end;


function GetMyWidgetFromHandle(wh : TpgfWinHandle) : TpgfWidget;
begin
  if (wh <> 0) and (MainInstance = LongWord(GetWindowLong(wh, GWL_HINSTANCE))) then
  begin
    result := TpgfWidget(Windows.GetWindowLong(wh, GWL_USERDATA));
  end
  else result := nil;
end;

(*
procedure SendMouseMessage(wg : TWidget; msg : UINT; button : integer; wParam : WPARAM; lParam : LPARAM);
var
  p3 : integer;
  x,y : integer;
  wwg : TWidget;
  pwg : TWidget;
  h : THANDLE;
  pt : TPOINT;
begin
  x := SmallInt(lParam and $FFFF);
  y := SmallInt((lParam and $FFFF0000) shr 16);

  p3 := button shl 8;

  if (wParam and MK_CONTROL) <> 0 then p3 := p3 or ss_control;
  if (wParam and MK_SHIFT)   <> 0 then p3 := p3 or ss_shift;


  wwg := wg;

  if (PopupListFirst <> nil) then
  begin
    if wg = nil then Writeln('wg is NIL !!!');

    pt.x := x;
    pt.y := y;

    ClientToScreen(wg.WinHandle, pt);

    //Writeln('click x=',pt.X,' y=',pt.y);

    h := WindowFromPoint(pt);
    wwg := GetMyWidgetFromHandle(h);

    // if wwg <> nil then writeln('widget ok.');

    pwg := wwg;
    while (pwg <> nil) and (pwg.Parent <> nil) do pwg := pwg.Parent;

    if ((pwg = nil) or (PopupListFind(pwg.WinHandle) = nil)) and (not PopupDontCloseWidget(wwg)) and
       ((msg = MSG_MOUSEDOWN) or (msg = MSG_MOUSEUP)) then
    begin
      ClosePopups;

      SendMessage(nil, wwg, MSG_POPUPCLOSE, 0, 0, 0 );
    end;

    // sending the message...
    if wwg <> nil then
    begin
      ScreenToClient(wwg.WinHandle, pt);
      x := pt.x;
      y := pt.y;
    end;
  end;

  if ptkTopModalForm <> nil then
  begin
    pwg := WidgetParentForm(wwg);
    if (pwg <> nil) and (ptkTopModalForm <> pwg) then wwg := nil;
  end;

  if wwg <> nil then
  begin
    if (Msg = MSG_MOUSEDOWN) and (PopupListFirst = nil) then
    begin
      SetCapture(wwg.WinHandle);
    end
    else if (Msg = MSG_MOUSEUP) and (PopupListFirst = nil) then
    begin
      ReleaseCapture();
    end;

    SendMessage(nil, wwg, Msg, x, y, p3);

  end;

end;

*)

function pgfWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  w : TpgfWindowImpl;
  kwg, mwg, wwg, pwg : TpgfWidget;
  kcode,i : integer;
  sstate : TShiftState;
  h : THANDLE;
  p : PChar;
  pt : TPOINT;
  r  : TRECT;
  blockmsg : boolean;

  msgp : TpgfMessageParams;
  mcode : integer;

  wmsg: TMsg;

  PaintStruct: TPaintStruct;
begin
//  writeln('WND=',IntToHex(hwnd,8),' MSG=',IntToHex(uMsg,4),' wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));

  if uMsg = WM_CREATE then
  begin
    w := TpgfWindowImpl(PCreateStruct(lParam)^.lpCreateParams);

    w.FWinHandle := hwnd; // this is very important, because number of messages sent
                          // before the createwindow returns the window handle

    Windows.SetWindowLong(hwnd, GWL_USERDATA, LongWord(w));
  end;

  w := TpgfWindowImpl(Windows.GetWindowLong(hwnd, GWL_USERDATA));

  result := 0;

  if not Assigned(w) then
  begin
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    Exit;
  end;

  blockmsg := false;
  fillchar(msgp,sizeof(msgp),0);

  case uMsg of

    WM_CHAR,
    WM_KEYUP,
    WM_KEYDOWN:
    begin
      //Writeln('KeyMsg: ',umsg,' wp=',IntToHex(wParam,4),' lp=',IntToHex(lparam,8));

      kwg := FindKeyboardFocus;
      if kwg <> nil then w := kwg;

      sstate := [];
      if GetKeyState(VK_SHIFT) < 0 then sstate := sstate + [ssshift];
      if GetKeyState(VK_MENU) < 0 then sstate := sstate + [ssalt];
      if GetKeyState(VK_CONTROL) < 0 then sstate := sstate + [ssCtrl];

      kcode := (lParam shr 16) and $1FF;

      msgp.keyboard.keycode := kcode;
      msgp.keyboard.shiftstate := sstate;

      if uMsg = WM_KEYDOWN then
      begin
        //Writeln('PGFM_KEYPRESS: ',IntToHex(kcode,4),' (',kcode,')');

        pgfSendMessage(nil, w, PGFM_KEYPRESS, msgp);

        // generating WM_CHAR
        fillchar(wmsg,sizeof(wmsg),0);

        wmsg.hwnd := hwnd;
        wmsg.message := uMsg;
        wmsg.wParam := wParam;
        wmsg.lParam := lParam;

        Windows.TranslateMessage( {$ifdef FPC}@{$endif} wmsg);
        // TranslateMessage sends WM_CHAR ocassionally
        // but NOBODY KNOWS WHEN!

        // lets generate the PGFM_KEYCHAR for some special keys
        // based on this table of Windows virtual keys
        case wParam of
        $70..$7B,  // F1..F12
        $21..$24,  // home, end, pageup, pagedn
        $2D..$2E,  // insert, delete
        $25..$28:  // arrows
          begin
            msgp.keyboard.keycode := kcode or $FF00; // scan code + $FF00
            pgfSendMessage(nil, w, PGFM_KEYCHAR, msgp);
          end;
        end;
      end
      else if uMsg = WM_KEYUP then
      begin
        pgfSendMessage(nil, w, PGFM_KEYRELEASE, msgp);
      end
      else if uMsg = WM_CHAR then
      begin
        msgp.keyboard.keycode := wParam;
        pgfSendMessage(nil, w, PGFM_KEYCHAR, msgp);
      end;

    end;


    WM_SETCURSOR:
    begin
      //Writeln('Hittest: ',IntToHex((lParam and $FFFF),4));
      if (lParam and $FFFF) <= 1 then
      begin
        //ptkSetMouseCursor(wg.WinHandle, wg.MouseCursor);
        //result := 1;
        w.DoSetCursor;
      end
      else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    end;


    WM_MOUSEMOVE,
    WM_LBUTTONDOWN,
    WM_LBUTTONUP,
    WM_LBUTTONDBLCLK,
    WM_RBUTTONDOWN,
    WM_RBUTTONUP:
    begin
      msgp.mouse.x := SmallInt(lParam and $FFFF);
      msgp.mouse.y := SmallInt((lParam and $FFFF0000) shr 16);

      case uMsg of
        WM_MOUSEMOVE: mcode := PGFM_MOUSEMOVE;
        WM_LBUTTONDOWN, WM_RBUTTONDOWN: mcode := PGFM_MOUSEDOWN;
        WM_LBUTTONUP, WM_RBUTTONUP: mcode := PGFM_MOUSEUP;
        WM_LBUTTONDBLCLK: mcode := PGFM_DOUBLECLICK;
      else
        mcode := 0;
      end;

      if (PopupListFirst <> nil) and (mcode = PGFM_MOUSEDOWN) then
      begin
        pt.x := msgp.mouse.x;
        pt.y := msgp.mouse.y;

        ClientToScreen(w.WinHandle, pt);

        h := WindowFromPoint(pt);
        wwg := GetMyWidgetFromHandle(h);

        pwg := wwg;
        while (pwg <> nil) and (pwg.ParentWindow <> nil) do
          pwg := TpgfWidget(pwg.ParentWindow);

        if ((pwg = nil) or (not PopupListFind(pwg)))
          and (not PopupDontCloseWidget(wwg)) then
        begin
          ClosePopups;

          //pgfSendMessage(nil, wwg, MSG_POPUPCLOSE, msgp );
        end;
      end;

      case uMsg of
        WM_MOUSEMOVE:
          begin
            i := 0;
            if (wParam and MK_LBUTTON) <> 0 then i := i or MOUSE_LEFT;
            if (wParam and MK_RBUTTON) <> 0 then i := i or MOUSE_RIGHT;
            if (wParam and MK_MBUTTON) <> 0 then i := i or MOUSE_MIDDLE;
            msgp.mouse.buttons := i;
          end;
        WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK: msgp.mouse.buttons := MOUSE_LEFT;
        WM_RBUTTONDOWN, WM_RBUTTONUP: msgp.mouse.buttons := MOUSE_RIGHT;
      end;

      sstate := [];
      if (wParam and MK_CONTROL) <> 0 then sstate := sstate + [ssCtrl];
      if (wParam and MK_SHIFT)   <> 0 then sstate := sstate + [ssshift];
      msgp.mouse.shiftstate := sstate;

      if mcode <> 0 then pgfSendMessage(nil, w, mcode, msgp);
{
      if uMsg = WM_MOUSEMOVE then
      begin
        // OK! Windoze doesn't provide MOUSEENTER and MOUSEEXIT messages, so we
        // have to generate implicitly 'couse we need it for buttons

        GetCursorPos(PT);
        h := WindowFromPoint(PT);
        if h <> MouseFocusedWH then
        begin
          if MouseFocusedWH > 0 then
          begin
             mwg := GetMyWidgetFromHandle(MouseFocusedWH);
             if mwg <> nil then pgfSendMessage(nil, mwg, PGFM_MOUSEEXIT);
          end;

          mwg := GetMyWidgetFromHandle(h);
          if mwg <> nil then
          begin
            MouseFocusedWH := h;
            pgfSendMessage(nil, mwg, PGFM_MOUSEENTER);
          end
          else
          begin
            MouseFocusedWH := 0;
          end;
        end;
      end;
}
    end;

    WM_SIZE:
    begin
      // note that WM_SIZING allows some control on sizeing

      //writeln('WM_SIZE: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));

      msgp.rect.Width := smallint(lParam and $FFFF);
      msgp.rect.Height := smallint((lParam and $FFFF0000) shr 16);

      //writeln('WM_SIZE: width=',msgp.rect.width, ' height=',msgp.rect.height);

      // skip minimize...
      if lparam <> 0 then pgfSendMessage(nil, w, PGFM_RESIZE, msgp);
    end;


    WM_MOVE:
    begin
      // window decoration correction ...
      if (GetWindowLong(w.WinHandle, GWL_STYLE) and WS_CHILD) = 0 then
      begin
        GetWindowRect(w.WinHandle, r);
        msgp.rect.Left := r.Left;
        msgp.rect.top := r.Top;
      end
      else
      begin
        msgp.rect.Left := smallint(lParam and $FFFF);
        msgp.rect.Top  := smallint((lParam and $FFFF0000) shr 16);
      end;
      
      pgfSendMessage(nil, w, PGFM_MOVE, msgp);
    end;

(*
    WM_MOUSEWHEEL:
    begin
      //writeln('MWHEEL: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8)); // and $FF00) shr 8);

      pt.x := (lParam and $FFFF);
      pt.y := ((lParam and $FFFF0000) shr 16);

      h := WindowFromPoint(pt); //, CWP_SKIPINVISIBLE or CWP_SKIPDISABLED);
      if h > 0 then
      begin
        wg := TWidget(Windows.GetWindowLong(h, GWL_USERDATA));
      end;

      if wg <> nil then
      begin
        if int(wParam) < 0 then SendMessage(nil, wg, MSG_SCROLL, 1, 3, 0)
                           else SendMessage(nil, wg, MSG_SCROLL, 0, 3, 0);
      end;
    end;
*)
    WM_ACTIVATE:
    begin
      if ((wParam and $FFFF) = WA_INACTIVE) then pgfSendMessage(nil, w, PGFM_DEACTIVATE)
                                            else pgfSendMessage(nil, w, PGFM_ACTIVATE);
    end;

    WM_TIMER:
    begin
      //Writeln('TIMER EVENT!!!');
      // used for event wait timeout
      result := 0;
    end;

(*
    WM_NCACTIVATE:
    begin
      if (ptkTopModalForm <> nil) then
      begin
        if (wParam = 0) and (ptkTopModalForm = wg) then
        begin
          blockmsg := true;
        end
        else if (wParam <> 0) and (ptkTopModalForm <> wg) then
        begin
          blockmsg := true;
        end;
      end;

      if (PopupListFirst <> nil) and (PopupListFirst.Visible) then BlockMsg := True;

      //writeln('ncactivate: ', ord(BlockMsg));

      if not BlockMsg then
        Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);

    end;
*)

    WM_CLOSE:  pgfSendMessage(nil, w, PGFM_CLOSE, msgp);

    WM_PAINT:
      begin
        Windows.BeginPaint(w.WinHandle, {$ifdef FPC}@{$endif} PaintStruct);

        pgfSendMessage(nil, w, PGFM_PAINT, msgp);

        Windows.EndPaint(w.WinHandle, {$ifdef FPC}@{$endif} PaintStruct);
      end;

  else
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
end;

{ TpgfDisplayImpl }

constructor TpgfDisplayImpl.Create(const aparams: string);
begin
  // The lptk uses several writelines that we redirect to nul if {$APPTYPE GUI}

  FInitialized := false;

  {$I-}
  Writeln('PGF-Win32');
  if ioresult <> 0 then
  begin
    Assign(output,'nul');
    rewrite(output);
  end;
  {$I+}

  FDisplay := Windows.GetDC(0);

  //Writeln('Screen resolution: ',ScreenWidth,'x',ScreenHeight);

  with WindowClass do
  begin
    style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc := WndProc(@pgfWindowProc);
    hInstance := MainInstance;
    hIcon := LoadIcon(0, IDI_APPLICATION);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := 0; //COLOR_WINDOW;
    lpszClassName := 'PGFWIN';
  end;
  Windows.RegisterClass( {$ifdef FPC}@{$endif} WindowClass);

  with WidgetClass do
  begin
    style := CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc := WndProc(@pgfWindowProc);
    hInstance := MainInstance;
    hIcon := 0;
    hCursor := 0;
    hbrBackground := 0; //COLOR_BACKGROUND;
    lpszClassName := 'PGFWIDGET';
  end;
  Windows.RegisterClass( {$ifdef FPC}@{$endif} WidgetClass);

  hcr_default := LoadCursor(0, IDC_ARROW);
  hcr_dir_ew  := LoadCursor(0, IDC_SIZEWE);
  hcr_dir_ns  := LoadCursor(0, IDC_SIZENS);
  hcr_edit    := LoadCursor(0, IDC_IBEAM);

  hcr_dir_nwse := LoadCursor(0, IDC_SIZENWSE);
  hcr_DIR_NESW := LoadCursor(0, IDC_SIZENESW);

  hcr_MOVE     := LoadCursor(0, IDC_SIZEALL);

  hcr_CROSSHAIR := LoadCursor(0, IDC_CROSS);
  hcr_wait      := LoadCursor(0, IDC_WAIT);
  hcr_hand      := LoadCursor(0, IDC_HAND);
  

  FInitialized := True;

  wdisp := TpgfDisplay(self);
end;

destructor TpgfDisplayImpl.Destroy;
begin
  inherited Destroy;
end;

function TpgfDisplayImpl.DoMessagesPending: boolean;
var
  Msg: TMsg;
begin
  result := Windows.PeekMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0, PM_NOREMOVE);
end;

procedure TpgfDisplayImpl.DoWaitWindowMessage(atimeoutms : integer);
var
  Msg: TMsg;
  timerid  : longword;
  timerwnd : HWND;
  mp : boolean;
begin
  timerid := 0;
  timerwnd := 0;

  if (atimeoutms >= 0) and (not DoMessagesPending) then
  begin
    // handling waiting timeout
    if atimeoutms > 0 then
    begin
      timerwnd := pgfMainForm.WinHandle; 
      timerid := Windows.SetTimer(timerwnd, 1, atimeoutms, nil);
    end
    else
    begin
      Exit;
    end;
  end;

  // Some Win98 hack
  if (GetVersion() < $80000000)
    then Windows.GetMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0)   //NT
    else Windows.GetMessage( {$ifdef FPC}@{$endif} Msg, 0, 0, 0);   //Win98

  //  Writeln('Message: ',msg.message,' wp=',IntToHex(msg.wparam,4),
  //    ' lp=',intToHex(msg.lparam,8),
  //    ' px=',msg.pt.x, ' py=',msg.pt.y );

  Windows.DispatchMessage( {$ifdef FPC}@{$endif} msg);

  if timerid > 0 then
  begin
    Windows.KillTimer(timerwnd,1);
  end;

end;

procedure TpgfDisplayImpl.DoFlush;
begin
  GdiFlush;
end;

function TpgfDisplayImpl.GetScreenWidth: TpgfCoord;
var
  r : TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);

  result := r.Right - r.Left;
end;

function TpgfDisplayImpl.GetScreenHeight: TpgfCoord;
var
  r : TRECT;
begin
  GetWindowRect(GetDesktopWindow, r);

  result := r.Bottom - r.Top;
end;

procedure TpgfDisplayImpl.GetScreenCoordinates(atwindow : TpgfWindowImpl; x,y : TpgfCoord; out sx, sy : TpgfCoord);
var
  pt : TPoint;
begin
  pt.X := x;
  pt.Y := y;
  ClientToScreen(atwindow.WinHandle, pt);
  sx := pt.X;
  sy := pt.Y;
end;

procedure TpgfDisplayImpl.GrabPointer(awin : TpgfWindowImpl);
begin
  SetCapture(awin.WinHandle);
end;

procedure TpgfDisplayImpl.UnGrabPointer;
begin
  ReleaseCapture;
end;

{ TpgfWindowImpl }

procedure TpgfWindowImpl.DoAllocateWindowHandle(aparent : TpgfWindowImpl);
var
  wcname, wname : string;
  mid : dword;

  rwidth, rheight : integer;

  r : TRect;
begin
  if FWinHandle > 0 then Exit;

  FWinStyle := WS_OVERLAPPEDWINDOW;
  FWinStyleEx := WS_EX_APPWINDOW;
  mid := 0;
  wcname := 'PGFWIN';

  if aparent <> nil then
  begin
    FParentWinHandle := aparent.WinHandle;
  end
  else FParentWinHandle := 0;

  if FWindowType = wtChild then
  begin
    FWinStyle := WS_CHILD;
    FWinStyleEx := 0;
    mid := 1;
    wcname := 'PGFWIDGET';
  end
  else if FWindowType in [wtPopup] then
  begin
    {FWinStyle := WS_POPUP;
    FWinStyleEx := WS_EX_TOOLWINDOW;}
    // This prevents the popup window from stealing the focus. eg: ComboBox dropdown
    FParentWinHandle := GetDesktopWindow;
    FWinStyle   := WS_CHILD;
    FWinStyleEx := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  end;

  if FWindowType = wtModalForm then
  begin
    // for modal windows, this is necessary
    FWinStyle := WS_OVERLAPPEDWINDOW or WS_POPUPWINDOW;
    FWinStyle := FWinStyle and not (WS_MINIMIZEBOX);
    FWinStyleEx := 0;
  end;

  AdjustWindowStyle;

{
  if (pgfMainForm <> nil) and (FWinStyleEx = WS_EX_APPWINDOW ) then
  begin
    //FParentWinHandle := ptkMainForm.WinHandle;
    FWinStyle := WS_OVERLAPPEDWINDOW;
    FWinStyleEx := 0;
  end;
}

  if waAutoPos in FWindowAttributes then
  begin
    FLeft := TpgfCoord(CW_USEDEFAULT);
    FTop  := TpgfCoord(CW_USEDEFAULT);
  end;

  if (FWindowType <> wtChild) and not (waSizeable in FWindowAttributes)  then
  begin
    FWinStyle := FWinStyle and not (WS_SIZEBOX or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);
  end;

  FWinStyle := FWinStyle or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;

  wname := '';

  rwidth := FWidth;
  rheight := FHeight;

  if (FWinStyle and WS_CHILD) = 0 then
  begin
    r.Left := FLeft;
    r.Top  := FTop;
    r.Right := FLeft + FWidth;
    r.Bottom := FTop + FHeight;
    AdjustWindowRectEx(r, FWinStyle, false, FWinStyleEx);
    rwidth := r.Right - r.Left;
    rheight := r.Bottom - r.Top;
  end;

  FWinHandle := Windows.CreateWindowEx(
    FWinStyleEx,		// extended window style
    PChar(wcname),          	// registered class name
    PChar(wname),		// window name
    FWinStyle,			// window style
    FLeft,			// horizontal position of window
    FTop,			// vertical position of window
    rwidth,			// window width
    rheight,			// window height
    FParentWinHandle,		// handle to parent or owner window
    mid,			// menu handle or child identifier
    MainInstance,		// handle to application instance
    Self                        // window-creation data
    );

if not pgfDesigning then
begin
  if waScreenCenterPos in FWindowAttributes then
  begin
    FLeft := (wdisp.ScreenWidth-FWidth) div 2;
    FTop := (wdisp.ScreenHeight-FHeight) div 2;
    DoMoveWindow(FLeft,FTop);
  end;

  SetWindowParameters; // the forms require some adjustments before the Window appears

  BringWindowToTop(FWinHandle);

  if FWindowType in [wtPopup] then Windows.ShowWindow(FWinHandle, SW_SHOWNOACTIVATE)
                              else Windows.ShowWindow(FWinHandle, SW_SHOWNORMAL);

  if (waAutoPos in FWindowAttributes) or
     (waScreenCenterPos in FWindowAttributes) then
  begin
    // get the proper position
    GetWindowRect(FWinHandle, r);
    FLeft := r.Left;
    FTop := r.Top;
  end;


  Windows.UpdateWindow(FWinHandle);

// OS independent part:

//  ptkSetMouseCursor(FWinHandle, FMouseCursor);

end; //EOF not pgfDesigning

end;

procedure TpgfWindowImpl.DoReleaseWindowHandle;
begin
  if FWinHandle <= 0 then Exit;

  windows.DestroyWindow(FWinHandle);

  FWinHandle := 0;
end;

procedure TpgfWindowImpl.DoMoveWindow(x, y: TpgfCoord);
begin
  if (FWinHandle > 0) and not pgfDesigning then
    Windows.SetWindowPos(WinHandle,0,x,y,0,0,SWP_NOZORDER or SWP_NOSIZE or SWP_NOREDRAW);
end;

{
procedure TpgfWindowImpl.MoveToScreenCenter;
var
  r : TRECT;
begin
  GetWindowRect(WinHandle, r);
  FLeft := (wdisp.ScreenWidth-(r.Right - r.Left)) div 2;
  FTop := (wdisp.ScreenHeight-(r.Bottom - r.Top)) div 2;
  MoveWindow(FLeft,FTop);
end;
}

procedure TpgfWindowImpl.DoSetWindowTitle(const atitle: widestring);
var
  s8 : string;
begin
  if FWinHandle <= 0 then Exit;

  s8 := atitle;

  SetWindowText(FWinHandle, PChar(s8));
end;

constructor TpgfWindowImpl.Create(aowner: TComponent);
begin
  inherited;
  FWinHandle := 0;
end;

function TpgfWindowImpl.HandleIsValid: boolean;
begin
  result := FWinHandle > 0;
end;

procedure TpgfWindowImpl.DoUpdateWindowPosition(aleft, atop, awidth, aheight: TpgfCoord);
begin
  if pgfDesigning then
    exit;
    
  Windows.SetWindowPos(
    WinHandle,0,
    aleft,atop,awidth,aheight,
    SWP_NOZORDER or SWP_NOREDRAW
  );
end;

procedure TpgfWindowImpl.DoSetCursor;
var
  hc: HCURSOR;
begin
  if not HandleIsValid then
    Exit; //==>

 { crDefault     = TCursor(0);
    crNone        = TCursor(-1);
    crArrow       = TCursor(-2);
    crCross       = TCursor(-3);
    crIBeam       = TCursor(-4);
    crSize        = TCursor(-22);
    crSizeNESW    = TCursor(-6); // diagonal north east - south west
    crSizeNS      = TCursor(-7);
    crSizeNWSE    = TCursor(-8);
    crSizeWE      = TCursor(-9);
    crSizeNW      = TCursor(-23);
    crSizeN       = TCursor(-24);
    crSizeNE      = TCursor(-25);
    crSizeW       = TCursor(-26);
    crSizeE       = TCursor(-27);
    crSizeSW      = TCursor(-28);
    crSizeS       = TCursor(-29);
    crSizeSE      = TCursor(-30);
    crUpArrow     = TCursor(-10);
    crHourGlass   = TCursor(-11);
    crDrag        = TCursor(-12);
    crNoDrop      = TCursor(-13);
    crHSplit      = TCursor(-14);
    crVSplit      = TCursor(-15);
    crMultiDrag   = TCursor(-16);
    crSQLWait     = TCursor(-17);
    crNo          = TCursor(-18);
    crAppStart    = TCursor(-19);
    crHelp        = TCursor(-20);
    crHandPoint   = TCursor(-21);
    crSizeAll     = TCursor(-22);  }

  case FCursor of
    crSizeWE:     hc := wdisp.hcr_dir_ew;
    crSizeNS:     hc := wdisp.hcr_dir_ns;
    crIBeam:      hc := wdisp.hcr_edit;
    crSizeNWSE:   hc := wdisp.hcr_dir_nwse;
    crSizeNESW:   hc := wdisp.hcr_dir_nesw;
//    mcSizeSWNE:   hc := wdisp.hcr_dir_swne;
//    mcSizeSENW:   hc := wdisp.hcr_dir_senw;
    crSizeAll:       hc := wdisp.hcr_move;
    crCross:      hc := wdisp.hcr_crosshair;
    crHourGlass:  hc := wdisp.hcr_wait;
    crHandPoint:  hc := wdisp.hcr_hand;
  else
    hc := wdisp.hcr_default;
  end;

  Windows.SetCursor(hc);
end;

function TpgfWindowImpl.WindowToScreen(ASource: TpgfWindowBase;
  const AScreenPos: TPoint): TPoint;
begin
  if not TpgfWindowImpl(ASource).HandleIsValid then
    Exit; //==>

  Result.X := AScreenPos.X;
  Result.Y := AScreenPos.Y;
  ClientToScreen(TpgfWindowImpl(ASource).WinHandle, Result);
end;

{ TpgfCanvasImpl }

constructor TpgfCanvasImpl.Create;
begin
  FDrawing := false;
  FDrawWindow := nil;
  FBufferBitmap := 0;
end;

destructor TpgfCanvasImpl.Destroy;
begin
  if FDrawing then DoEndDraw;
  inherited;
end;

procedure TpgfCanvasImpl.DoBeginDraw(awin: TpgfWindowImpl; buffered : boolean);
var
  ARect : TpgfRect;
  bmsize : Windows.TSIZE;
begin
  if FDrawing and buffered and (FBufferBitmap > 0) then
  begin
    // check if the dimensions are ok
    GetBitmapDimensionEx(FBufferBitmap, bmsize);
    FDrawWindow := awin;
    DoGetWinRect(ARect);
    if (bmsize.cx <> ARect.width) or (bmsize.cy <> ARect.Height) then
    begin
      DoEndDraw;
    end;
  end;

  if not FDrawing then
  begin
    FDrawWindow := awin;

    FWinGC := windows.GetDC(FDrawWindow.FWinHandle);

    if buffered then
    begin
      DoGetWinRect(ARect);
      FBufferBitmap := windows.CreateCompatibleBitmap(FWinGC, ARect.Width, ARect.Height);
      Fgc := CreateCompatibleDC(FWinGC);
      SelectObject(Fgc, FBufferBitmap);
    end
    else
    begin
      FBufferBitmap := 0;
      Fgc := FWinGC;
    end;

    SetTextAlign(Fgc, TA_TOP); //TA_BASELINE);
    SetBkMode(Fgc, TRANSPARENT);
    
    FBrush := CreateSolidBrush(0);
    FPen := CreatePen(PS_SOLID, 0, 0);

    FClipRegion := CreateRectRgn(0,0,1,1);

    FColor := clText1;
    FLineStyle := PS_SOLID;
    FLineWidth := 0;
    FBackgroundColor := clBoxColor;
  end;
  
  FDrawing := true;
end;

procedure TpgfCanvasImpl.DoEndDraw;
begin
  if FDrawing then
  begin
    DeleteObject(FBrush);
    DeleteObject(FPen);
    DeleteObject(FClipRegion);

    if FBufferBitmap > 0 then DeleteObject(FBufferBitmap);
    FBufferBitmap := 0;

    if Fgc <> FWinGC then DeleteDC(Fgc);
    Fgc := 0;

    Windows.ReleaseDC(FDrawWindow.FWinHandle, FWingc);

    FDrawing := false;
    FDrawWindow := nil;
  end;
end;

procedure TpgfCanvasImpl.DoPutBufferToScreen(x, y, w, h: TpgfCoord);
begin
  if FBufferBitmap > 0 then BitBlt(FWinGC, x,y, w, h, Fgc, x, y, SRCCOPY);
end;

procedure TpgfCanvasImpl.DoAddClipRect(const rect: TpgfRect);
var
  rg : HRGN;
begin
  rg := CreateRectRgn(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height);
  FClipRect := Rect;
  FClipRectSet := True;
  CombineRgn(FClipRegion,rg,FClipRegion,RGN_AND);
  SelectClipRgn(Fgc, FClipRegion);
  DeleteObject(rg);
end;

procedure TpgfCanvasImpl.DoClearClipRect;
begin
  SelectClipRgn(Fgc, 0);
  FClipRectSet := False;
end;

procedure TpgfCanvasImpl.DoDrawLine(x1, y1, x2, y2: TpgfCoord);
var
  pts : array[1..2] of windows.TPoint;
begin
  pts[1].X := x1; pts[1].Y := y1;
  pts[2].X := x2; pts[2].Y := y2;
  PolyLine(Fgc, pts, 2);
  SetPixel(Fgc, x2,y2, FWindowsColor);
end;

procedure TpgfCanvasImpl.DoDrawRectangle(x, y, w, h: TpgfCoord);
var
  wr : windows.TRect;
begin
  wr.Left := x;
  wr.Top  := y;
  wr.Right := x + w;
  wr.Bottom := y + h;
  Windows.FrameRect(Fgc, wr, FBrush);
end;

procedure TpgfCanvasImpl.DoDrawString(x, y: TpgfCoord; const txt: widestring);
begin
  if length(txt) < 1 then exit;

  windows.TextOutW(Fgc, x,y{+FCurFont.Ascent}, @txt[1], length(txt));
end;

procedure TpgfCanvasImpl.DoFillRectangle(x, y, w, h: TpgfCoord);
var
  wr : windows.TRect;
begin
  wr.Left := x;
  wr.Top  := y;
  wr.Right := x + w;
  wr.Bottom := y + h;
  Windows.FillRect(Fgc, wr, FBrush);
end;

procedure TpgfCanvasImpl.DoFillTriangle(x1, y1, x2, y2, x3, y3: TpgfCoord);
var
  pts : array[1..3] of windows.TPoint;
begin
  pts[1].X := x1; pts[1].Y := y1;
  pts[2].X := x2; pts[2].Y := y2;
  pts[3].X := x3; pts[3].Y := y3;
  Polygon(Fgc, pts, 3);
end;

function TpgfCanvasImpl.DoGetClipRect: TpgfRect;
begin
  result := FClipRect;
end;

procedure TpgfCanvasImpl.DoGetWinRect(var r: TpgfRect);
var
  wr : windows.TRECT;
begin
  if assigned(FDrawWindow) and  FDrawWindow.HandleIsValid then
  begin
  GetClientRect(FDrawWindow.FWinHandle,wr);
  r.top := wr.Top;
  r.left := wr.Left;
  r.width := wr.Right - wr.Left + 1;
  r.height := wr.Bottom - wr.Top + 1;

  end;
end;

procedure TpgfCanvasImpl.DoSetClipRect(const rect: TpgfRect);
begin
  FClipRectSet := True;
  FClipRect := rect;
  DeleteObject(FClipRegion);
  FClipRegion := CreateRectRgn(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height);
  SelectClipRgn(Fgc, FClipRegion);
end;

procedure TpgfCanvasImpl.DoSetColor(cl: TpgfColor);
begin
  DeleteObject(FBrush);
  DeleteObject(FPen);

  FWindowsColor := pgfColorToWin(cl);

  FBrush := CreateSolidBrush(FWindowsColor);
  FPen := CreatePen(FintLineStyle, FintLineWidth, FWindowsColor);
  SelectObject(Fgc,FBrush);
  SelectObject(Fgc,FPen);
end;

procedure TpgfCanvasImpl.DoSetLineStyle(awidth: integer; astyle: TpgfLineStyle);
begin
  if astyle = lsDashed then FintLineStyle := PS_DASH else FintLineStyle := PS_SOLID;
  FintLineWidth := awidth;
  DeleteObject(FPen);
  FPen := CreatePen(FintLineStyle, FintLineWidth, FWindowsColor);
  SelectObject(Fgc,FPen);
end;  

procedure TpgfCanvasImpl.DoSetTextColor(cl: TpgfColor);
begin
  Windows.SetTextColor(Fgc, pgfColorToWin(cl));
end;

procedure TpgfCanvasImpl.DoSetFontRes(fntres: TpgfFontResourceImpl);
begin
  if fntres = nil then Exit;
  FCurFontRes := fntres;
  Windows.SelectObject(Fgc, FCurFontRes.Handle);
end;

procedure TpgfCanvasImpl.DoDrawImagePart(x, y: TpgfCoord;
             img: TpgfImageImpl; xi, yi, w, h: integer);
const
  DSTCOPY = $00AA0029;
  ROP_DSPDxax = $00E20746;
var
  tmpdc : HDC;
  rop : longword;
begin
  if img = nil then exit;

  tmpdc := CreateCompatibleDC(wdisp.display);

  SelectObject(tmpdc, img.BMPHandle);

  if img.FIsTwoColor then rop := PATCOPY  //ROP_DSPDxax
                     else rop := SRCCOPY;

  if img.MaskHandle > 0 then
  begin
    MaskBlt(Fgc, x,y, w, h, tmpdc, xi, yi, img.MaskHandle, xi, yi, MakeRop4(rop, DSTCOPY));
  end
  else BitBlt(Fgc, x,y, w, h, tmpdc, xi, yi, rop);

  DeleteDC(tmpdc);
end;

procedure TpgfCanvasImpl.DoXORFillRectangle(col: TpgfColor; x, y, w, h: TpgfCoord);
var
  hb : HBRUSH;
  nullpen : HPEN;
begin
  hb := CreateSolidBrush(pgfColorToWin(pgfColorToRGB(col)));
  nullpen := CreatePen(PS_NULL,0,0);

  SetROP2(Fgc, R2_XORPEN);
  SelectObject(Fgc, hb);
  SelectObject(Fgc, nullpen);

  Windows.Rectangle(Fgc, x,y,x + w + 1,y + h + 1);

  SetROP2(Fgc, R2_COPYPEN);
  DeleteObject(hb);
  SelectObject(Fgc,FPen);
end;

procedure TpgfCanvasImpl.DoDrawArc(x, y, w, h: TpgfCoord; a1, a2: double);
var
  xr, yr : double;
begin
  xr := w / 2;
  yr := h / 2;
  Arc(Fgc, x,y,x+w,y+h,
    trunc(0.5 + x + xr + cos(a1)*xr),
    trunc(0.5 + y + yr - sin(a1)*yr),

    trunc(0.5 + x + xr + cos(a1+a2)*xr),
    trunc(0.5 + y + yr - sin(a1+a2)*yr)
  );
end;

procedure TpgfCanvasImpl.DoFillArc(x, y, w, h: TpgfCoord; a1, a2: double);
var
  xr, yr : double;
begin
  xr := w / 2;
  yr := h / 2;
  Pie(Fgc, x,y,x+w,y+h,
    trunc(0.5 + x + xr + cos(a1)*xr),
    trunc(0.5 + y + yr - sin(a1)*yr),

    trunc(0.5 + x + xr + cos(a1+a2)*xr),
    trunc(0.5 + y + yr - sin(a1+a2)*yr)
  );
end;

function TpgfCanvasImpl.PaintTo(Dst: LongWord; X, Y, W, H: integer):boolean;
begin
  result := false;
  if Fgc > 0 then
    begin
      BitBlt(Dst, x,y, w, h, Fgc, 0, 0, SRCCOPY);
      result := true;
    end;
end;

{ TpgfFontResourceImpl }

constructor TpgfFontResourceImpl.Create(const afontdesc : string);
begin
  FFontData := OpenFontByDesc(afontdesc);

  if HandleIsValid then
  begin
    SelectObject(wdisp.display, FFontData);
    GetTextMetrics(wdisp.display, FMetrics);
  end;
end;

destructor TpgfFontResourceImpl.Destroy;
begin
  if HandleIsValid then Windows.DeleteObject(FFontData);
  inherited;
end;

function TpgfFontResourceImpl.OpenFontByDesc(const desc: string): HFONT;
var
  lf : Windows.LOGFONT;

  facename : string;

  cp : integer;
  c : char;

  token : string;
  prop, propval : string;

  function NextC : char;
  begin
    inc(cp);
    if cp > length(desc) then c := #0
                         else c := desc[cp];
    result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ','a'..'z','A'..'Z','_','0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
//  Writeln('ptkGetFont(''',desc,''')');

  FillChar(lf,sizeof(lf),0);

  with lf do
  begin
    lfWidth := 0; { have font mapper choose }
    lfEscapement := 0; { only straight fonts }
    lfOrientation := 0; { no rotation }
    lfWeight := FW_NORMAL;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := DEFAULT_CHARSET; //0; //Byte(Font.Charset);
    lfQuality := ANTIALIASED_QUALITY;
    { Everything else as default }
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfPitchAndFamily := DEFAULT_PITCH;
  end;

  cp := 0;
  NextC;

  NextToken;

//  Writeln('FaceName=',token);

  facename := token + #0;
  move(facename[1],lf.lfFaceName[0],length(facename));

  if c = '-' then
  begin
    NextC;
    NextToken;
    lf.lfHeight := -MulDiv(StrToIntDef(token,0), GetDeviceCaps(wdisp.display, LOGPIXELSY), 72);
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    if prop = 'BOLD' then
    begin
      lf.lfWeight := FW_BOLD;
      //Writeln('bold!');
    end
    else if prop = 'ITALIC' then
    begin
      lf.lfItalic := 1;
    end
    else if prop = 'ANTIALIAS' then
    begin
      if propval = 'FALSE' then lf.lfQuality := DEFAULT_QUALITY;
    end
    ;

  end;

  result := CreateFontIndirectA({$ifdef FPC}@{$endif}lf);
end;

function TpgfFontResourceImpl.HandleIsValid: boolean;
begin
  result := FFontData <> 0;
end;

function TpgfFontResourceImpl.GetAscent: integer;
begin
  result := FMetrics.tmAscent;
end;

function TpgfFontResourceImpl.GetDescent: integer;
begin
  result := FMetrics.tmDescent;
end;

function TpgfFontResourceImpl.GetHeight: integer;
begin
  result := FMetrics.tmHeight;
end;

function TpgfFontResourceImpl.GetTextWidth(const txt: widestring): integer;
var
  ts : Windows.SIZE;
begin
  if length(txt) < 1 then
  begin
    result := 0;
    exit;
  end;
  SelectObject(wdisp.display, FFontData);
  GetTextExtentPoint32W(wdisp.display, @txt[1], length(txt), ts);
  result := ts.cx;
end;

{ TpgfImageImpl }

constructor TpgfImageImpl.Create;
begin
  FBMPHandle := 0;
  FMaskHandle := 0;
  FIsTwoColor := false;
end;

procedure TpgfImageImpl.DoFreeImage;
begin
  if FBMPHandle > 0 then DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  if FMaskHandle > 0 then DeleteObject(FMaskHandle);
  FMaskHandle := 0;
end;

procedure TpgfImageImpl.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: pointer);
var
  bi : TBitmapInfo;
begin
  if FBMPHandle > 0 then DeleteObject(FBMPHandle);

  FBMPHandle := CreateCompatibleBitmap(wdisp.display, awidth, aheight);

  FillChar(bi, sizeof(bi), 0);

  with bi.bmiHeader do
  begin
    biSize  := sizeof(bi);
    biWidth  := awidth;
    biHeight := -aheight;
    biPlanes := 1;
    if acolordepth = 1 then bibitcount := 1
                       else bibitcount := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed := 0;
    biClrImportant := 0;
  end;

  SetDIBits(wdisp.display, FBMPHandle, 0, aheight, aimgdata, bi, DIB_RGB_COLORS);

  FIsTwoColor := (acolordepth = 1);
end;

type
  TMyMonoBitmap = packed record
    bmiHeader : TBitmapInfoHeader;
    bmColors : array[1..2] of longword;
  end;

procedure TpgfImageImpl.DoInitImageMask(awidth, aheight: integer; aimgdata: pointer);
var
  bi : TMyMonoBitmap;
  pbi : PBitmapInfo;
begin
  if FMaskHandle > 0 then DeleteObject(FMaskHandle);

  FMaskHandle := CreateBitmap(awidth, aheight, 1, 1, nil);

  FillChar(bi, sizeof(bi), 0);

  with bi.bmiHeader do
  begin
    biSize  := sizeof(bi.bmiHeader);
    biWidth  := awidth;
    biHeight := -aheight;
    biPlanes := 1;
    bibitcount := 1;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed := 2;
    biClrImportant := 0;
  end;
  bi.bmColors[1] := $000000;
  bi.bmColors[2] := $FFFFFF;

  pbi := @bi;
  SetDIBits(wdisp.display, FMaskHandle, 0, aheight, aimgdata, pbi^, DIB_RGB_COLORS);
end;

initialization
begin
  wdisp := nil;
  MouseFocusedWH := 0;
end;

end.

