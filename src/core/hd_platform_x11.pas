unit hd_platform_x11;

// platform dependent workout for the pgfdefs
// only the very necessary code
// one big source file

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils,
  X, Xlib, XUtil,
  x11_xft, x11_keyconv,
  pgf_defs;


type
  TpgfWinHandle   = TXID;
  TpgfGContext    = Xlib.TGc;

type
  PInt = ^Integer;

  TXIC = record
           dummy : pointer;
         end;
  PXIC = ^TXIC;

  TXIM = record
           dummy : pointer;
         end;
  PXIM = ^TXIM;

type
  TpgfWindowImpl = class;

  TpgfFontResourceImpl = class(TpgfFontResourceBase)
  private
    FFontData : PXftFont;
  protected
    property Handle : PXftFont read FFontData;

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

  TpgfImageImpl = class(TpgfImageBase)
  private
    FXimg  : TXImage;
    FXimgmask : TXImage;

    FMasked : boolean;

    function XImage : PXImage;
    function XImageMask : PXImage;

  protected
    procedure DoFreeImage;

    procedure DoInitImage(acolordepth, awidth, aheight : integer; aimgdata : pointer);
    procedure DoInitImageMask(awidth, aheight : integer; aimgdata : pointer);

  public
    constructor Create;

  end;

  { TpgfCanvasImpl }

  TpgfCanvasImpl = class(TpgfCanvasBase)
  private
    FDrawing : boolean;
    FDrawWindow : TpgfWindowImpl;
    FBufferPixmap : TPixmap;

    FDrawHandle : TXID;

    Fgc  : TpgfGContext;

    FColorText : TpgfColor;
    FColor     : TpgfColor;

    FCurFontRes : TpgfFontResourceImpl;

    FClipRect  : TpgfRect;
    FClipRectSet : Boolean;

    FLineStyle : integer;
    FLineWidth : integer;

    FXftDraw : PXftDraw;
    FXftDrawBuffer : PXftDraw;
    FColorTextXft : TXftColor;
    FClipRegion   : TRegion;

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
  end;

  { TpgfWindowImpl }

  TpgfWindowImpl = class(TpgfWindowBase)
  protected
    FWinHandle : TpgfWinHandle;
    FModalForWin : TpgfWindowImpl;
    property WinHandle : TpgfWinHandle read FWinHandle;

  protected
    procedure DoAllocateWindowHandle(aparent : TpgfWindowImpl);
    procedure DoReleaseWindowHandle;

    function HandleIsValid : boolean;

    procedure DoSetWindowTitle(const atitle : widestring);

    procedure DoMoveWindow(x,y : TpgfCoord);

    procedure DoUpdateWindowPosition(aleft,atop,awidth,aheight : TpgfCoord);

  public
    constructor Create(aowner : TComponent); override;
  end;

  { TpgfDisplayImpl }

  TpgfDisplayImpl = class(TpgfDisplayBase)
  protected
    FDisplay : PXDisplay;

    DisplayDepth : integer;

    DefaultBackground : TpgfColor;
    DefaultForeground : TpgfColor;

    DefaultScreen : integer;
    DefaultVisual : PVisual;
    DefaultColorMap : TColorMap;
    RootWindow : TpgfWinHandle;

    xia_clipboard        : TAtom;
    xia_motif_wm_hints   : TAtom;
    xia_wm_protocols     : TAtom;
    xia_wm_delete_window : TAtom;

    xia_wm_state       : TAtom;
    xia_wm_state_modal : TAtom;

    xia_targets        : TAtom;

    InputMethod  : PXIM;
    InputContext : PXIC;

    // double click generation
    LastClickWindow  : TpgfWinHandle;
    LastWinClickTime : longword;

    FInitialized : boolean;

  public
    constructor Create(const aparams : string); virtual;
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
    property Display : PXDisplay read FDisplay;
  end;

implementation

uses baseunix, pgf_main, pgf_widget, pgf_form, pgf_popup;

var
  xdisp : TpgfDisplay;

const
  MSG_KEYPRESS   = 2;
  MSG_KEYRELEASE = 3;
  MSG_PAINT      = 12;

  MSG_ACTIVATE   = 9;
  MSG_DEACTIVATE = 10;

  MSG_MOUSEDOWN  = 4;
  MSG_MOUSEUP    = 5;

  MSG_MOUSEMOVE  = 6;

  MSG_MOUSEENTER = 7;
  MSG_MOUSEEXIT  = 8;

  MSG_CLOSE      = 33;

const
  MSG_SCROLL     = 65;
  MSG_RESIZE     = 66;

  MSG_POPUPCLOSE = 67;

  MSG_MOVE       = 68;

  MSG_DOUBLECLICK = 69;


// some externals

// defines:
procedure XRenderSetPictureClipRectangles(disp : PXDisplay; pic : TPicture; xorigin,yorigin : integer; rect : PXRectangle; num : integer); cdecl; external;

// redefines:
function XmbLookupString(p1 : PXIC; ev : PXKeyPressedEvent; str : PChar; len : longword; ks:PKeySym; stat:PStatus):longint;cdecl; external;

function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):PXIM;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;


function ConvertTo565Pixel(rgb : longword) : word;
begin
  result := (rgb and $F8) shr 3;
  result := result or ((rgb and $FC00) shr 5);
  result := result or ((rgb and $F80000) shr 8);
end;

function pgfColorToX(col : TpgfColor) : longword;
var
  xc : TXColor;
  c : TpgfColor;
begin
  c := pgfColorToRGB(col);

  if xdisp.DisplayDepth >= 24 then
  begin
    result := c;
  end
  else if xdisp.DisplayDepth = 16 then
  begin
    result := ConvertTo565Pixel(c);
  end
  else
  begin
    c := col;
    xc.blue  := (c and $000000FF) shl 8;
    xc.green := (c and $0000FF00);
    xc.red   := (c and $00FF0000) shr 8;

    // THIS CALL IS TOO SLOW !!!!!:
    XAllocColor(xdisp.display, xdisp.DefaultColorMap, @xc);
    result := xc.pixel;
  end;
end;

procedure SetXftColor(col : TpgfColor; var colxft : TXftColor);
var
  c : TpgfColor;
begin
  c := pgfColorToRGB(col);

  colxft.color.blue  := (c and $000000FF) shl 8;
  colxft.color.green := (c and $0000FF00);
  colxft.color.red   := (c and $00FF0000) shr 8;

  colxft.color.alpha := (c and $7F000000) shr 15;
  colxft.color.alpha := colxft.color.alpha xor $FFFF;  // invert: 0 in ptkColor means not translucent

  colxft.pixel := 0;
end;

type
  PWindowLookupRec = ^WindowLookupRec;
  WindowLookupRec = record
    w  : TpgfWindowImpl;
    Next : PWindowLookupRec;
  end;

var
  FirstWindowLookupRec,
  LastWindowLookupRec   : PWindowLookupRec;

procedure AddWindowLookup(w : TpgfWindowImpl);
var
  p : PWindowLookupRec;
begin
  if w = nil then Exit;

  New(p);
  p^.w := w;
  p^.Next := nil;
  if FirstWindowLookupRec = nil then FirstWindowLookupRec := p
                                else LastWindowLookupRec^.Next := p;
  LastWindowLookupRec := p;
end;

procedure RemoveWindowLookup(w : TpgfWindowImpl);
var
  prevp, p, px : PWindowLookupRec;
begin
  p := FirstWindowLookupRec;
  prevp := nil;

  while p <> nil do
  begin
    if p^.w = w then
    begin
      if prevp = nil then FirstWindowLookupRec := p^.Next
                     else prevp^.Next := p^.Next;
      if LastWindowLookupRec = p then LastWindowLookupRec := prevp;
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

function FindWindowByHandle(wh : TpgfWinHandle) : TpgfWindowImpl;
var
  p : PWindowLookupRec;
begin
  p := FirstWindowLookupRec;
  while p <> nil do
  begin
    if p^.w.WinHandle = wh then
    begin
      Result := p^.w;
      Exit;
    end;
    p := p^.Next;
  end;
  result := nil;
end;

{ TpgfDisplayImpl }

constructor TpgfDisplayImpl.Create(const aparams: string);
var
  wa : TXWindowAttributes;
begin
  Finitialized := false;

  FDisplay := XOpenDisplay(PChar(aparams));

  if FDisplay = nil then Exit;

  DefaultScreen := XDefaultScreen(Display);
  RootWindow := XRootWindow(FDisplay, DefaultScreen);
  DefaultBackground := XBlackPixel(FDisplay, DefaultScreen);
  DefaultForeground := XWhitePixel(FDisplay, DefaultScreen);

  DefaultVisual := XDefaultVisual(FDisplay, DefaultScreen);
  DisplayDepth := XDefaultDepth(FDisplay, DefaultScreen);

  //Writeln('display depth: ',DisplayDepth);

  DefaultColorMap := XDefaultColorMap(FDisplay, DefaultScreen);

  // Initialize atoms
  xia_clipboard := XInternAtom(FDisplay, 'CLIPBOARD', longbool(0));
  xia_targets := XInternAtom(FDisplay, 'TARGETS', longbool(0));
  xia_motif_wm_hints := XInternAtom(FDisplay, '_MOTIF_WM_HINTS', longbool(0));
  xia_wm_protocols     := XInternAtom(FDisplay, 'WM_PROTOCOLS', longbool(0));
  xia_wm_delete_window := XInternAtom(FDisplay, 'WM_DELETE_WINDOW', longbool(0));

  xia_wm_state := XInternAtom(FDisplay, '_NET_WM_STATE', longbool(0));
  xia_wm_state_modal := XInternAtom(FDisplay, '_NET_WM_STATE_MODAL', longbool(0));

  //writeln('modal=',xia_wm_modal);

//  dpisX := (XDisplayWidth(display,DefaultScreen) * 254 + XDisplayWidthMM(display,DefaultScreen)*5)
//                  / (XDisplayWidthMM(display,DefaultScreen)*10);

//  dpisY := (XDisplayHeight(display,DefaultScreen) * 254 + XDisplayHeightMM(display,DefaultScreen)*5)
//                  / (XDisplayHeightMM(display,DefaultScreen)*10);


  // for correct keyboard handling
  InputMethod := XOpenIM(FDisplay,nil,nil,nil);
  if InputMethod = nil then Exit;

  InputContext := XCreateIC(InputMethod, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0 ] );
  //InputContext := XCreateIC(im, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, XNClientWindow, win, 0 ] );
  if InputContext = nil then Exit;
  //XGetICValues(ic, [XNFilterEvents, @lw, nil]);

  FInitialized := True;

  xdisp := TpgfDisplay(self);
end;

destructor TpgfDisplayImpl.Destroy;
begin
  XCloseDisplay(FDisplay);

  inherited Destroy;
end;

function TpgfDisplayImpl.DoMessagesPending: boolean;
begin
  result := (XPending(display) > 0);
end;

function GetParentWindow(wh : TpgfWinHandle; var pw, rw : TpgfWinHandle) : boolean;
var
  rootw,
  parentw : TpgfWinHandle;
  childs : ^TpgfWinHandle;
  cnum : longword;
begin
  childs := nil;
  if XQueryTree(xdisp.display, wh, @rootw, @parentw, @childs, @cnum) <> 0 then
  begin
    pw := parentw;
    rw := rootw;
    result := true;
  end
  else result := false;
  if childs <> nil then XFree(childs);
end;

function GetDecorationWindow(wh : TpgfWinHandle) : TpgfWinHandle;
var
  lpw, pw, rw : TpgfWinHandle;
  bok : boolean;
begin
  pw := wh;
  repeat
    lpw := pw;
    bok := GetParentWindow(lpw, pw, rw);
  until (not bok) or (pw = rw);
  if bok then result := lpw else result := 0;
end;

function X11keycodeToScanCode(akeycode : word) : word;
begin
  case akeycode and $ff of

  $09..$5B: result := akeycode - 8;

  $6C: result := $11C; // numpad enter
  $6D: result := $11D; // right ctrl
  $70: result := $135; // numpad /

  $62: result := $148;
  $64: result := $14B;
  $66: result := $14D;
  $68: result := $150;

  $6A: result := $152;
  $61: result := $147;
  $63: result := $149;

  $6B: result := $153;
  $67: result := $14F;
  $69: result := $151;

  $71: result := $138;

  else
    result := akeycode;
  end;
end;

procedure TpgfDisplayImpl.DoWaitWindowMessage(atimeoutms : integer);
var
  ev : TXEvent;
  n,i,r,i2 : integer;
  ks : integer;
  uc : word;
  a : array[1..16] of char;
  ss, sr : integer;
  p : PChar;
  blockmsg : boolean;
  b : boolean;

  w, ew : TpgfWindowImpl;
  wg, ewg : TpgfWidget;
  Popup : TpgfWidget;
//  frm : TpgfForm;

  kwg : TpgfWidget;

  wh : TpgfWinHandle;
  wa : TXWindowAttributes;
  px,py : integer;

  mcode : integer;
  msgp : TpgfMessageParams;

  rfds : TFDSet;
  xfd  : integer;
begin
  xfd := XConnectionNumber(display);

  repeat
    if (atimeoutms >= 0) and (XPending(display) <= 0) then
    begin
      // waiting some event for the given timeout

      // this Select handles only the first 256 file descriptors
      // poll would be better but FPC has no official poll interface (if I'm right)

      //Write('W',atimeoutms);
      if atimeoutms > 0 then
      begin
        fpFD_ZERO(rfds);
        fpFD_SET(xfd, rfds);
        r := fpSelect(xfd+1, @rfds, nil, nil, atimeoutms);
      end
      else r := 0;

      if r <= 0 then
      begin
        // no event received.
        //write('.');
        Exit;
      end;

      //write(r);
    end;

    XNextEvent(display, @ev);

  until (not XFilterEvent(@ev,0));

  blockmsg := false;

  fillchar(msgp,sizeof(msgp),0);

  Popup := PopupListFirst;

  //WriteLn('Event ',n,': ', ev._type,' window: ', ev.xany.window);

  case ev._type of

    MSG_KEYPRESS, MSG_KEYRELEASE:
    begin
      msgp.keyboard.keycode := X11keycodeToScanCode(ev.xkey.keycode);
      msgp.keyboard.shiftstate := ev.xkey.state;

      kwg := FindKeyboardFocus;
      if kwg <> nil then w := kwg
                    else w := FindWindowByHandle(ev.xkey.window);

      //Writeln('XKey event(',ev._type,'):',
      //  IntToHex(ev.xkey.keycode,4),' (',ev.xkey.keycode,'), shift=',IntToHex(ev.xkey.state,4));

      if ev._type = MSG_KEYPRESS then
      begin
        pgfPostMessage(nil, w, PGFM_KEYPRESS, msgp);

        //Writeln('scancode: ',IntToHex(X11keycodeToScanCode(ev.xkey.keycode),4)
        //  ,' (',X11keycodeToScanCode(ev.xkey.keycode),')');

        // force some function keys to send as keychar too

        uc := msgp.keyboard.keycode;

        b := true;
        case uc of
        $01:          uc := $001B; // esc
        $0E:          uc := $0008; // backspace
        $1C, $11C:    uc := $000D; // enter
        $0F:          uc := $0009; // tab

        $3B..$44,
        $57,$58,      // F1 .. F12

        $147..$149,   // nav keys
        $14B,$14D,
        $14F..$153:
                      uc := uc or $FF00;
        else
          b := false;
        end;

        if b then
        begin
          msgp.keyboard.keycode := uc;
          //Writeln('keychar: ',IntToHex(uc,4),' (',uc,')');
          pgfPostMessage(nil, w, PGFM_KEYCHAR, msgp);
        end
        else
        begin
          // try to convert it to some char
          sr := 0;
          r := XmbLookupString(InputContext, PXKeyPressedEvent(@ev), @a, 16, @ks, @sr);

          uc := ks and $FFFF;

          KeySymToUnicode(ks, @uc);

          msgp.keyboard.keycode := uc;

          //Writeln('keychar: ',IntToHex(uc,4),' (',uc,')');

          pgfPostMessage(nil, w, PGFM_KEYCHAR, msgp);
        end;

      end
      else if ev._type = MSG_KEYRELEASE then
      begin
        pgfPostMessage(nil, w, PGFM_KEYRELEASE, msgp);
      end;
    end;


    MSG_MOUSEDOWN, MSG_MOUSEUP:
    begin
      msgp.mouse.x := ev.xbutton.x;
      msgp.mouse.y := ev.xbutton.y;
      msgp.mouse.buttons := ev.xbutton.button;
      msgp.mouse.shiftstate := ev.xbutton.state;

      if Popup <> nil then
      begin
        wg := TpgfWidget(FindWindowByHandle(ev.xbutton.window));
        ewg := wg;
        while (wg <> nil) and (wg.ParentWindow <> nil) do wg := TpgfWidget(wg.ParentWindow);

        if (wg <> nil) and not PopupListFind(wg) and (not PopupDontCloseWidget(ewg)) then
        begin
          ClosePopups;
          Popup := nil;
          pgfPostMessage(nil, ewg, MSG_POPUPCLOSE, msgp );
          //blockmsg := true;
        end;
      end;

      w := FindWindowByHandle(ev.xbutton.window);

{
      if pgfTopModalForm <> nil then
      begin
        wg := WidgetParentForm(ewg);
        if (wg <> nil) and (pgfTopModalForm <> wg) then blockmsg := true;
      end;
}
      if not blockmsg then
      begin

        if (ev.xbutton.button >= 4) and (ev.xbutton.button <= 7) then  // mouse wheel
        begin
          // generate scroll events:
          if ev._type = MSG_MOUSEDOWN then
          begin
            if ev.xbutton.button > 5 then i := 1 else i := 3;  // amount

            //pgfPostMessage(nil, ewg, MSG_SCROLL, ev.xbutton.button mod 4, i, ev.xbutton.state );
          end;
        end
        else
        begin
          if ev._type = MSG_MOUSEUP then mcode := PGFM_MOUSEUP
                                    else mcode := PGFM_MOUSEDOWN;

          pgfPostMessage(nil, w, mcode, msgp);
{
          // doubleclick check
          if (ev.xbutton.button = 1) then
          begin
            if (ev._type = MSG_MOUSEDOWN) then
            begin
              if (ev.xbutton.window = LastClickWindow) and ((ev.xbutton.time - LastWinClickTime) < DOUBLECLICK_MS) then
              begin
                //Writeln('doubleclick');
                pgfPostMessage(nil, ewg, MSG_DOUBLECLICK, ev.xbutton.x, ev.xbutton.y,
                   (ev.xbutton.state and $FF) or ((ev.xbutton.button and $FF) shl 8) );

              end;
              //Writeln('button time: ',ev.xbutton.time);

              LastWinClickTime := ev.xbutton.time;
              LastClickWindow  := ev.xbutton.window;
            end;
          end;
}

        end;

      end;
    end;

    MSG_PAINT:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xany.window, MSG_PAINT, @ev);

      pgfPostMessage(nil, FindWindowByHandle(ev.xany.window), PGFM_PAINT);
    end;


    MSG_MOUSEMOVE:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xbutton.window, MSG_MOUSEMOVE, @ev);

{
      if pgfTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWidget(ev.xbutton.window));
        if (wg <> nil) and (pgfTopModalForm <> wg) then blockmsg := true;
      end;
}
//      Writeln('Motion: x=',ev.xmotion.x,' y=',ev.xmotion.y,'  st=',ev.xmotion.state);

      if not blockmsg then
      begin
        msgp.mouse.x := ev.xmotion.x;
        msgp.mouse.y := ev.xmotion.y;
        msgp.mouse.buttons := (ev.xmotion.state and $FF00) shr 8;
        msgp.mouse.shiftstate := ev.xmotion.state and $FF;
        pgfPostMessage(nil, FindWindowByHandle(ev.xbutton.window), PGFM_MOUSEMOVE, msgp);
      end;

    end;

    // message blockings for modal windows
    MSG_CLOSE:
    begin
      {
      if pgfTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWindowByHandle(ev.xbutton.window));
        if (wg <> nil) and (pgfTopModalForm <> wg) then blockmsg := true;
      end;
      }

      if not blockmsg then pgfPostMessage(nil, FindWindowByHandle(ev.xany.window), PGFM_CLOSE);

    end;



    ConfigureNotify:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xany.window, ConfigureNotify, @ev);

      msgp.rect.Left := ev.xconfigure.x;
      msgp.rect.Top  := ev.xconfigure.y;
      msgp.rect.Width  := ev.xconfigure.width;
      msgp.rect.Height := ev.xconfigure.height;

      w := FindWindowByHandle(ev.xconfigure.window);

      if w <> nil then
      begin
        if w.FWindowType <> wtChild then
        begin
          wh := GetDecorationWindow(ev.xconfigure.window);
          if wh > 0 then
          begin
            XGetWindowAttributes(display, wh, @wa);
            msgp.rect.Left := wa.x;
            msgp.rect.Top  := wa.y;
          end;
        end;

        if (w.FWidth <> msgp.rect.width) or (w.FHeight <> msgp.rect.height) then
        begin
          pgfPostMessage(nil, w, PGFM_RESIZE, msgp);
        end;

        if (w.FLeft <> msgp.rect.Left) or (w.FTop <> msgp.rect.Top) then
        begin
          pgfPostMessage(nil, w, PGFM_MOVE, msgp);
        end;
      end;
    end;


{
    SelectionNotify:
    begin
      ProcessSelection(ev);
    end;

    SelectionRequest:
    begin
      ProcessSelectionRequest(ev);
    end;
}

  MSG_ACTIVATE: pgfPostMessage(nil, FindWindowByHandle(ev.xany.window), PGFM_ACTIVATE);

  MSG_DEACTIVATE: pgfPostMessage(nil, FindWindowByHandle(ev.xany.window), PGFM_DEACTIVATE);

  else
    //pgfPostMessage(nil, FindWindowByHandle(ev.xany.window), ev._type, msgp);
  end;

end;

procedure TpgfDisplayImpl.DoFlush;
begin
  XFlush(FDisplay);
end;

function TpgfDisplayImpl.GetScreenWidth: TpgfCoord;
var
  wa : TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, RootWindow, @wa);
  result := wa.width;
end;

function TpgfDisplayImpl.GetScreenHeight: TpgfCoord;
var
  wa : TXWindowAttributes;
begin
  XGetWindowAttributes(FDisplay, RootWindow, @wa);
  result := wa.height;
end;

procedure TpgfDisplayImpl.GetScreenCoordinates(atwindow : TpgfWindowImpl; x,y : TpgfCoord; out sx, sy : TpgfCoord);
var
  dx,dy : integer;
  cw : TpgfWinHandle;
begin
  XTranslateCoordinates(xdisp.display, atwindow.WinHandle, xdisp.RootWindow, x, y, @sx, @sy, @cw);
end;

procedure TpgfDisplayImpl.GrabPointer(awin : TpgfWindowImpl);
begin
  XGrabPointer(display, pgfMainForm.WinHandle,
       true,
       ButtonPressMask or ButtonReleaseMask or ButtonMotionMask or PointerMotionMask,
       GrabModeAsync,
       GrabModeAsync,
       None,
       0,
       0 //fl_event_time
       );
end;

procedure TpgfDisplayImpl.UnGrabPointer;
begin
  XUngrabPointer(xdisp.display, 0);
end;

{ TpgfWindowImpl }

procedure TpgfWindowImpl.DoAllocateWindowHandle(aparent : TpgfWindowImpl);
var
  pwh : TpgfWinHandle;
  wh  : TpgfWinHandle;
  attr : TXSetWindowAttributes;
  mask : longword;
  bcolor : longword;
  hints : TXSizeHints;
begin
  if FWinHandle > 0 then Exit;

  if aparent <> nil then pwh := aparent.WinHandle else pwh := xdisp.RootWindow;

  FillChar(attr,sizeof(attr),0);
  mask := 0;
  if FWindowType in [wtPopup] then
  begin
    attr.Override_Redirect := longint(1); //* pbm gave it what it expected :-) was: longbool(1);
    mask := CWOverrideRedirect;
  end;

  AdjustWindowStyle;

  wh := XCreateWindow(xdisp.Display, pwh,
        FLeft, FTop, FWidth, FHeight, 0,
        CopyFromParent,
        InputOutput,
         xdisp.DefaultVisual,
         mask,
         @attr);

//    XMoveWindow(Display, wh, Left, Top);

  FWinHandle := wh;

  hints.flags := 0;

  if not (waAutoPos in FWindowAttributes) then
  begin
    hints.flags := hints.flags or PPosition;
  end;

  if waScreenCenterPos in FWindowAttributes then
  begin
    hints.flags := hints.flags or PPosition;

    FLeft := (xdisp.ScreenWidth-FWidth) div 2;
    FTop := (xdisp.ScreenHeight-FHeight) div 2;
    DoMoveWindow(FLeft,FTop);
  end;

  if (FWindowType <> wtChild) and (waSizeable in FWindowAttributes)  then
  begin
    hints.flags := hints.flags or PMinSize;
    hints.min_width  := FMinWidth;
    hints.min_height  := FMinHeight;

{

    if (FSizeParams.max_Width > 0) or (FSizeParams.max_height > 0) then
    begin
      hints.flags := hints.flags or PMaxSize;
      hints.max_width  := FSizeParams.max_width;
      hints.max_height  := FSizeParams.max_height;
    end;
}
  end
  else
  begin
    hints.flags := hints.flags or PMinSize or PMaxSize;
    hints.min_width  := FWidth;
    hints.min_height  := FHeight;
    hints.max_width  := FWidth;
    hints.max_height  := FHeight;
  end;

  XSetWMNormalHints(xdisp.display, FWinHandle, @hints);

  if FWindowType <> wtChild then
  begin
    // send close event instead of quitting the whole application...
    XSetWMProtocols(xdisp.Display, FWinHandle, @(xdisp.xia_wm_delete_window), 1);
  end;

  // for modal windows, this is necessary
  if (FWindowType = wtModalForm) and (aparent <> nil) then
  begin
    XSetTransientForHint(xdisp.display, self.FWinHandle, aparent.WinHandle);
  end;

  XSelectInput(xdisp.Display, wh, KeyPressMask or KeyReleaseMask or ButtonPressMask or
    ButtonReleaseMask or EnterWindowMask or LeaveWindowMask or
    PointerMotionMask or ExposureMask or FocusChangeMask or StructureNotifyMask);

  SetWindowParameters;

  XMapWindow(xdisp.Display, wh);

  AddWindowLookup(self);

// OS independent part:

//  pgfSetMouseCursor(wh, FMouseCursor);

end;

procedure TpgfWindowImpl.DoReleaseWindowHandle;
begin
  if FWinHandle <= 0 then Exit;

  RemoveWindowLookup(self);
  XDestroyWindow(xdisp.Display, FWinHandle);

  FWinHandle := 0;
end;

function TpgfWindowImpl.HandleIsValid: boolean;
begin
  result := (FWinHandle > 0);
end;

procedure TpgfWindowImpl.DoMoveWindow(x, y: TpgfCoord);
begin
  if FWinHandle > 0 then
    XMoveWindow(xdisp.display, FWinHandle, x,y);
end;

procedure TpgfWindowImpl.DoUpdateWindowPosition(aleft,atop,awidth,aheight : TpgfCoord);
var
  w,h : longword;
begin
  if awidth > 1 then w := awidth else w := 1;
  if aheight > 1 then h := aheight else h := 1;

  if FWinHandle > 0 then
    XMoveResizeWindow(xdisp.display, FWinHandle, aleft, atop, w, h);
end;

procedure TpgfWindowImpl.DoSetWindowTitle(const atitle: widestring);
var
  s8 : string;
  p : PByte;
begin
  if FWinHandle <= 0 then Exit;

  s8 := atitle;

  if length(s8) > 0 then p := @s8[1] else p := nil;
  XChangeProperty(xdisp.display, FWinHandle, 39, 31, 8, 0, p, length(s8));
  XChangeProperty(xdisp.display, FWinHandle, 37, 31, 8, 0, p, length(s8));
end;

constructor TpgfWindowImpl.Create(aowner: TComponent);
begin
  inherited;
  FWinHandle := 0;
end;

{ TpgfFontResourceImpl }

constructor TpgfFontResourceImpl.Create(const afontdesc: string);
begin
  FFontData := XftFontOpenName(xdisp.display, xdisp.DefaultScreen, PChar(afontdesc) );
end;

destructor TpgfFontResourceImpl.Destroy;
begin
  if HandleIsValid then XftFontClose(xdisp.Display, FFontData);

  inherited;
end;

function TpgfFontResourceImpl.HandleIsValid: boolean;
begin
  result := (FFontData <> nil);
end;

function TpgfFontResourceImpl.GetAscent: integer;
begin
  result := FFontData^.ascent;
end;

function TpgfFontResourceImpl.GetDescent: integer;
begin
  result := FFontData^.descent;
end;

function TpgfFontResourceImpl.GetHeight: integer;
begin
  result := FFontData^.Ascent + FFontData.Descent;
end;

function TpgfFontResourceImpl.GetTextWidth(const txt: widestring): integer;
var
  extents : TXGlyphInfo;
begin
  if length(txt) < 1 then
  begin
    result := 0;
    exit;
  end;
  XftTextExtents16(xdisp.display, FFontData, @txt[1], Length(txt), extents);
  result := extents.xOff;
end;

{ TpgfCanvasImpl }

constructor TpgfCanvasImpl.Create;
begin
  FDrawing := false;
  FDrawWindow := nil;

  FBufferPixmap := 0;
  FDrawHandle := 0;
  Fgc := nil;
  FXftDraw := nil;
  FClipRegion := nil;
end;

destructor TpgfCanvasImpl.Destroy;
begin
  if FDrawing then DoEndDraw;
  inherited Destroy;
end;

procedure TpgfCanvasImpl.DoBeginDraw(awin : TpgfWindowImpl; buffered : boolean);
var
  x, y : integer;
  rw, d : TXID;
  w, h, bw,
  pmw, pmh : longword;
  GcValues : TXGcValues;
begin
  XGetGeometry(xdisp.display, awin.FWinHandle, @rw, @x, @y, @w, @h, @bw, @d);

  if FDrawing and buffered and (FBufferPixmap > 0) then
  begin
    if FBufferPixmap > 0 then
    begin
      // check if the dimensions are ok
      XGetGeometry(xdisp.display, FBufferPixmap, @rw, @x, @y, @pmw, @pmh, @bw, @d);
      if (pmw <> w) or (pmh <> h) then
      begin
        DoEndDraw;
      end;
    end;
  end;

  if not FDrawing then
  begin
    FDrawWindow := awin;

    if buffered then
    begin
      FBufferPixmap := XCreatePixmap(xdisp.display, FDrawWindow.FWinHandle, w, h, xdisp.DisplayDepth);
      FDrawHandle := FBufferPixmap;
    end
    else
    begin
      FBufferPixmap := 0;
      FDrawHandle := FDrawWindow.FWinHandle;
    end;

    Fgc := XCreateGc(xdisp.display, FDrawHandle, 0, @GcValues);

    FXftDraw := XftDrawCreate(xdisp.display, FDrawHandle,
      XDefaultVisual(xdisp.display, xdisp.DefaultScreen),
      XDefaultColormap(xdisp.display, xdisp.DefaultScreen));

    FClipRegion := XCreateRegion;
  end;

  FDrawing := true;
end;

procedure TpgfCanvasImpl.DoPutBufferToScreen(x, y, w, h: TpgfCoord);
var
  cgc : TpgfGContext;
  GcValues : TXGcValues;
begin
  if FBufferPixmap > 0 then
  begin
    cgc := XCreateGc(xdisp.display, FBufferPixmap, 0, @GcValues);
    XCopyArea(xdisp.Display, FBufferPixmap, FDrawWindow.FWinHandle, cgc, x,y,w,h,x,y);
    XFreeGc(xdisp.display, cgc);
  end;
end;

procedure TpgfCanvasImpl.DoEndDraw;
begin
  if FDrawing then
  begin
    XDestroyRegion(FClipRegion);
    XftDrawDestroy(FXftDraw);
    XFreeGc(xdisp.display, Fgc);

    if FBufferPixmap > 0 then XFreePixmap(xdisp.Display, FBufferPixmap);
    FBufferPixmap := 0;

    FDrawing := false;
    FDrawWindow := nil;
  end;
end;

procedure TpgfCanvasImpl.DoSetFontRes(fntres : TpgfFontResourceImpl);
begin
  if fntres = nil then Exit;
  FCurFontRes := fntres;
end;

procedure TpgfCanvasImpl.DoSetTextColor(cl: TpgfColor);
begin
  SetXftColor(cl,FColorTextXft);
end;

procedure TpgfCanvasImpl.DoSetColor(cl: TpgfColor);
begin
  XSetForeGround(xdisp.display, Fgc, pgfColorToX(cl) );
end;

procedure TpgfCanvasImpl.DoSetLineStyle(awidth: integer; astyle : TpgfLineStyle);
var
  ls : integer;
begin
  if astyle = lsDashed then ls := LineOnOffDash else ls := LineSolid;
  XSetLineAttributes(xdisp.display, Fgc, awidth, ls, 3, 0);
end;

procedure TpgfCanvasImpl.DoDrawString(x, y: TpgfCoord; const txt: widestring);
begin
  if length(txt) < 1 then exit;

  XftDrawString16(FXftDraw, FColorTextXft, FCurFontRes.Handle, x,y+FCurFontRes.GetAscent, @txt[1], Length(txt) )
end;

procedure TpgfCanvasImpl.DoGetWinRect(var r: TpgfRect);
var
  rw : TpgfWinHandle;
  x,y : integer;
  bw,d : longword;
begin
  r.left := 0;
  r.Top := 0;
  XGetGeometry(xdisp.display, FDrawWindow.FWinHandle, @rw, @x, @y, @(r.width), @(r.height), @bw, @d);
end;

procedure TpgfCanvasImpl.DoFillRectangle(x, y, w, h: TpgfCoord);
begin
  XFillRectangle(xdisp.display, FDrawHandle, Fgc, x,y, w, h);
end;

procedure TpgfCanvasImpl.DoXORFillRectangle(col : TpgfColor; x, y, w, h : TpgfCoord);
begin
  XSetForeGround(xdisp.display, Fgc, pgfColorToX(pgfColorToRGB(col)));
  XSetFunction(xdisp.display, Fgc, GXxor);
  XFillRectangle(xdisp.display, FDrawHandle, Fgc, x,y, w, h);
  XSetForeGround(xdisp.display, Fgc, 0);
  XSetFunction(xdisp.display, Fgc, GXcopy);
end;

procedure TpgfCanvasImpl.DoFillTriangle(x1, y1, x2, y2, x3, y3: TpgfCoord);
var
  pts : array[1..3] of TXPoint;
begin
  pts[1].x := x1; pts[1].y := y1;
  pts[2].x := x2; pts[2].y := y2;
  pts[3].x := x3; pts[3].y := y3;

  //XSetFillRule(display, Fgc, 0);
  XFillPolygon(xdisp.display, FDrawHandle, Fgc, @pts, 3, 0,0);
end;

procedure TpgfCanvasImpl.DoDrawRectangle(x, y, w, h: TpgfCoord);
begin
  XDrawRectangle(xdisp.display, FDrawHandle, Fgc, x,y,w-1,h-1);   // transformed into polyline requests!
end;

procedure TpgfCanvasImpl.DoDrawLine(x1, y1, x2, y2: TpgfCoord);
begin
  XDrawLine(xdisp.display, FDrawHandle, Fgc, x1,y1,x2,y2 );
end;

procedure TpgfCanvasImpl.DoDrawArc(x, y, w, h: TpgfCoord; a1, a2: double);
begin
  XDrawArc(xdisp.display, FDrawHandle, Fgc, x, y, w, h,
    trunc(64 * (a1 * 180/pi) + 0.5), trunc(64 * (a2 * 180/pi) + 0.5) );
end;

procedure TpgfCanvasImpl.DoFillArc(x, y, w, h: TpgfCoord; a1, a2: double);
begin
  XFillArc(xdisp.display, FDrawHandle, Fgc, x, y, w, h,
    trunc(64 * (a1 * 180/pi) + 0.5), trunc(64 * (a2 * 180/pi) + 0.5) );
end;

procedure TpgfCanvasImpl.DoSetClipRect(const rect: TpgfRect);
var
  r : TXRectangle;
  rg : TRegion;
begin
  r.x := rect.left;
  r.y := rect.top;
  r.width := rect.width;
  r.height := rect.height;
  rg := XCreateRegion;
  XUnionRectWithRegion(@r,rg,FClipRegion);
  XSetRegion(xdisp.display, Fgc, FClipRegion);
  XftDrawSetClip(FXftDraw, FClipRegion);
  FClipRect := rect;
  FClipRectSet := True;
  XDestroyRegion(rg);
end;

function TpgfCanvasImpl.DoGetClipRect: TpgfRect;
begin
  result := FClipRect;
end;

procedure TpgfCanvasImpl.DoAddClipRect(const rect: TpgfRect);
var
  r : TXRectangle;
  rg : TRegion;
begin
  r.x := rect.left;
  r.y := rect.top;
  r.width := rect.width;
  r.height := rect.height;

  rg := XCreateRegion;
  XUnionRectWithRegion(@r,rg,rg);
  XIntersectRegion(FClipRegion,rg,FClipRegion);
  XSetRegion(xdisp.display, Fgc, FClipRegion);
  FClipRect := Rect;
  FClipRectSet := True;
  XftDrawSetClip(FXftDraw, FClipRegion);
  XDestroyRegion(rg);
end;

procedure TpgfCanvasImpl.DoClearClipRect;
var
  r : TpgfRect;
begin
  DoGetWinRect(r);
  DoSetClipRect(r);
  FClipRectSet := False;
end;

procedure TpgfCanvasImpl.DoDrawImagePart(x, y: TpgfCoord;
  img: TpgfImageImpl; xi, yi, w, h: integer);
var
  msk : TPixmap;
  gc2,drawgc : Tgc;
  GcValues : TXGcValues;
begin
  if img = nil then exit;

  if img.FMasked then
  begin
    // rendering the mask

    msk := XCreatePixmap(xdisp.display, XDefaultRootWindow(xdisp.display), w, h, 1);
    GcValues.foreground := 1;
    GcValues.background := 0;
    gc2 := XCreateGc(xdisp.display, msk, GCForeground or GCBackground, @GcValues);

    // clear mask
    XSetForeground(xdisp.display, gc2, 0);
    XFillRectangle(xdisp.display, msk, gc2, 0,0, w, h);

//    XOffsetRegion(FClipRegion, -x, -y);
//    XSetRegion(xdisp.display, gc2, FClipRegion);
//    XOffsetRegion(FClipRegion, x, y);

    XSetForeground(xdisp.display, gc2, 1);
    XPutImage(xdisp.display, msk, gc2, img.XImageMask, xi,yi, 0,0, w, h);

    drawgc := XCreateGc(xdisp.display, FDrawHandle, 0, @GcValues);

    XSetClipMask(xdisp.display, drawgc, msk);
    XSetClipOrigin(xdisp.display, drawgc, x,y);

    XPutImage(xdisp.display, FDrawHandle, drawgc, img.XImage, xi,yi, x,y, w, h);

    XFreePixmap(xdisp.display, msk);
    XFreeGc(xdisp.display,drawgc);
    XFreeGc(xdisp.display,gc2);
  end
  else
  begin
    XPutImage(xdisp.display, FDrawHandle, Fgc, img.XImage, xi,yi, x,y, w, h);
  end;
end;

{ TpgfImageImpl }

constructor TpgfImageImpl.Create;
begin
end;

procedure TpgfImageImpl.DoFreeImage;
begin
  // does nothing on X11
end;

procedure TpgfImageImpl.DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: pointer);
begin
  FMasked := false;

  with FXimg do
  begin
    width := awidth;
    height := aheight;
    xoffset := 0;
    obdata := #0;
    byte_order := LSBFirst;
    bitmap_bit_order := MSBFirst;
    bitmap_pad := 32;
    bytes_per_line := 0;

    if acolordepth = 1 then
    begin
      format := XYBitmap;
      bitmap_unit := 8;
      depth := 1;
      bits_per_pixel := 1;
      red_mask   := 1;
      green_mask := 0;
      blue_mask  := 0;
    end
    else
    begin
      format := ZPixmap;
      bitmap_unit := 32;

      // only truecolor 24/32 displays supported now, otherwise color conversion required!

      // this must be match for the display !!!
      depth := xdisp.DisplayDepth; //  acolordepth;
      bits_per_pixel := 32;

      red_mask :=   $000000FF;
      green_mask := $0000FF00;
      blue_mask :=  $00FF0000;
    end;

    data := aimgdata;
  end;

  XInitImage(@FXimg);
end;

procedure TpgfImageImpl.DoInitImageMask(awidth, aheight: integer; aimgdata: pointer);
begin
  FMasked := true;

  with FXimgMask do
  begin
    width := awidth;
    height := aheight;
    xoffset := 0;
    format := XYBitmap;
    byte_order := LSBFirst;
    bitmap_unit := 8;
    bitmap_bit_order := MSBFirst;
    bitmap_pad := 32;
    depth := 1;
    bytes_per_line := 0;
    bits_per_pixel := 1;

    red_mask :=   1;
    green_mask := 0;
    blue_mask :=  0;

    obdata := #0;

    data := aimgdata;
  end;

  XInitImage(@FXimgMask);
end;

function TpgfImageImpl.XImage: PXImage;
begin
  result := @FXimg;
end;

function TpgfImageImpl.XImageMask: PXImage;
begin
  result := @FXimgMask;
end;

initialization
begin
  xdisp := nil;
end;

end.

