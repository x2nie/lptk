{ gfxbase.pas: base functionality and OS dependent functions
  File maintainer: nvitya@freemail.hu

History:
  15.01.2004  complete buffering support for Windows and Linux
}
unit gfxbase;

{$DEFINE BUFFERING}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}
{$IFDEF BUFFERING}
{$IFNDEF win32}
{$linklib Xext}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$ifdef Win32}
    windows, {$ifndef FPC}messages,{$endif}
{$else}
    X, Xlib, XUtil, unitxft,
{$endif}
  schar16, messagequeue;

const
  LINEFEEDSTRING = #13#10;

{$INCLUDE lptkstrings.inc}

type
  TGfxCoord = integer;     // Maybe we will use floating point coordinates in the future...

  TGfxColor = longword;

type
  TOrientation = (orVertical, orHorizontal);

  TAlignment  = (alLeft, alRight, alCenter, alJustify);

  TAnchor = (anLeft,anRight,anTop,anBottom);
  TAnchors = set of TAnchor;

  TClipboardKeyType = (ckNone, ckCopy, ckPaste, ckCut);

const
  AllAnchors = [anLeft,anRight,anTop,anBottom];

type
  TNotifyEvent = procedure(Sender : TObject) of object;

  TKeyPressNotifyEvent = procedure(Sender: TObject; var keycode: word; var shiftstate: word; var consumed : boolean) of object;
  TMouseNotifyEvent    = procedure(Sender: TObject; x,y : integer; var button : word; var shiftstate : word) of object;

type
  TSizeParams = record
    min_width, max_width,
    min_height, max_height : TGfxCoord;
  end;

  TGfxRect = object  // not class !
    top, left,
    width, height : TGfxCoord;
    procedure SetRect(aleft,atop,awidth,aheight : TGfxCoord);
    function bottom : TGfxCoord;
    function right  : TGfxCoord;
    procedure SetBottom(value : TGfxCoord);
    procedure SetRight(value : TGfxCoord);
  end;

{$ifdef Win32}
type
  TWinHandle   = HWND;
  TGfxFontData = HFONT;
  TFontHandle  = HFONT;
  TGContext    = HDC;
  TGfxDisplay  = HDC;

{$ifndef FPC}
  WndProc = TFNWndProc;
{$else}
const
  WM_MOUSEWHEEL       = $020A;
{$endif}

const
  KEY_LEFT  = $FF25;
  KEY_RIGHT = $FF27;
  KEY_DOWN  = $FF28;
  KEY_UP    = $FF26;

  KEY_PGUP    = $FF21;
  KEY_PGDN    = $FF22;
  KEY_END     = $FF23;
  KEY_HOME    = $FF24;
  KEY_INSERT  = $FF2D;
  KEY_DELETE  = $FF2E;

  KEY_ENTER   = $FF0D;
  KEY_ESC     = $FF1B;
  KEY_BACKSPACE = $FF08;

  KEY_TAB     = $FF09;
  KEY_STAB    = $FE09;

  KEY_F1      = $FF70;
  KEY_F2      = KEY_F1 + 1;
  KEY_F3      = KEY_F1 + 2;
  KEY_F4      = KEY_F1 + 3;
  KEY_F5      = KEY_F1 + 4;
  KEY_F6      = KEY_F1 + 5;
  KEY_F7      = KEY_F1 + 6;
  KEY_F8      = KEY_F1 + 7;
  KEY_F9      = KEY_F1 + 8;
  KEY_F10     = KEY_F1 + 9;
  KEY_F11     = KEY_F1 + 10;
  KEY_F12     = KEY_F1 + 11;

const
  MSG_PAINT      = WM_PAINT;
  MSG_KEYPRESS   = WM_KEYDOWN;
  MSG_KEYRELEASE = WM_KEYUP;

  MSG_ACTIVATE   = WM_ACTIVATE;

  MSG_MOUSEDOWN  = WM_LBUTTONDOWN;
  MSG_MOUSEUP    = WM_LBUTTONUP;

  MSG_MOUSEMOVE  = WM_MOUSEMOVE;

  MSG_CLOSE      = WM_CLOSE;

const
  MSG_LPTKINT = WM_USER;

  MSG_SCROLL     = MSG_LPTKINT + 1;
  MSG_RESIZE     = MSG_LPTKINT + 2;

  MSG_POPUPCLOSE = MSG_LPTKINT + 3;

  MSG_MOUSEENTER = MSG_LPTKINT + 4;
  MSG_MOUSEEXIT  = MSG_LPTKINT + 5;

  MSG_DEACTIVATE = MSG_LPTKINT + 6;
  
  MSG_MOVE       = MSG_LPTKINT + 7;
  
  MSG_DOUBLECLICK = MSG_LPTKINT + 8;

const
  CUR_DEFAULT   = 1;
  CUR_DIR_EW    = 2;
  CUR_DIR_NS    = 3;
  CUR_EDIT      = 4;

  CUR_DIR_NWSE  = 5;
  CUR_DIR_NESW  = 6;

  CUR_MOVE      = 7;

  CUR_CROSSHAIR = 8;

const
  ss_Shift   = $0001;
  ss_Control = $0004;
  ss_Alt     = $0008;

  ss_CapsLock   = $0002;
  ss_NumLock    = $0010;
  ss_ScrollLock = $0080;

{$else}

type
  TWinHandle   = TXID;
  TGfxDisplay  = PXDisplay;
  TGfxFontData = PXftFont;
  TFontHandle  = PXftFont;
  TGContext    = Xlib.TGc;

const
  KEY_LEFT  = $FF51;
  KEY_RIGHT = $FF53;
  KEY_DOWN  = $FF54;
  KEY_UP    = $FF52;

  KEY_END     = $FF57;
  KEY_HOME    = $FF50;

  KEY_PGUP    = $FF55;
  KEY_PGDN    = $FF56;
  KEY_INSERT  = $FF63;
  KEY_DELETE  = $FFFF;

  KEY_ENTER   = $FF0D;
  KEY_ESC     = $FF1B;
  KEY_BACKSPACE = $FF08;

  KEY_TAB     = $FF09;
  KEY_STAB    = $FE20;

  KEY_F1      = $FFBE;
  KEY_F2      = KEY_F1 + 1;
  KEY_F3      = KEY_F1 + 2;
  KEY_F4      = KEY_F1 + 3;
  KEY_F5      = KEY_F1 + 4;
  KEY_F6      = KEY_F1 + 5;
  KEY_F7      = KEY_F1 + 6;
  KEY_F8      = KEY_F1 + 7;
  KEY_F9      = KEY_F1 + 8;
  KEY_F10     = KEY_F1 + 9;
  KEY_F11     = KEY_F1 + 10;
  KEY_F12     = KEY_F1 + 11;

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

const
  CUR_DEFAULT   = 68;
  CUR_DIR_EW    = 108;
  CUR_DIR_NS    = 116;
  CUR_EDIT      = 152;

  CUR_DIR_NWSE  = 120;
  CUR_DIR_NESW  = 120;  // Not Perfect!!!
  
  CUR_MOVE      = 52;
  
  CUR_CROSSHAIR = 34;

const
  DOUBLECLICK_MS = 200; // the max time between left-clicks for doubleclick
  
const
  ss_Shift   = $0001;
  ss_Control = $0004;
  ss_Alt     = $0008;

  ss_CapsLock   = $0002;
  ss_NumLock    = $0010;
  ss_ScrollLock = $0080;

const
  MWM_HINTS_FUNCTIONS    = 1;       // Definitions for FXMotifHints.flags
  MWM_HINTS_DECORATIONS  = 2;
  MWM_HINTS_INPUT_MODE   = 4;
  MWM_HINTS_ALL          = 7;

  MWM_FUNC_ALL           = 1;       // Definitions for FXMotifHints.functions
  MWM_FUNC_RESIZE        = 2;
  MWM_FUNC_MOVE          = 4;
  MWM_FUNC_MINIMIZE      = 8;
  MWM_FUNC_MAXIMIZE      = 16;
  MWM_FUNC_CLOSE         = 32;

  MWM_DECOR_ALL          = 1;       // Definitions for FXMotifHints.decorations
  MWM_DECOR_BORDER       = 2;
  MWM_DECOR_RESIZEH      = 4;
  MWM_DECOR_TITLE        = 8;
  MWM_DECOR_MENU         = 16;
  MWM_DECOR_MINIMIZE     = 32;
  MWM_DECOR_MAXIMIZE     = 64;

  MWM_INPUT_MODELESS                  = 0;   // Values for FXMotifHints.inputmode
  MWM_INPUT_PRIMARY_APPLICATION_MODAL = 1;
  MWM_INPUT_SYSTEM_MODAL              = 2;
  MWM_INPUT_FULL_APPLICATION_MODAL    = 3;

const
  XA_PRIMARY     = 1;
  XA_SECONDARY   = 2;
  XA_ATOM        = 4;
  XA_BITMAP      = 5;
  XA_CURSOR      = 8;
  XA_INTEGER     = 19;
  XA_PIXMAP      = 20;
  XA_POINT       = 21;
  XA_RECTANGLE   = 22;
  XA_RESOURCE_MANAGER = 23;
  XA_STRING      = 31;
  XA_VISUALID    = 32;
  XA_WINDOW      = 33;
  XA_WM_COMMAND  = 34;
  XA_WM_HINTS    = 35;
  XA_WM_ICON_NAME = 37;
  XA_WM_ICON_SIZE = 38;
  XA_WM_NAME      = 39;
  XA_WM_NORMAL_HINTS = 40;
  XA_WM_SIZE_HINTS   = 41;

{$endif}

type

  TGfxFontResource = class
  private
    FFont : TGfxFontData;
    FRefCount : integer;
    FFontDesc : string;
{$ifdef Win32}
    FMetrics : Windows.TEXTMETRIC;
{$else}{$endif}
  public
    constructor Create(afont : TGfxFontData; aFontDesc : string);
    destructor Destroy; override;

    function Handle : TFontHandle;

    property FontDesc : string read FFontDesc;

{$ifdef Win32}
    property Metrics : Windows.TEXTMETRIC read FMetrics;
{$else}{$endif}
  end;

  TGfxFont = class
  private
    FFontRes : TGfxFontResource;
    FName : string;
  public
    constructor Create(afont : TGfxFontResource; AllocName : string);
    destructor Destroy; override;

    function Handle : TFontHandle;

    function TextWidth16(txt : string16) : integer;

    function Ascent  : integer;
    function Descent : integer;
    function Height  : integer;

    function FontDescx : string;
    
    property FontName : string read FName;
  end;

  TGfxImage = class
  private
{$ifdef Win32}
    FBMPHandle : HBITMAP;
    FMaskHandle : HBITMAP;
{$else}
    FXimg  : TXImage;
    FXimgmask : TXImage;
{$endif}
  protected
    FWidth, FHeight : integer;
    FColorDepth : integer;

    FMasked : boolean;

    FImageData : pointer;
    FImageDataSize : integer;

    FMaskData : pointer;
    FMaskDataSize : integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure FreeImage;
    procedure AllocateRGBImage(awidth, aheight : integer);
    procedure Allocate2CImage(awidth, aheight : integer);
    procedure AllocateMask;

    procedure Invert;
    procedure CreateMaskFromSample(x,y : integer);

    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property ColorDepth : integer read FColorDepth;

    property ImageData : pointer read FImageData;
    property ImageDataSize : integer read FImageDataSize;
    property MaskData : pointer read FMaskData;
    property MaskDataSize : integer read FMaskDataSize;

    property Masked : boolean read FMasked;

    procedure LoadFromFile(AFileName : String);

{$ifdef Win32}
    procedure SetWindowsBitmap(pdata, pinfoheader : pointer; startscan, scanlines : longword);
    property BMPHandle : HBITMAP read FBMPHandle;
    property MaskHandle : HBITMAP read FMaskHandle;
{$else}
    function XImage : PXImage;
    function XImageMask : PXImage;
{$endif}

  end;

  TGfxCanvas = class
  private
     FWin : TWinHandle;
     FBufferWin : TWinHandle;
     FMainWin : TWinHandle;
     Fgc  : TGContext;
     FColorText : TGfxColor;
     FColor     : TGfxColor;
     FBackgroundColor : TgfxColor;
     FCurFont   : TGfxFont;
     FClipRect  : TGfxRect;
     FClipRectSet : Boolean;
     FBufferClipRect : TGfxRect;
     FBufferClipRectSet : Boolean;     
     FLineStyle : integer;
     FLineWidth : integer;
     FDrawOnBuffer : Boolean;
{$ifdef Win32}
     FWindowsColor : longword;

     FBrush : HBRUSH;
     FPen   : HPEN;
     FClipRegion   : HRGN;

     FBufferBrush : HBRUSH;
     FBufferPen   : HPEN;
     FBufferClipRegion   : HRGN;
     FBufferGC : TGContext;
     FBufferBitmap : HBitmap;
     FBufferFont : TgfxFont;
{$else}
     FXftDraw : PXftDraw;
     FXftDrawBuffer : PXftDraw;
     FColorTextXft : TXftColor;
     FClipRegion   : TRegion;
{$endif}
  protected
    procedure SetDrawOnBuffer(AValue : Boolean);
  public
    {$IFDEF win32}
    procedure _ReCreateBuffer(AWidth, AHeight : Integer);
    {$ENDIF}
    constructor Create(winhandle : TWinHandle);
    destructor Destroy; override;

    procedure MoveResizeWindow(x,y,w,h : TGfxCoord);
    procedure SwapBuffer;
    procedure SetFont(fnt : TGfxFont);
    procedure SetTextColor(cl : TGfxColor);

    procedure SetColor(cl : TGfxColor);
    procedure SetLineStyle(width : integer; dashed : boolean);

    procedure DrawString16(x,y : TGfxCoord; txt : String16);

    procedure FillRectangle(x,y, w,h : TGfxCoord);
    procedure FillRect(r : TGfxRect);

    procedure FillTriangle(x1,y1, x2,y2, x3,y3 : TGfxCoord);

    procedure DrawRectangle(x,y, w,h : TGfxCoord);
    procedure DrawRect(r : TGfxRect);

    procedure DrawLine(x1,y1,x2,y2 : TGfxCoord);

    procedure DrawSelectionRectangle(x, y, w, h : TGfxCoord);

    procedure SetClipRect(const rect : TGfxRect);
    function GetClipRect : TgfxRect;
    procedure AddClipRect(const rect : TGfxRect);
    procedure ClearClipRect;

    procedure GetWinRect(var r : TGfxRect);

    procedure Clear(col : TGfxColor);

    procedure DrawImage(x,y : TGfxCoord; img : TGfxImage);
    procedure DrawImagePart(x,y : TGfxCoord; img : TGfxImage; xi,yi,w,h : integer);
  public

    property Font : TGfxFont read FCurFont write SetFont;
    property TextColor : TGfxColor read FColorText;
    property Color : TGfxColor read FColor;
    property DrawOnBuffer : Boolean read FDrawOnBuffer write SetDrawOnBuffer;
  end;

var
  ScreenWidth, ScreenHeight : integer;

  Display : TGfxDisplay;

{$ifdef Win32}

  WindowClass : TWndClass;
  WidgetClass : TWndClass;

  hcr_default : HCURSOR;
  hcr_dir_ew  : HCURSOR;
  hcr_dir_ns  : HCURSOR;
  hcr_edit    : HCURSOR;

  hcr_dir_nwse,
  hcr_dir_nesw,
  hcr_move,
  hcr_crosshair : HCURSOR;

  FFocusedWindow : THANDLE;

{$else}

  DisplayDepth : integer;

  DefaultBackground : TGfxColor;
  DefaultForeground : TGfxColor;

  GfxDefaultScreen : integer;
  GfxDefaultVisual : PVisual;
  GfxDefaultColorMap : TColorMap;
  GfxRootWindow : TWinHandle;

var
  xia_clipboard        : TAtom;
  xia_motif_wm_hints   : TAtom;
  xia_wm_protocols     : TAtom;
  xia_wm_delete_window : TAtom;

  xia_wm_state       : TAtom;
  xia_wm_state_modal : TAtom;

  xia_targets        : TAtom;
{$endif}

var
  GfxImageLibrary : TStringList;   // this is public for listing
  
function GfxOpenDisplay(DisplayName : string) : boolean;
procedure GfxCloseDisplay;

function GfxColorToRGB(col : TGfxColor) : TGfxColor;

function GfxGetFont(desc : string) : TGfxFont;
function GfxGetFontFaceList : TStringList;

procedure GfxProcessMessages;

procedure WaitWindowMessage;
procedure GfxDoMessageLoop;

procedure GfxFlush;

procedure GfxActivateWindow(wh : TWinHandle);

procedure GfxGetAbsolutePosition(wh : TWinHandle; x,y : TGfxCoord; var xap, yap : TGfxCoord);

procedure GfxSetMouseCursor(wh : TWinHandle; cur : integer);

function GfxCheckClipboardKey(key, shiftstate : word) : TClipboardKeyType;

function GfxIsAlphaNum16(s : string16) : boolean;

{$ifdef Win32}
function GfxColorToWin(col : TGfxColor) : TGfxColor;
{$else}
procedure SetXftColor(col : TGfxColor; var colxft : TXftColor);
procedure GfxSetWMOptions(wh : TWinHandle; aflags, afunctions, adecorations, ainputmode : longword);
function GfxColorToX(col : TGfxColor) : longword;
{$endif}

procedure GfxHideConsoleWindow;


function GfxLibAddImage(const imgid : string; img : TGfxImage) : boolean;
function GfxLibDeleteImage(const imgid : string; freeimg : boolean) : boolean;
function GfxLibGetImage(const imgid : string) : TGfxImage;

function GfxLibAddBMP(const imgid : string; bmpdata : pointer; bmpsize : integer) : TGfxImage;
function GfxLibAddMaskedBMP(const imgid : string; bmpdata : pointer; bmpsize : integer; mcx, mcy : integer) : TGfxImage;

implementation

uses unitkeys, gfxstyle, gfxwidget, gfxform, gfxclipboard, popupwindow, gfxstdimg, gfxbmpimage;

var
  FFontResourceList : TList;

{$ifdef Win32}{$else}
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
  PXdbeSwapInfo = ^TXdbeSwapInfo;
  TXdbeSwapInfo = record
                Window : TWinHandle;
                SwapAction : PChar;
  end;

var
  InputMethod  : PXIM;
  InputContext : PXIC;

var
  LastClickWindow  : TWinHandle;
  LastWinClickTime : longword;

// defines:
procedure XRenderSetPictureClipRectangles(disp : PXDisplay; pic : TPicture; xorigin,yorigin : integer; rect : PXRectangle; num : integer); cdecl; external;

// redefines:
function XmbLookupString(p1 : PXIC; ev : PXKeyPressedEvent; str : PChar; len : longword; ks:PKeySym; stat:PStatus):longint;cdecl; external;

{$IFDEF BUFFERING}
function XdbeQueryExtension(ADisplay : PXDisplay; AMajor, AMinor : PInt) : PStatus; cdecl; external;
function XdbeAllocateBackBufferName(ADisplay : PXDisplay; AWindow : TWinHandle; ASwapAction : PChar) : TWinHandle; cdecl; external;
function XdbeSwapBuffers(ADisplay : PXDisplay;ASwapInfo : PXdbeSwapInfo; AScreenNums : Integer) : PStatus; cdecl; external;
function XdbeDeallocateBackBufferName(ADisplay : PXDisplay; ABuffer : TWinHandle) : PStatus; cdecl; external;
{$ENDIF}

function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):PXIM;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;

{$endif}

{$ifdef Win32}
function GetConsoleWindowHandle : HWND;
var
  s, ns : string;
begin
  SetLength(s,1024);
  GetConsoleTitle(@s[1],length(s));
  ns := 'CONSOLE'+IntToStr(maininstance)+#0;
  SetConsoleTitle(PChar(ns));
  Sleep(40);
  result := FindWindow(nil, PChar(ns));
  SetConsoleTitle(PChar(s));
end;
{$else}{$endif}

procedure GfxHideConsoleWindow;
{$ifdef Win32}
var
  h : HWND;
begin
  h := GetConsoleWindowHandle;
  //Writeln('Console window: ', h);

  if h <> 0 then
    SetWindowPos(h, 0, 0,0,0,0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOZORDER or SWP_NOMOVE);
{$else}
begin
{$endif}
end;

procedure GfxFlush;
begin
{$ifdef Win32} GdiFlush; {$else} XFlush(display); {$endif}
end;

(*
procedure GfxMoveResizeWindow(wh : TWinHandle; x,y,w,h : TGfxCoord);
{$ifdef Win32}
var
  rwidth, rheight : integer;
  ws,es : integer;
  r : TRect;
{$endif}  
begin
  if wh <= 0 then Exit;

{$ifdef Win32}
  // windows decoration correction on stupid windows...
  ws := GetWindowLong(wh, GWL_STYLE);
  es := GetWindowLong(wh, GWL_EXSTYLE);

  rwidth := w;
  rheight := h;

  if (ws and WS_CHILD) = 0 then
  begin
    r.Left := x;
    r.Top  := y;
    r.Right := x + w;
    r.Bottom := y + h;
    AdjustWindowRectEx(r, ws, false, es);
    rwidth := r.Right - r.Left;
    rheight := r.Bottom - r.Top;
  end;

  windows.MoveWindow(wh, x,y, rwidth, rheight, true);
{$else}
  XMoveResizeWindow(display, wh, x,y,w,h);
{$endif}
end;
*)

procedure GfxActivateWindow(wh : TWinHandle);
begin
  if wh > 0 then
  begin
{$ifdef Win32}
    Windows.SetActiveWindow(wh);
{$else}
    XMapRaised(display, wh);
    XSetInputFocus(display, wh, RevertToNone, 0);
{$endif}
  end;
end;

procedure GfxGetAbsolutePosition(wh : TWinHandle; x,y : TGfxCoord; var xap, yap : TGfxCoord);
var
{$ifdef Win32}
  pt : TPoint;
{$else}
  cw : TWinHandle;
{$endif}
begin
  if wh > 0 then
  begin
{$ifdef Win32}
    pt.X := x;
    pt.Y := y;
    ClientToScreen(wh, pt);
    xap := pt.X;
    yap := pt.Y;
{$else}
    XTranslateCoordinates(display, wh, GfxRootWindow, x, y, @xap, @yap, @cw);
{$endif}
  end;
end;

function GfxIsAlphaNum16(s : string16) : boolean;
var
  c : char;
begin
  result := true;
  if length(s) < 2 then
  begin
    result := false;
    exit;
  end;

  if s[2] > #0 then Exit;

  c := s[1];

  if (c < '0') or
     ((c > 'Z') and (c < 'a')) or
     ((c > 'z') and (c < #$C0))
  then result := false
end;

procedure GfxSetMouseCursor(wh : TWinHandle; cur : integer);
var
{$ifdef Win32}
  hc : HCURSOR;
{$else}
  xc : TCursor;
{$endif}
begin
  if wh <= 0 then Exit;

{$ifdef Win32}
  case cur of
  CUR_DIR_EW:  hc := hcr_dir_ew;
  CUR_DIR_NS:  hc := hcr_dir_ns;
  CUR_EDIT:    hc := hcr_edit;

  CUR_DIR_NWSE: hc := hcr_dir_nwse;
  CUR_DIR_NESW: hc := hcr_dir_nesw;

  CUR_MOVE:     hc := hcr_move;

  CUR_CROSSHAIR: hc := hcr_crosshair;

  else
    hc := hcr_default;
  end;

  SetCursor(hc);
  //ShowCursor(true);

{$else}
  xc := XCreateFontCursor(display, cur);
  XDefineCursor(display, wh, xc);
  XFreeCursor(display, xc);
{$endif}
end;

function GfxCheckClipboardKey(key, shiftstate : word) : TClipboardKeyType;
begin
  result := ckNone;

  if key = KEY_INSERT then
  begin
    if (shiftstate and ss_control) <> 0 then result := ckCopy
    else if (shiftstate and ss_shift) <> 0 then result := ckPaste;
  end
  else if (key = KEY_DELETE) and ((shiftstate and ss_control) <> 0) then result := ckCut
  else
  if (shiftstate and ss_control) <> 0 then
  begin
    case key of
{$ifdef Win32}
      $0003:  result := ckCopy;
      $0016:  result := ckPaste;
      $0018:  result := ckCut;
{$else}
      $0043, $0063:  result := ckCopy;
      $0056, $0076:  result := ckPaste;
      $0058, $0078:  result := ckCut;
{$endif}
    end;
  end

end;

function FindKeyboardFocus : TWidget;
begin
  Result := nil;

  if FocusRoot <> nil then
  begin
    Result := FocusRoot;
    while (Result <> nil) and (result.ActiveWidget <> nil) do result := result.ActiveWidget;
  end;
end;

function GfxColorToRGB(col : TGfxColor) : TGfxColor;
begin
  if (col and $80000000) <> 0 then
  begin
    // named color
    result := guistyle.GetNamedColorRGB(col) or (col and $7F000000);  // keeping alpha
  end
  else result := col;
end;

{$ifdef Win32}

function GfxColorToWin(col : TGfxColor) : TGfxColor;
var
  c : dword;
begin
  if (col and $80000000) <> 0 then
  begin
    // named color
    c := guistyle.GetNamedColorRGB(col);
  end
  else c := col;
  //swapping bytes
  result := ((c and $FF0000) shr 16) or ((c and $0000FF) shl 16) or (c and $00FF00);
end;

function GetMyWidgetFromHandle(wh : TWinHandle) : TWidget;
begin
  if (wh <> 0) and (MainInstance = LongWord(GetWindowLong(wh, GWL_HINSTANCE))) then
  begin
    result := TWidget(Windows.GetWindowLong(wh, GWL_USERDATA));
  end
  else result := nil;
end;

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

  if GfxTopModalForm <> nil then
  begin
    pwg := WidgetParentForm(wwg);
    if (pwg <> nil) and (GfxTopModalForm <> pwg) then wwg := nil;
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

function GfxWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  wg,kwg : TWidget;
  kcode,i : integer;
  sstate : integer;
  h : THANDLE;
  p : PChar;
  pt : TPOINT;
  r  : TRECT;
  blockmsg : boolean;
begin
//  Writeln('got the message: ',umsg);
//  writeln('WND=',IntToHex(hwnd,8),' MSG=',IntToHex(uMsg,4),' wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));

  if uMsg = WM_CREATE then
  begin
    wg := TWidget(PCreateStruct(lParam)^.lpCreateParams);
    //wg.SetWinHandle(hwnd);
    Windows.SetWindowLong(hwnd, GWL_USERDATA, LongWord(wg));
  end
  else if (uMsg = WM_RENDERALLFORMATS) or (uMsg = WM_RENDERFORMAT) then
  begin
    writeln('cliboard rendering...');

    if uMsg = WM_RENDERALLFORMATS then
    begin
      writeln('ALL');
      CloseClipboard();
      OpenClipboard(0);
    end;
    // Windoze seems unhappy unless I do these two steps. Documentation
    // seems to vary on whether opening the clipboard is necessary or
    // is in fact wrong:
    // fall through...

    h := GlobalAlloc(GHND, Length(ClipboardData)+1);
    if (h <> 0) then
    begin
      p := GlobalLock(h);
      move(ClipboardData[1],p^,Length(ClipboardData));
      inc(p,length(ClipboardData));
      p^ := #0;
      GlobalUnlock(h);
      SetClipboardData(CF_TEXT, h);
    end;

    // Windoze also seems unhappy if I don't do this. Documentation very
    // unclear on what is correct:
    if uMsg = WM_RENDERALLFORMATS then CloseClipboard();

    result := 1;
    Exit;
  end;

  wg := TWidget(Windows.GetWindowLong(hwnd, GWL_USERDATA));

  result := 0;

  if not Assigned(wg) then
  begin
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    Exit;
  end;

  blockmsg := false;

  case uMsg of

    WM_CHAR,
    WM_KEYDOWN:
    begin
      kcode := wParam;
      if uMsg <> WM_CHAR then kcode := kcode or $FF00;

      sstate := 0;
      if GetKeyState(VK_SHIFT) < 0 then sstate := sstate + ss_shift;
      //if GetKeyState(VK_MENU) < 0 then sstate := sstate + ss_alt;
      if GetKeyState(VK_CONTROL) < 0 then sstate := sstate + ss_control;

      //Writeln('msg: ',umsg,' wp=',IntToHex(wParam,4),' lp=',IntToHex(lparam,8));

      if (kcode = KEY_TAB) and ((sstate and ss_shift) <> 0) then kcode := KEY_STAB;

      case kcode of
        $0008, $000D, $001B, $0009:
        begin
          // don't send it!
        end;
      else
        //writeln('KEYDOWN: ',IntToHex(kcode,4));

        kwg := FindKeyboardFocus;
        if kwg <> nil then SendMessage(nil, kwg, MSG_KEYPRESS, kcode, sstate, 0 )
                      else SendMessage(nil, wg,  MSG_KEYPRESS, kcode, sstate, 0 );

      end;

    end;


    WM_KEYUP:
    begin
      kcode := wParam;
      sstate := 0;
      if GetKeyState(VK_SHIFT) < 0 then sstate := sstate + ss_shift;
      if GetKeyState(VK_CONTROL) < 0 then sstate := sstate + ss_control;
      kwg := FindKeyboardFocus;

      case kcode of
        $0008, $000D, $001B, $0009:
        begin
          kcode := kcode or $FF00;
        end;
      end;

      if (kcode = KEY_TAB) and ((sstate and ss_shift) <> 0) then kcode := KEY_STAB;

      //writeln('KEYUP: ',IntToHex(kcode,4));

      if kwg <> nil then SendMessage(nil, kwg, MSG_KEYRELEASE, kcode, sstate, 0 )
		    else SendMessage(nil, wg,  MSG_KEYRELEASE, kcode, sstate, 0 );
    end;

    WM_SETCURSOR:
    begin
      //Writeln('Hittest: ',IntToHex((lParam and $FFFF),4));
      if (lParam and $FFFF) <= 1 then
      begin
        GfxSetMouseCursor(wg.WinHandle, wg.MouseCursor);
        result := 1;
      end
      else Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
    end;


    WM_MOUSEMOVE:
    begin
      i := 0;
      if (wParam and MK_LBUTTON) <> 0 then i := i or 1;
      if (wParam and MK_RBUTTON) <> 0 then i := i or 2;
      if (wParam and MK_MBUTTON) <> 0 then i := i or 4;
      SendMouseMessage(wg, MSG_MOUSEMOVE, i, wParam, lParam);

      // OK! Windoze doesn't provide MOUSEENTER and MOUSEEXIT messages, so we
      // have to generate implicitly 'couse we need it for buttons

      GetCursorPos(PT);
      h := WindowFromPoint(PT);
      if h <> FFocusedWindow then
      begin
        if FFocusedWindow > 0 then
        begin
           wg := GetMyWidgetFromHandle(FFocusedWindow);
           if wg <> nil then SendMouseMessage(wg, MSG_MOUSEEXIT, 0, 0, 0);
        end;

        wg := GetMyWidgetFromHandle(h);
        if wg <> nil then
        begin
          FFocusedWindow := h;
          SendMouseMessage(wg, MSG_MOUSEENTER, 0, 0, 0);
        end
        else
        begin
          FFocusedWindow := 0;
        end;
      end;

    end;

    WM_LBUTTONDOWN:  SendMouseMessage(wg, MSG_MOUSEDOWN, 1, wParam, lParam);
    WM_LBUTTONUP:    SendMouseMessage(wg, MSG_MOUSEUP, 1, wParam, lParam);

    WM_LBUTTONDBLCLK: SendMouseMessage(wg, MSG_DOUBLECLICK, 1, wParam, lParam);

    WM_RBUTTONDOWN:  SendMouseMessage(wg, MSG_MOUSEDOWN, 2, wParam, lParam);
    WM_RBUTTONUP:    SendMouseMessage(wg, MSG_MOUSEUP, 2, wParam, lParam);

    WM_MBUTTONDOWN:  SendMouseMessage(wg, MSG_MOUSEDOWN, 3, wParam, lParam);
    WM_MBUTTONUP:    SendMouseMessage(wg, MSG_MOUSEUP, 3, wParam, lParam);

{
    WM_SIZING:
    begin

      GetWindowRect(wg.WinHandle, r);
      SendMessage(nil, wg, MSG_RESIZE,
                           (PRect(lParam)^.Right - PRect(lParam)^.Left) - (r.Right - r.Left),
                           (PRect(lParam)^.Bottom - PRect(lParam)^.Top) - (r.Bottom - r.Top), 0);
    end;
}

    WM_SIZE:
    begin

      //writeln('WM_SIZE: wp=',IntToHex(wparam,8), ' lp=',IntToHex(lparam,8));

      if lparam <> 0 then  // skip minimize...
        SendMessage(nil, wg, MSG_RESIZE, integer(lParam and $FFFF) - wg.Width, integer((lParam and $FFFF0000) shr 16) - wg.Height, 0);

    end;

    WM_MOVE:
    begin
      // window decoration correction on stupid windows...

      r.Left := smallint(lParam and $FFFF);
      r.Top  := smallint((lParam and $FFFF0000) shr 16);

      if (GetWindowLong(wg.WinHandle, GWL_STYLE) and WS_CHILD) = 0 then
      begin
        GetWindowRect(wg.WinHandle, r);
      end;

      SendMessage(nil, wg, MSG_MOVE, r.Left, r.Top, 0);
    end;

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

    WM_ACTIVATE:
    begin
      if ((wParam and $FFFF) = WA_INACTIVE) then SendMessage(nil, wg, MSG_DEACTIVATE, 0, 0, 0)
                                            else SendMessage(nil, wg, MSG_ACTIVATE, 0, 0, 0);
    end;

    WM_NCACTIVATE:
    begin
      if (GfxTopModalForm <> nil) then
      begin
        if (wParam = 0) and (GfxTopModalForm = wg) then
        begin
          blockmsg := true;
        end
        else if (wParam <> 0) and (GfxTopModalForm <> wg) then
        begin
          blockmsg := true;
        end;
      end;

      if (PopupListFirst <> nil) and (PopupListFirst.Visible) then BlockMsg := True;

      //writeln('ncactivate: ', ord(BlockMsg));

      if not BlockMsg then
        Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);

    end;


    WM_CLOSE:  SendMessage(nil, wg, MSG_CLOSE, wParam, lParam, 0);

    WM_PAINT:  SendMessage(nil, wg, MSG_PAINT, wParam, lParam, 0);

  else
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
end;

{$else}{$endif}

procedure GfxInternalInit;
begin
  FFontResourceList := TList.Create;
  GfxImageLibrary := TStringList.Create;
  InitClipboard;

  GfxCreateStandardImages;
end;

function GfxOpenDisplay(DisplayName : string) : boolean;
{$ifdef Win32}
var
  h : HWND;
  r : TRECT;
begin
  // The lptk uses several writelines that we redirect to nul if {$APPTYPE GUI}
  {$I-}
  Writeln('LPTK-Win32');
  if ioresult <> 0 then
  begin
    Assign(output,'nul');
    rewrite(output);
  end;
  {$I+}

  display := Windows.GetDC(0);

  h := GetDesktopWindow;
  GetWindowRect(h, r);

  ScreenWidth  := r.Right - r.Left;
  ScreenHeight := r.Bottom - r.Top;

  //Writeln('Screen resolution: ',ScreenWidth,'x',ScreenHeight);

  with WindowClass do
  begin
    style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc := WndProc(@GfxWindowProc);
    hInstance := MainInstance;
    hIcon := LoadIcon(0, IDI_APPLICATION);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := 0; //COLOR_WINDOW;
    lpszClassName := 'LPTKWIN';
  end;
  Windows.RegisterClass( {$ifdef FPC}@{$endif} WindowClass);

  with WidgetClass do
  begin
    style := CS_OWNDC or CS_DBLCLKS;
    lpfnWndProc := WndProc(@GfxWindowProc);
    hInstance := MainInstance;
    hIcon := 0;
    hCursor := 0;
    hbrBackground := 0; //COLOR_BACKGROUND;
    lpszClassName := 'LPTKWIDGET';
  end;
  Windows.RegisterClass( {$ifdef FPC}@{$endif} WidgetClass);

  hcr_default := Windows.LoadCursor(0, IDC_ARROW);
  hcr_dir_ew  := Windows.LoadCursor(0, IDC_SIZEWE);
  hcr_dir_ns  := LoadCursor(0, IDC_SIZENS);
  hcr_edit    := LoadCursor(0, IDC_IBEAM);

  hcr_dir_nwse := LoadCursor(0, IDC_SIZENWSE);
  hcr_DIR_NESW := LoadCursor(0, IDC_SIZENESW);

  hcr_MOVE     := LoadCursor(0, IDC_SIZEALL);

  hcr_CROSSHAIR := LoadCursor(0, IDC_CROSS);

  GfxInternalInit;

  if pos('-HIDECONSOLE',UpperCase(CmdLine)) > 0 then GfxHideConsoleWindow;

  result := true;
end;
{$else}
var
  wa : TXWindowAttributes;
begin
  Result := false;

  if Display <> nil then GfxCloseDisplay;
  Display := XOpenDisplay(PChar(@DisplayName));

  if Display = nil then Exit;

  GfxDefaultScreen := XDefaultScreen(Display);
  GfxRootWindow := XRootWindow(Display, GfxDefaultScreen);
  DefaultBackground := XBlackPixel(Display, GfxDefaultScreen);
  DefaultForeground := XWhitePixel(Display, GfxDefaultScreen);

  GfxDefaultVisual := XDefaultVisual(display, GfxDefaultScreen);
  DisplayDepth := XDefaultDepth(display, GfxDefaultScreen);

  GfxDefaultColorMap := XDefaultColorMap(display, GfxDefaultScreen);

  XGetWindowAttributes(display, GfxRootWindow, @wa);
  ScreenWidth  := wa.width;
  ScreenHeight := wa.height;

  // Initialize atoms
  xia_clipboard := XInternAtom(display, 'CLIPBOARD', longbool(0));
  xia_targets := XInternAtom(display, 'TARGETS', longbool(0));
  xia_motif_wm_hints := XInternAtom(display, '_MOTIF_WM_HINTS', longbool(0));
  xia_wm_protocols     := XInternAtom(display, 'WM_PROTOCOLS', longbool(0));
  xia_wm_delete_window := XInternAtom(display, 'WM_DELETE_WINDOW', longbool(0));

  xia_wm_state := XInternAtom(display, '_NET_WM_STATE', longbool(0));
  xia_wm_state_modal := XInternAtom(display, '_NET_WM_STATE_MODAL', longbool(0));

  //writeln('modal=',xia_wm_modal);

//  dpisX := (XDisplayWidth(display,DefaultScreen) * 254 + XDisplayWidthMM(display,DefaultScreen)*5)
//                  / (XDisplayWidthMM(display,DefaultScreen)*10);

//  dpisY := (XDisplayHeight(display,DefaultScreen) * 254 + XDisplayHeightMM(display,DefaultScreen)*5)
//                  / (XDisplayHeightMM(display,DefaultScreen)*10);


  // for correct keyboard handling
  InputMethod := XOpenIM(Display,nil,nil,nil);
  if InputMethod = nil then Exit;

  InputContext := XCreateIC(InputMethod, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0 ] );
  //InputContext := XCreateIC(im, [XNInputStyle, XIMPreeditNothing or XIMStatusNothing, XNClientWindow, win, 0 ] );
  if InputContext = nil then Exit;
  //XGetICValues(ic, [XNFilterEvents, @lw, nil]);

  GfxInternalInit;

  result := True;
end;
{$endif}

procedure GfxCloseDisplay;
begin
{$ifdef Win32}{$else}
  if Display <> nil then
  begin
    XCloseDisplay(Display);
    Display := nil;
  end;
{$endif}
end;


{$ifdef Win32}

procedure WaitWindowMessageWin;
var
  Msg: TMsg;
begin
  // Some Win98 hack
  if (GetVersion() < $80000000) then Windows.GetMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0)   //NT
                                else Windows.GetMessage( {$ifdef FPC}@{$endif} Msg, 0, 0, 0);   //Win98

  if Windows.TranslateMessage( {$ifdef FPC}@{$endif} msg) then
  begin
    //Windows.GetMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0);
    //Msg.message := MSG_KEYPRESS;
  end;
{
    Writeln('Message: ',msg.message,' wp=',IntToHex(msg.wparam,4),
      ' lp=',intToHex(msg.lparam,8),
      ' px=',msg.pt.x, ' py=',msg.pt.y );
}
  Windows.DispatchMessage( {$ifdef FPC}@{$endif} msg);

  DeliverMessages;
end;

{$else}

procedure ProcessSelection(var ev : TXEvent);
var
  s : string;
  actual : TAtom;
  format : integer;
  count, remaining : longword;
  data : PChar;
begin
  //Writeln('selection notify: ', ev.xselection.requestor, ',', ev.xselection._property);

  if ev.xselection._property > 0 then
  begin
    XGetWindowProperty(display, ev.xselection.requestor, ev.xselection._property,
  		      0, 16000,
                        false, // delete
                        0, // type
                        @actual, @format, @count, @remaining,
                        @data);
    s := data;

  //  Writeln('actual=',actual,' format=',format,' count=',count,' remaining=',remaining);
  //  Writeln('data="',s,'"');

    ClipBoardData := s;

    XFree(data);
  end
  else
  begin
    ClipBoardData := '';
  end;

  WaitingForSelection := false;
end;

procedure ProcessSelectionRequest(var ev : TXEvent);
var
  e : TXSelectionEvent;
  a : TAtom;
begin

  e._type := SelectionNotify;
  e.requestor := ev.xselectionrequest.requestor;
  e.selection := ev.xselectionrequest.selection;
  e.selection := xia_clipboard;
  e.target := ev.xselectionrequest.target;
  e.time := ev.xselectionrequest.time;
  e._property := ev.xselectionrequest._property;

//  Writeln('Selection request. selection=',ev.xselectionrequest.selection,' target=',ev.xselectionrequest.target);

  if e.target = xia_targets then
  begin
    a := XA_STRING;
    XChangeProperty(display, e.requestor, e._property,
		      XA_ATOM, sizeof(TAtom)*8, 0, PByte(@a), sizeof(TAtom)  );
  end
  else
  begin
    XChangeProperty(display, e.requestor, e._property, e.target, 8, 0,
          PByte(@ClipBoardData[1]), length(ClipBoardData)  );
  end;

  XSendEvent(display, e.requestor, false, 0, @e );

end;

function GetParentWindow(wh : TWinHandle; var pw, rw : TWinHandle) : boolean;
var
  rootw,
  parentw : TWinHandle;
  childs : ^TWinHandle;
  cnum : longword;
begin
  childs := nil;
  if XQueryTree(display, wh, @rootw, @parentw, @childs, @cnum) <> 0 then
  begin
    pw := parentw;
    rw := rootw;
    result := true;
  end
  else result := false;
  if childs <> nil then XFree(childs);
end;

function GetDecorationWindow(wh : TWinHandle) : TWinHandle;
var
  lpw, pw, rw : TWinHandle;
  bok : boolean;
begin
  pw := wh;
  repeat
    lpw := pw;
    bok := GetParentWindow(lpw, pw, rw);
  until (not bok) or (pw = rw);
  if bok then result := lpw else result := 0;
end;

procedure WaitWindowMessageX;
var
  ev : TXEvent;
  n,i,r,i2 : integer;
  wg, ewg : TWidget;
  ks : integer;
  uc : word;
  a : array[1..16] of char;
  ss, sr : integer;
  p : PChar;
  blockmsg : boolean;

  Popup : TWidget;
  frm : TGfxForm;

  wh : TWinHandle;
  wa : TXWindowAttributes;
  px,py : integer;

begin
  repeat
    XNextEvent(display, @ev);
  until (not XFilterEvent(@ev,0));


  blockmsg := false;

  Popup := PopupListFirst;

  //WriteLn('Event ',n,': ', ev._type,' window: ', ev.xany.window);

  case ev._type of

    MSG_KEYPRESS, MSG_KEYRELEASE:
    begin
      sr := 0;
      ss := ev.xkey.state;

      { i know this is a rough hack but...
        ...otherwise XmbLookupString doesn't work how i want it to work }

      n := PXKeyPressedEvent(@ev)^._type;
      PXKeyPressedEvent(@ev)^._type := MSG_KEYPRESS;
      r := XmbLookupString(InputContext, PXKeyPressedEvent(@ev), @a, 16, @ks, @sr);
      PXKeyPressedEvent(@ev)^._type := n;

      uc := ks and $FFFF;
      KeySymToUnicode(ks, @uc);

      //Writeln('XKey event: ',ev.xkey.keycode,', shift=',IntToHex(ss,4),' keysym=',IntToHex(ks,4),' unicode=',IntToHex(uc,4));

      wg := FindKeyboardFocus;
      if wg <> nil then PostMessage(nil, wg, ev.xkey._type, uc, ev.xkey.state, 0 )
		   else PostMessage(nil, FindWidget(ev.xkey.window), ev.xkey._type, uc, ev.xkey.state, 0 );
    end;

    MSG_MOUSEDOWN, MSG_MOUSEUP:
    begin

      if (Popup <> nil) then
      begin
        wg := FindWidget(ev.xbutton.window);
        ewg := wg;
        while (wg <> nil) and (wg.Parent <> nil) do wg := wg.Parent;

        if (wg <> nil) and (PopupListFind(wg.WinHandle) = nil) and (not PopupDontCloseWidget(ewg)) then
        begin
          ClosePopups;
          Popup := nil;
          PostMessage(nil, ewg, MSG_POPUPCLOSE, 0, 0, 0 );
          //blockmsg := true;
        end;
      end;

      ewg := FindWidget(ev.xbutton.window);

      if GfxTopModalForm <> nil then
      begin
        wg := WidgetParentForm(ewg);
        if (wg <> nil) and (GfxTopModalForm <> wg) then blockmsg := true;
      end;

      if not blockmsg then
      begin

        if (ev.xbutton.button >= 4) and (ev.xbutton.button <= 7) then  // mouse wheel
        begin
          // generate scroll events:
          if ev._type = MSG_MOUSEDOWN then
          begin
            if ev.xbutton.button > 5 then i := 1 else i := 3;  // amount
            PostMessage(nil, ewg, MSG_SCROLL, ev.xbutton.button mod 4, i, ev.xbutton.state );
          end;
        end
        else
        begin
          PostMessage(nil, ewg, ev._type, ev.xbutton.x, ev.xbutton.y,
             (ev.xbutton.state and $FF) or ((ev.xbutton.button and $FF) shl 8) );
             
          // doubleclick check
          if (ev.xbutton.button = 1) then
          begin
            if (ev._type = MSG_MOUSEDOWN) then
            begin
              if (ev.xbutton.window = LastClickWindow) and ((ev.xbutton.time - LastWinClickTime) < DOUBLECLICK_MS) then
              begin
                //Writeln('doubleclick');
                PostMessage(nil, ewg, MSG_DOUBLECLICK, ev.xbutton.x, ev.xbutton.y,
                   (ev.xbutton.state and $FF) or ((ev.xbutton.button and $FF) shl 8) );

              end;
              //Writeln('button time: ',ev.xbutton.time);

              LastWinClickTime := ev.xbutton.time;
              LastClickWindow  := ev.xbutton.window;
            end;
          end;

        end;

      end;
    end;

    MSG_PAINT:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xany.window, MSG_PAINT, @ev);
      PostMessage(nil, FindWidget(ev.xany.window), ev._type, 0,0,0);
    end;

    MSG_MOUSEMOVE:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xbutton.window, MSG_MOUSEMOVE, @ev);

      if GfxTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWidget(ev.xbutton.window));
        if (wg <> nil) and (GfxTopModalForm <> wg) then blockmsg := true;
      end;

      //Writeln('Motion: x=',ev.xmotion.x,' y=',ev.xmotion.y,'  st=',ev.xmotion.state);

      if not blockmsg then
        PostMessage(nil, FindWidget(ev.xbutton.window), ev._type, ev.xmotion.x, ev.xmotion.y, ev.xmotion.state);

    end;

    // message blockings for modal windows
    MSG_CLOSE:
    begin
      if GfxTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWidget(ev.xbutton.window));
        if (wg <> nil) and (GfxTopModalForm <> wg) then blockmsg := true;
      end;

      if not blockmsg then PostMessage(nil, FindWidget(ev.xany.window), ev._type, 0,0,0);

    end;

    ConfigureNotify:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xany.window, ConfigureNotify, @ev);
      
      wg := FindWidget(ev.xconfigure.window);
      if (wg <> nil) and (wg is TGfxForm) then
      begin
        frm := TGfxForm(wg);
        if (frm.width <> ev.xconfigure.width) or (frm.height <> ev.xconfigure.height) then
        begin
          PostMessage(nil, wg, MSG_RESIZE, ev.xconfigure.width - wg.Width, ev.xconfigure.height - wg.Height, 0);
        end;

        wh := GetDecorationWindow(ev.xconfigure.window);
        
        if wh > 0 then
        begin
          XGetWindowAttributes(display, wh, @wa);
          // Writeln('parent: ',wh);
          //writeln('form: ',frm.Name);
          //writeln('parent x:',wa.x,' y:',wa.y);
          
          px := wa.x;
          py := wa.y;
        end
        else
        begin
          px := ev.xconfigure.x;
          py := ev.xconfigure.y;
        end;

        if (frm.Left <> px) or (frm.Top <> py) then
        begin
          PostMessage(nil, wg, MSG_MOVE, px, py, 0);
        end;
      end;
    end;

    SelectionNotify:
    begin
      ProcessSelection(ev);
    end;

    SelectionRequest:
    begin
      ProcessSelectionRequest(ev);
    end;

  else
    PostMessage(nil, FindWidget(ev.xany.window), ev._type, 0,0,0);
  end;

end;

function ConvertTo565Pixel(rgb : longword) : word;
begin
  result := (rgb and $F8) shr 3;
  result := result or ((rgb and $FC00) shr 5);
  result := result or ((rgb and $F80000) shr 8);
end;

function GfxColorToX(col : TGfxColor) : longword;
var
  xc : TXColor;
  c : TGfxColor;
begin
  if (col and $80000000) <> 0 then
  begin
    // named color
    c := guistyle.GetNamedColorRGB(col);
  end
  else c := col;

  if DisplayDepth >= 24 then
  begin
    result := c;
  end
  else if DisplayDepth = 16 then
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
    XAllocColor(display, GfxDefaultColorMap, @xc);
    result := xc.pixel;
  end;
end;

procedure SetXftColor(col : TGfxColor; var colxft : TXftColor);
var
  c : TGfxColor;
begin
  c := GfxColorToRGB(col);

  colxft.color.blue  := (c and $000000FF) shl 8;
  colxft.color.green := (c and $0000FF00);
  colxft.color.red   := (c and $00FF0000) shr 8;

  colxft.color.alpha := (c and $7F000000) shr 15;
  colxft.color.alpha := colxft.color.alpha xor $FFFF;  // invert: 0 in GfxColor means not translucent

  colxft.pixel := 0;
end;

procedure GfxSetWMOptions(wh : TWinHandle; aflags, afunctions, adecorations, ainputmode : longword);
var
  mhints : array[0..4] of longword;
begin
  mhints[0] := aflags;  // flags
  mhints[1] := afunctions;  // functions
  mhints[2] := adecorations;  // decorations
  mhints[3] := ainputmode;  // inputmode
  mhints[4] := 0;  // ??? dummy

{
  if wmNoBorder in wmo then
  begin
    mhints[0] := mhints[0] or 2;
    mhints[2] := 0;
  end;
}

  XChangeProperty(display, wh, xia_motif_wm_hints, xia_motif_wm_hints, 32, 0, @mhints, 4);
end;

{$endif}

procedure GfxProcessMessages;
{$ifdef Win32}
var
  Msg: TMsg;
{$endif}
begin
{$ifdef Win32}
  GdiFlush;
  while Windows.PeekMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0, PM_NOREMOVE) do
  begin
    WaitWindowMessageWin;
    GdiFlush;
  end;
{$else}
  XFlush(display);
  while XPending(Display) > 0 do
  begin
    WaitWindowMessageX;
    DeliverMessages;
    XFlush(display);
  end;
{$endif}
end;

procedure WaitWindowMessage;
begin
{$ifdef Win32}
    WaitWindowMessageWin;
{$else}
    WaitWindowMessageX;
{$endif}
end;

procedure GfxDoMessageLoop;
begin
  repeat
{$ifdef Win32}
    WaitWindowMessageWin;
{$else}
    WaitWindowMessageX;
    DeliverMessages;
{$endif}
  until false;
end;


{$ifdef Win32}

function WinOpenFont(desc : string) : TGfxFontData;
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
//  Writeln('GfxGetFont(''',desc,''')');

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
    lf.lfHeight := -MulDiv(StrToIntDef(token,0), GetDeviceCaps(display, LOGPIXELSY), 72);
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

{$endif}

function GfxGetFont(desc : string) : TGfxFont;
var
  fnt : TGfxFontData;
  fr : TGfxFontResource;
  n : integer;
  fdesc : string;
begin
  fdesc := desc;
  if copy(fdesc,1,1)='#' then fdesc := guistyle.GetNamedFontDesc(copy(desc,2,length(desc)));
  
  for n := 0 to FFontResourceList.Count-1 do
  begin
    if TGfxFontResource(FFontResourceList[n]).FontDesc = fdesc then
    begin
      fr := TGfxFontResource(FFontResourceList[n]);
      inc(fr.FRefCount);
      //Writeln(fr.FRefCount,': ',fr.FontDesc);
      result := TGfxFont.Create(fr, desc);
      Exit;
    end;
  end;

{$ifdef Win32}
  fnt := WinOpenFont(fdesc);
{$else}
  fnt := XftFontOpenName(display, GfxDefaultScreen, PChar(fdesc) );
{$endif}

  if {$ifdef Win32}fnt <> 0{$else}fnt <> nil{$endif} then
  begin
    fr := TGfxFontResource.Create(fnt, fdesc);
    FFontResourceList.Add(fr);
    Result := TGfxFont.Create(fr, desc);
  end
  else
  begin
    writeln('error opening font.');
    Result := nil;
  end;
end;

{$ifdef Win32}

function MyFontEnumerator(var LogFont: ENUMLOGFONTEX; var TextMetric: {$ifndef FPC}NEWTEXTMETRICEXA{$else}NEWTEXTMETRICEX{$endif};
  FontType: Integer; data: LPARAM): Integer; stdcall;
var
  sl : TStringList;
  s : string;
begin
  sl := TStringList(data);
  s := LogFont.elfLogFont.lfFaceName;
  if ((sl.Count = 0) or (sl.Strings[sl.Count-1] <> s)) then sl.Add(s);
  Result := 1;
end;

function GfxGetFontFaceList : TStringList;
var
  LFont: TLogFont;
begin
  result := TStringList.Create;
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(display, {$ifdef FPC}@{$endif}LFont, @MyFontEnumerator, LongInt(result), 0);
  result.Sort;
end;

{$else}

function GfxGetFontFaceList : TStringList;
var
  pfs : PFcFontSet;
  ppat : PPFcPattern;
  n : integer;
  s : string;
  pc : PChar;
  fl : TStringList;
begin
  pfs := XftListFonts(display, GfxDefaultScreen,
    [FC_SCALABLE, FcTypeBool, 1, 0,
    FC_FAMILY,0
    ]);

  if pfs = nil then Exit;

  result := TStringList.Create;

  GetMem(pc,128);

  n := 0;
  ppat := pfs^.fonts;

  while n < pfs^.nfont do
  begin
    XftNameUnparse(ppat^,pc,127);  //FtNameUnparse does not free the name string!
    s := pc;
    result.Add(s);
    inc(PChar(ppat),sizeof(pointer));
    inc(n);
  end;

  FreeMem(pc);
  FcFontSetDestroy(pfs);

  result.Sort;
end;

{$endif}


{ TGfxCanvas }

constructor TGfxCanvas.Create(winhandle : TWinHandle);
{$ifdef Win32}
var
  ARect : TgfxRect;
begin
  FWin := winhandle;
  FDrawOnBuffer := False;
  Fgc := windows.GetDC(FWin);
  SetTextAlign(Fgc, TA_TOP); //TA_BASELINE);
  SetBkMode(Fgc, TRANSPARENT);
  SetFont(guistyle.DefaultFont);
  {$IFDEF BUFFERING}
  FBufferGC := 0;
  FBufferBitmap := 0;
  FBufferClipRegion := 0;
  FBufferPen := 0;
  FBufferBrush := 0;
  GetWinRect(ARect);
  _ReCreateBuffer(ARect.Width, ARect.Height);
  {$ENDIF}
  FColor := clText1;
  FLineStyle := PS_SOLID;
  FLineWidth := 0;
  FBackgroundColor := clBoxColor;
  FBrush := CreateSolidBrush(0);
  FPen := CreatePen(PS_SOLID, 0, 0);
  SetColor(clText1);
  SetTextColor(clText1);

  FClipRegion := CreateRectRgn(0,0,1,1);
end;
{$else}
var
  GcValues : TXGcValues;
  rw : TWinHandle;
  x,y : integer;
  bw,d : longword;
  event_base, error_base : Integer;
begin
  FWin := winhandle;
  FDrawOnBuffer := False;
  FBackgroundColor := clBoxColor;
  Fgc := XCreateGc(display, FWin, 0, @GcValues);
  FCurFont := guistyle.DefaultFont;
  SetTextColor(clText1);

  FLineStyle := LineSolid;
  FLineWidth := 0;

//  FWinRect.Top := 0;
//  FWinRect.Left := 0;
//  XGetGeometry(display, FWin, @rw, @x,@y, @(FWinRect.width), @(FWinRect.height), @bw, @d);
  {$IFDEF BUFFERING}
  FBufferWin := XdbeAllocateBackBufferName(Display, FWin,nil);
  if FBufferWin > 0 then
     FXftDrawBuffer := XftDrawCreate(display, FBufferWin, XDefaultVisual(display, GfxDefaultScreen), XDefaultColormap(display, GfxDefaultScreen));
  {$ELSE}
         FBufferWin := -1;
         FXftDrawBuffer := nil;
  {$ENDIF}
  FXftDraw := XftDrawCreate(display, FWin, XDefaultVisual(display, GfxDefaultScreen), XDefaultColormap(display, GfxDefaultScreen));
  FClipRegion := XCreateRegion;
end;
{$endif}

procedure TgfxCanvas.SetDrawOnBuffer(AValue : Boolean);
begin
     {$IFDEF BUFFERING}
     if AValue <> FDrawOnBuffer then
     begin
        {$IFDEF win32}
          FDrawOnBuffer := AValue and (FBufferGC > 0);
        {$ELSE}
          FDrawOnBuffer := AValue and (FBufferWin > 0);
        {$ENDIF}
     end;
     {$ELSE}
            FDrawOnBuffer := False;
     {$ENDIF}
end;

{$IFDEF win32}
procedure TgfxCanvas._ReCreateBuffer(AWidth, AHeight : Integer);
begin
  {$IFDEF BUFFERING}
    gfxFlush;
    if FBufferBrush > 0 then DeleteObject(FBufferBrush);
    FBufferBrush := 0;
    if FBufferPen > 0 then DeleteObject(FBufferPen);
    FBufferPen := 0;
    if FBufferBitmap > 0 then DeleteObject(FBufferBitmap);
    FBufferBitmap := 0;
    if FBufferClipRegion > 0 then DeleteObject(FBufferClipRegion);
    FBufferClipRegion := 0;
    if FBufferGC > 0 then DeleteDC(FBufferGC);
    FBufferGC := 0;
    FBufferGC := CreateCompatibleDC(Fgc);
    if FBufferGC > 0 then
    begin
      FBufferBitmap := windows.CreateCompatibleBitmap(Fgc,AWidth, AHeight);
      SelectObject(FBufferGC,FBufferBitmap);
      SetTextAlign(FBufferGC, TA_TOP);
      SetBkMode(FBufferGC, TRANSPARENT);

      FBufferBrush := CreateSolidBrush(0);
      FBufferPen := CreatePen(PS_SOLID, 0, 0);
      FBufferClipRegion := CreateRectRgn(0,0,1,1);
      SelectObject(FBufferGC, FCurFont.Handle);
      SelectObject(FBufferGC,FBufferBrush);
      SelectObject(FBufferGC,FBufferPen);
      //SelectObject(FBufferGC, FCurFont.Handle);
  end;
  {$ENDIF}
end;
{$ENDIF}

procedure TgfxCanvas.MoveResizeWindow(x,y,w,h : TGfxCoord);
var
  rwidth, rheight : integer;
{$ifdef Win32}
  ws,es : integer;
  r : TRect;
{$endif}
begin
  if FWin <= 0 then Exit;

  rwidth := w;
  rheight := h;
  
  if rwidth < 1 then rwidth := 1;
  if rheight < 1 then rheight := 1;
  
{$ifdef Win32}
  // windows decoration correction on stupid windows...
  ws := GetWindowLong(FWin, GWL_STYLE);
  es := GetWindowLong(FWin, GWL_EXSTYLE);

  if (ws and WS_CHILD) = 0 then
  begin
    r.Left := x;
    r.Top  := y;
    r.Right := x + rwidth;
    r.Bottom := y + rheight;
    AdjustWindowRectEx(r, ws, false, es);
    rwidth := r.Right - r.Left;
    rheight := r.Bottom - r.Top;
  end;
  windows.MoveWindow(FWin, x,y, rwidth, rheight, true);
  _ReCreateBuffer(rwidth + 1,rheight + 1);
{$else}
  if FXftDrawBuffer <> nil then
  begin
     XftDrawDestroy(FXftDrawBuffer);
     FXftDrawBuffer := nil;
  end;
  {$IFDEF BUFFERING}
  if FBufferWin > 0 then
     XdbeDeallocateBackBufferName(Display, FBufferWin);
  {$ENDIF}
  XMoveResizeWindow(display, FWin, x,y,rwidth,rheight);
  {$IFDEF BUFFERING}
  FBufferWin := XdbeAllocateBackBufferName(Display, FWin,nil);
  {$ENDIF}
  if FBufferWin > 0 then
     FXftDrawBuffer := XftDrawCreate(display, FBufferWin, XDefaultVisual(display, GfxDefaultScreen), XDefaultColormap(display, GfxDefaultScreen));
{$endif}
end;

procedure TGfxCanvas.SwapBuffer;
{$IFDEF win32}
var
  ARect : TgfxRect;
begin
  GetWinRect(ARect);
  BitBlt(Fgc, 0,0, ARect.Width+1, ARect.Height+1, FBufferGC, 0, 0, SRCCOPY);
//  _ReCreateBuffer(ARect.width, ARect.Height);
end;
{$ELSE}
{$IFDEF BUFFERING}
var
   SwapInfo : TXdbeSwapInfo;
   TmpWinHandle : TWinHandle;
begin
     SwapInfo.Window := FWin;
     SwapInfo.SwapAction := nil;
     XdbeSwapBuffers(Display, @SwapInfo,1);
end;
{$ELSE}
begin
     // Do Nothing if buffering is deactivated
end;
{$ENDIF}
{$ENDIF}

destructor TGfxCanvas.Destroy;
begin
{$ifdef Win32}
  DeleteObject(FBrush);
  DeleteObject(FPen);
  if FBufferBrush > 0 then DeleteObject(FBufferBrush);
  FBufferBrush := 0;
  if FBufferPen > 0 then DeleteObject(FBufferPen);
  FBufferPen := 0;
  if FBufferBitmap > 0 then DeleteObject(FBufferBitmap);
  FBufferBitmap := 0;
  if FBufferClipRegion > 0 then DeleteObject(FBufferClipRegion);
  FBufferClipRegion := 0;
  if FBufferGC > 0 then DeleteDC(FBufferGC);
  FBufferGC := 0;

  Windows.ReleaseDC(FWin, Fgc);
  DeleteObject(FClipRegion);
{$else}
  XDestroyRegion(FClipRegion);
  XFreeGc(display, Fgc);
  if FXftDraw <> nil then XftDrawDestroy(FXftDraw);
  {$IFDEF BUFFERING}
  if FXftDrawBuffer <> nil then XftDrawDestroy(FXftDrawBuffer);
  if FBufferWin > 0 then XdbeDeallocateBackBufferName(Display,FBufferWin);
  {$ENDIF}
{$endif}
  inherited Destroy;
end;

procedure TGfxCanvas.SetFont(fnt : TGfxFont);
begin
  if fnt = nil then Exit;
  FCurFont := fnt;
{$ifdef Win32}
  Windows.SelectObject(Fgc, FCurFont.Handle);
  {$IFDEF BUFFERING}
  if FBufferGC > 0 then
      Windows.SelectObject(FBufferGC, FCurFont.Handle);
  {$ENDIF}
{$else}
  //XSetFont(display, Fgc, fnt.Handle);
{$endif}
end;

procedure TGfxCanvas.SetTextColor(cl : TGfxColor);
begin
  FColorText := cl;
{$ifdef Win32}
  {$IFDEF BUFFERING}
  Windows.SetTextColor(FBufferGC, GfxColorToWin(cl));
  {$ENDIF}
  Windows.SetTextColor(Fgc, GfxColorToWin(cl));
{$else}
  SetXftColor(cl,FColorTextXft);
{$endif}
end;

procedure TGfxCanvas.SetLineStyle(width : integer; dashed : boolean);
begin
  FLineWidth := width;
{$ifdef Win32}
  DeleteObject(FPen);
  if dashed then FLineStyle := PS_DASH else FLineStyle := PS_SOLID;
  FPen := CreatePen(FLineStyle, FLineWidth, FWindowsColor);
  SelectObject(Fgc,FPen);
  {$IFDEF BUFFERING}
  DeleteObject(FBufferPen);
  FBufferPen := CreatePen(FLineStyle, FLineWidth, FWindowsColor);  
  SelectObject(FBufferGC,FBufferPen);
  {$ENDIF}
{$else}
  if dashed then FLineStyle := LineOnOffDash else FLineStyle := LineSolid;
  XSetLineAttributes(display, Fgc, FLineWidth, FLineStyle, 3, 0);
{$endif}
end;

procedure TGfxCanvas.SetColor(cl: TGfxColor);
begin
{$ifdef Win32}
  DeleteObject(FBrush);
  DeleteObject(FPen);

  FWindowsColor := GfxColorToWin(cl);

  FBrush := CreateSolidBrush(FWindowsColor);
  FPen := CreatePen(FLineStyle, FLineWidth, FWindowsColor);
  SelectObject(Fgc,FBrush);
  SelectObject(Fgc,FPen);
  {$IFDEF BUFFERING}
  DeleteObject(FBufferBrush);
  DeleteObject(FBufferPen);
  FBufferBrush := CreateSolidBrush(FWindowsColor);
  FBufferPen := CreatePen(FLineStyle, FLineWidth, FWindowsColor);
  SelectObject(FBufferGC,FBufferBrush);
  SelectObject(FBufferGC,FBufferPen);
  {$ENDIF}
{$else}
  XSetForeGround(display, Fgc, GfxColorToX(cl) );
{$endif}
end;

procedure TGfxCanvas.DrawString16(x, y : TGfxCoord; txt : String16);
begin
  if length(txt) < 1 then exit;
{$ifdef Win32}
  if DrawOnBuffer then
    windows.TextOutW(FBufferGC, x,y{+FCurFont.Ascent}, @txt[1], length16(txt))
  else
    windows.TextOutW(Fgc, x,y{+FCurFont.Ascent}, @txt[1], length16(txt));
{$else}
  if DrawOnBuffer then
     XftDrawString16(FXftDrawBuffer, FColorTextXft, FCurFont.Handle, x,y+FCurFont.Ascent, @txt[1], Length16(txt) )
  else
     XftDrawString16(FXftDraw, FColorTextXft, FCurFont.Handle, x,y+FCurFont.Ascent, @txt[1], Length16(txt) )
{$endif}
end;

procedure TGfxCanvas.FillRectangle(x, y, w, h : TGfxCoord);
{$ifdef Win32}
var
  wr : windows.TRect;
begin
  wr.Left := x;
  wr.Top  := y;
  wr.Right := x + w;
  wr.Bottom := y + h;
  if DrawOnBuffer then
    Windows.FillRect(FBufferGC, wr, FBufferBrush)
  else
    Windows.FillRect(Fgc, wr, FBrush);
{$else}
begin
  //XSetFunction(display, Fgc, GXinvert);
  if DrawOnBuffer then
    XFillRectangle(display, FBufferWin, Fgc, x,y, w, h)
  else
    XFillRectangle(display, Fwin, Fgc, x,y, w, h);
{$endif}
end;

procedure TGfxCanvas.FillRect(r: TGfxRect);
{$ifdef Win32}
var
  wr : windows.TRect;
begin
  if (r.Height <= 0) or (r.Width <= 0) then Exit;
  
  wr.Left := r.Left;
  wr.Top  := r.Top;
  wr.Right := r.left + r.Width;
  wr.Bottom := r.Top + r.height;

  if DrawOnBuffer then
    Windows.FillRect(FBufferGC, wr, FBufferBrush)
  else
    Windows.FillRect(Fgc, wr, FBrush);
{$else}
begin
  if (r.Height <= 0) or (r.Width <= 0) then Exit;
  
     if DrawOnBuffer then
        XFillRectangle(display, FBufferWin, Fgc, r.Left, r.Top, r.Width, r.Height)
     else
        XFillRectangle(display, Fwin, Fgc, r.Left, r.Top, r.Width, r.Height);
{$endif}
end;

procedure TGfxCanvas.FillTriangle(x1, y1, x2, y2, x3, y3: TGfxCoord);
{$ifdef Win32}
var
  pts : array[1..3] of windows.TPoint;
begin
  pts[1].X := x1; pts[1].Y := y1;
  pts[2].X := x2; pts[2].Y := y2;
  pts[3].X := x3; pts[3].Y := y3;
  if DrawOnBuffer then
    Polygon(FBuffergc, pts, 3)
  else
    Polygon(Fgc, pts, 3);
{$else}
var
  pts : array[1..3] of TXPoint;
begin
  pts[1].x := x1; pts[1].y := y1;
  pts[2].x := x2; pts[2].y := y2;
  pts[3].x := x3; pts[3].y := y3;

  //XSetFillRule(display, Fgc, 0);
  if DrawOnBuffer then
     XFillPolygon(display, FBufferWin, Fgc, @pts, 3, 0,0)
  else
      XFillPolygon(display, FWin, Fgc, @pts, 3, 0,0);
{$endif}
end;

procedure TGfxCanvas.DrawRectangle(x, y, w, h: TGfxCoord);
{$ifdef Win32}
var
  wr : windows.TRect;
begin
  wr.Left := x;
  wr.Top  := y;
  wr.Right := x + w;
  wr.Bottom := y + h;
  if DrawOnBuffer then
    Windows.FrameRect(FBufferGC, wr, FBufferBrush)
  else
    Windows.FrameRect(Fgc, wr, FBrush);
{$else}
begin
  if DrawOnBuffer then
     XDrawRectangle(display, FBufferWin, Fgc, x,y,w-1,h-1)
  else
     XDrawRectangle(display, Fwin, Fgc, x,y,w-1,h-1);   // transformed into polyline requests!
{$endif}
end;

procedure TGfxCanvas.DrawRect(r : TGfxRect);
{$ifdef Win32}
var
  wr : windows.TRect;
begin
  wr.Left := r.left;
  wr.Top  := r.top;
  wr.Right := r.left + r.width;
  wr.Bottom := r.Top + r.Height;
  if DrawOnBuffer then
    Windows.FrameRect(FBufferGC, wr, FBufferBrush)
  else
    Windows.FrameRect(Fgc, wr, FBrush);
{$else}
begin
  if DrawOnBuffer then
     XDrawRectangle(display, FBufferWin, Fgc, r.left,r.top,r.width-1,r.height-1)
  else
      XDrawRectangle(display, FWin, Fgc, r.left,r.top,r.width-1,r.height-1);   // transformed into polyline requests!
{$endif}
end;

procedure TGfxCanvas.DrawLine(x1, y1, x2, y2 : TGfxCoord);
{$ifdef Win32}
var
  pts : array[1..2] of windows.TPoint;
begin
  pts[1].X := x1; pts[1].Y := y1;
  pts[2].X := x2; pts[2].Y := y2;
  if DrawOnBuffer then
  begin
    PolyLine(FBufferGc, pts, 2);
    SetPixel(FBufferGc, x2,y2, FWindowsColor);
  end
  else
  begin
    PolyLine(Fgc, pts, 2);
    SetPixel(Fgc, x2,y2, FWindowsColor);
  end;
{$else}
begin
     if DrawOnBuffer then
             XDrawLine(display, FBufferwin, Fgc, x1,y1,x2,y2 )
     else
            XDrawLine(display, Fwin, Fgc, x1,y1,x2,y2 );
{$endif}
end;

procedure TGfxCanvas.DrawSelectionRectangle(x, y, w, h : TGfxCoord);
{$ifdef Win32}
var
  wr : windows.TRect;
//  hb : HBRUSH;
begin
  wr.Left := x;
  wr.Top  := y;
  wr.Right := x + w + 1;
  wr.Bottom := y + h;
  if DrawOnBuffer then
    Windows.InvertRect(FBufferGC, wr)
  else
    Windows.InvertRect(Fgc, wr)
{
  hb := CreateSolidBrush(GfxColorToWin(GfxColorToRGB(clSelection) xor $00FFFFFF));
  SetROP2(Fgc, R2_XORPEN);
  Windows.FillRect(Fgc, wr, hb);
  SetROP2(Fgc, R2_COPYPEN);
  DeleteObject(hb);
}
{$else}
begin
  XSetForeGround(display, Fgc, GfxColorToX(GfxColorToRGB(clSelection) xor $00FFFFFF));
  XSetFunction(display, Fgc, GXxor);
  if DrawOnBuffer then
     XFillRectangle(display, FBufferWin, Fgc, x,y, w, h)
  else
      XFillRectangle(display, Fwin, Fgc, x,y, w, h);
  XSetForeGround(display, Fgc, 0);
  XSetFunction(display, Fgc, GXcopy);
{$endif}
end;

procedure TGfxCanvas.SetClipRect(const rect : TGfxRect);
{$ifdef Win32}
begin
  if DrawOnBuffer then
  begin
    FBufferClipRect := Rect;
    FBufferClipRectSet := True;
    DeleteObject(FBufferClipRegion);
    FBufferClipRegion := CreateRectRgn(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height);
    SelectClipRgn(FBufferGC, FBufferClipRegion);
  end
  else
  begin
    FClipRectSet := True;
    FClipRect := rect;
    DeleteObject(FClipRegion);
    FClipRegion := CreateRectRgn(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height);
    SelectClipRgn(Fgc, FClipRegion);
  end;
end;
{$else}
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
  XSetRegion(display, Fgc, FClipRegion);
  if DrawOnBuffer then
  begin
     FBufferClipRect := Rect;
     FBufferClipRectSet := True;
     XftDrawSetClip(FXftDrawBuffer, FClipRegion)
  end
  else
  begin
    XftDrawSetClip(FXftDraw, FClipRegion);
    FClipRect := rect;
    FClipRectSet := True;
  end;
  XDestroyRegion(rg);
end;
{$endif}

function TgfxCanvas.GetClipRect : TgfxRect;
// added by aegluke
begin
  if DrawOnBuffer then
     result := FBufferClipRect
  else
    result := FClipRect;
end;

procedure TGfxCanvas.AddClipRect(const rect: TGfxRect);
{$ifdef Win32}
var
  rg : HRGN;
begin
  rg := CreateRectRgn(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height);
  if DrawOnBuffer then
  begin
    FBufferClipRect := Rect;
    FBufferClipRectSet := True;
    CombineRgn(FBufferClipRegion,rg,FBufferClipRegion,RGN_AND);
    SelectClipRgn(FBufferGC,FBufferClipRegion);
  end
  else
  begin
    FClipRect := Rect;
    FClipRectSet := True;
    CombineRgn(FClipRegion,rg,FClipRegion,RGN_AND);
    SelectClipRgn(Fgc, FClipRegion);
  end;
  DeleteObject(rg);
end;
{$else}
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
  XSetRegion(display, Fgc, FClipRegion);
  if DrawOnBuffer then
  begin
     FBufferClipRect := Rect;
     FBufferClipRectSet := True;
     XftDrawSetClip(FXftDrawBuffer, FClipRegion)
  end
  else
  begin
     FClipRect := Rect;
     FClipRectSet := True;
     XftDrawSetClip(FXftDraw, FClipRegion);
  end;
  XDestroyRegion(rg);
end;
{$endif}

procedure TGfxCanvas.ClearClipRect;
{$ifdef Win32}
begin
    if DrawOnBuffer then
    begin
	SelectClipRgn(FBuffergc, 0);
	FBufferClipRectSet := False;
    end
    else
    begin
	SelectClipRgn(Fgc, 0);
	FClipRectSet := False;
    end;
end;
{$else}
var
  r : TGfxRect;
begin
  GetWinRect(r);
  SetClipRect(r);
  if DrawOnBuffer then FBufferClipRectSet := False
  else FClipRectSet := False;
end;
{$endif}

procedure TGfxCanvas.GetWinRect(var r: TGfxRect);
{$ifdef Win32}
var
  wr : windows.TRECT;
begin
  GetClientRect(FWin,wr);
  r.top := wr.Top;
  r.left := wr.Left;
  r.width := wr.Right - wr.Left + 1;
  r.height := wr.Bottom - wr.Top + 1;
end;
{$else}
var
  rw : TWinHandle;
  x,y : integer;
  bw,d : longword;
begin
  r.left := 0;
  r.Top := 0;
  if DrawOnBuffer then
     XGetGeometry(display, FBufferWin, @rw, @x, @y, @(r.width), @(r.height), @bw, @d)
  else
      XGetGeometry(display, FWin, @rw, @x, @y, @(r.width), @(r.height), @bw, @d);
end;
{$endif}

procedure TGfxCanvas.Clear(col : TGfxColor);
{$ifdef Win32}
var
  r : windows.TRECT;
  br : HBRUSH;
begin
  GetClientRect(FWin,r);
  inc(r.Bottom,10);
  br := CreateSolidBrush(GfxColorToWin(col));
  if DrawOnBuffer then
    windows.FillRect(FBufferGC, r, br)
  else
    windows.FillRect(Fgc, r, br);
  DeleteObject(br);
end;
{$else}
var
  ACol : TgfxColor;
  AWinRect : TgfxRect;
begin
  ACol := FColor;
  SetColor(col);
  GetWinRect(AWinRect);
  FillRectangle(0,0,AWinRect.Width,AWinRect.Height);
  SetColor(ACol);
end;
{$endif}

procedure TGfxCanvas.DrawImage(x, y: TGfxCoord; img: TGfxImage);
begin
  if img = nil then exit;
  DrawImagePart(x,y,img,0,0,img.width,img.height);
end;

procedure TGfxCanvas.DrawImagePart(x, y: TGfxCoord; img: TGfxImage; xi, yi, w,h: integer);
{$ifdef Win32}
const
  DSTCOPY = $00AA0029;
  ROP_DSPDxax = $00E20746;
var
  tmpdc : HDC;
  dstrop : longword;
{$else}
var
  msk : TPixmap;
  gc2,drawgc : Tgc;
  GcValues : TXGcValues;
{$endif}
  ARect : TgfxRect;
  AInt : integer;
begin
  {$IFNDEF win32}
     // added by aegluke - only for linux needed
     if img = nil then exit;
     if (DrawOnBuffer and FBufferClipRectSet) or (not DrawOnBuffer and FClipRectSet) then
     begin
      ARect := GetClipRect;
      if ARect.top > y then
      begin
        AInt := ARect.top - y;
        y := ARect.top;
        yi := yi + AInt;
        h := h - AInt;
      end;
      if h < 0 then exit;
      if ARect.Left > x then
      begin
          AInt := ARect.Left - x;
          x := ARect.Left;
          xi := xi + AInt;
          w := w - AInt;
      end;
      if w < 0 then exit;
      if x + w > ARect.Right then
        w := ARect.Right - x ;
      if w < 0 then exit;
      if y + h > ARect.Bottom then
        h := ARect.Bottom - y;
      if h < 0 then exit;
     end;
     {$ENDIF}
     
{$ifdef Win32}
  tmpdc := CreateCompatibleDC(display);

  SelectObject(tmpdc, img.BMPHandle);

  dstrop := SRCCOPY;
  if img.ColorDepth = 1 then
  begin
    dstrop := ROP_DSPDxax;
  end;
  if DrawOnBuffer then
  begin
    if img.Masked then
    begin
      MaskBlt(FBufferGC, x,y, w, h, tmpdc, xi, yi, img.MaskHandle, xi, yi, MakeRop4(dstrop, DSTCOPY));
    end
    else BitBlt(FBufferGC, x,y, w, h, tmpdc, xi, yi, dstrop);
  end
  else
  begin
    if img.Masked then
    begin
      MaskBlt(Fgc, x,y, w, h, tmpdc, xi, yi, img.MaskHandle, xi, yi, MakeRop4(dstrop, DSTCOPY));
    end
    else BitBlt(Fgc, x,y, w, h, tmpdc, xi, yi, dstrop);
  end;

  DeleteDC(tmpdc);
{$else}
  if img.Masked then
  begin
    // rendering the mask

    msk := XCreatePixmap(display, XDefaultRootWindow(display), img.width, img.height, 1);
    GcValues.foreground := 1;
    GcValues.background := 0;
    gc2 := XCreateGc(display, msk, GCForeground or GCBackground, @GcValues);
    
    // clear mask
    XSetForeground(display, gc2, 0);
    XFillRectangle(display, msk, gc2, 0,0, img.width, img.height);
    
    XOffsetRegion(FClipRegion, -x, -y);
    XSetRegion(display, gc2, FClipRegion);
    XOffsetRegion(FClipRegion, x, y);

    XSetForeground(display, gc2, 1);
    XPutImage(display, msk, gc2, img.XImageMask, xi,yi, 0,0, w, h);

    drawgc := XCreateGc(display, FWin, 0, @GcValues);
    
    XSetClipMask(display, drawgc, msk);
    XSetClipOrigin(display, drawgc, x,y);

    if DrawOnBuffer
       then XPutImage(display, FBufferwin, drawgc, img.XImage, xi,yi, x,y, w, h)
       else XPutImage(display, Fwin, drawgc, img.XImage, xi,yi, x,y, w, h);
      
    XFreePixmap(display, msk);
    XFreeGc(display,drawgc);
    XFreeGc(display,gc2);
  end
  else
  begin
    if DrawOnBuffer
      then XPutImage(display, FBufferWin, Fgc, img.XImage, xi,yi, x,y, w, h)
      else XPutImage(display, FWin, Fgc, img.XImage, xi,yi, x,y, w, h);
  end;

{$endif}
//  if FClipRectSet then
//        SetClipRect(FClipRect);
end;

{ TGfxRect }

procedure TGfxRect.SetRect(aleft, atop, awidth, aheight : TGfxCoord);
begin
  left := aleft;
  top  := atop;
  width := awidth;
  height := aheight;
end;

function TGfxRect.bottom : TGfxCoord;
begin
  result := top + height - 1;
end;

function TGfxRect.right : TGfxCoord;
begin
  result := left + width - 1;
end;

procedure TGfxRect.SetBottom(value : TGfxCoord);
begin
  height := value - top + 1;
end;

procedure TGfxRect.SetRight(value : TGfxCoord);
begin
  width := value - left + 1;
end;

{ TGfxFont }

constructor TGfxFont.Create(afont : TGfxFontResource; AllocName : string);
begin
  FFontRes := afont;
  FName := AllocName;
end;

destructor TGfxFont.Destroy;
var
  n : integer;
begin
  if FFontRes.FRefCount > 1 then
  begin
    dec(FFontRes.FRefCount);
  end
  else
  begin
    for n := 0 to FFontResourceList.Count-1 do
    begin
      if FFontResourceList[n] = Pointer(FFontRes) then
      begin
        FFontRes.Free;
        FFontResourceList.Delete(n);
        Exit;
      end;
    end;
  end;
  
  inherited;
end;

function TGfxFont.Handle : TFontHandle;
begin
  result := FFontRes.Handle;
end;

function TGfxFont.TextWidth16(txt : string16) : integer;
var
{$ifdef Win32}
  ts : Windows.SIZE;
{$else}
  extents : TXGlyphInfo;
{$endif}
begin
  if length(txt) < 1 then
  begin
    result := 0;
    exit;
  end;
{$ifdef Win32}
  SelectObject(display, FFontRes.Handle);
  GetTextExtentPoint32W( display, @txt[1], length16(txt), ts);
  result := ts.cx;
{$else}
  XftTextExtents16(display, FFontRes.Handle, @txt[1], Length16(txt), extents);
  result := extents.xOff;
{$endif}
end;

function TGfxFont.Ascent : integer;
begin
{$ifdef Win32}
  result := FFontRes.Metrics.tmAscent;
{$else}
  result := FFontRes.Handle^.ascent;
{$endif}
end;

function TGfxFont.Descent : integer;
begin
{$ifdef Win32}
  result := FFontRes.FMetrics.tmDescent;
{$else}
  result := FFontRes.Handle^.Descent;
{$endif}
end;

function TGfxFont.Height : integer;
begin
{$ifdef Win32}
  result := FFontRes.FMetrics.tmHeight;
{$else}
  result := FFontRes.Handle^.height; //ascent + FFontRes.Handle^.descent;
{$endif}
end;

function TGfxFont.FontDescx: string;
begin
  result := FFontRes.FontDesc;
end;

{ TGfxImage }

constructor TGfxImage.Create;
begin
  FWidth := 0;
  FHeight := 0;
  FColorDepth := 0;

  FImageData := nil;
  FImageDataSize := 0;
  FMaskData := nil;
  FMaskDataSize := 0;
  FMasked := false;

{$ifdef Win32}
  FBMPHandle := 0;
  FMaskHandle := 0;
{$endif}
end;

destructor TGfxImage.Destroy;
begin
  FreeImage;
  inherited Destroy;
end;

procedure TGfxImage.FreeImage;
begin
  if FImageData <> nil then FreeMem(FImageData);
  FImageData := nil;
  FImageDataSize := 0;
  if FMaskData <> nil then FreeMem(FMaskData);
  FMaskData := nil;
  FMaskDataSize := 0;
  FMasked := false;
  FWidth := 0;
  FHeight := 0;

{$ifdef Win32}
  if FBMPHandle > 0 then DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  if FMaskHandle > 0 then DeleteObject(FMaskHandle);
  FMaskHandle := 0;
{$endif}

end;

procedure TgfxImage.LoadFromFile(AFileName : String);
// Added by aegluke
var
    AFile : File of Char;
    AImageData : Pointer;
    AImageDataSize : Longint;
begin
    if not FileExists(AFileName) then
    begin
	raise Exception.Create(Format(CEFileNotExist,[AFileName]));
	exit;
    end;
    AssignFile(AFile,AFileName);
    Reset(AFile);
    AImageDataSize := FileSize(AFile);
    GetMem(AImageData,AImageDataSize);
    BlockRead(AFile,AImageData^,AImageDataSize);
    CloseFile(AFile);
    if AImageData = nil then
    begin
	    raise Exception.Create(CENotEnoughImageMemory);
	    exit;
    end;
    ReadImage_BMP(self, AImageData, AImageDataSize);
    FreeMem(AImageData);
end;

procedure TGfxImage.AllocateRGBImage(awidth, aheight: integer);
begin
  FreeImage;

  FWidth := awidth;
  FHeight := aheight;

{$ifdef Win32}

  FColorDepth := 24;
  FBMPHandle := CreateCompatibleBitmap(display, awidth, aheight);

{$else}

  FColorDepth := DisplayDepth;

  FImageDataSize := FWidth * FHeight * 4;
  GetMem(FImageData, FImageDataSize);

  // Preparing XImage

  with FXimg do
  begin
    width := FWidth;
    height := FHeight;
    xoffset := 0;
    format := ZPixmap;
    byte_order := LSBFirst;
    bitmap_unit := 32;
    bitmap_bit_order := MSBFirst;
    bitmap_pad := 32;
    depth := FColorDepth;
    bytes_per_line := 0;
    bits_per_pixel := 32;

    red_mask :=   $000000FF;
    green_mask := $0000FF00;
    blue_mask :=  $00FF0000;

    obdata := #0;

    data := FImageData;
  end;

  XInitImage(@FXimg);

{$endif}

end;

procedure TGfxImage.Allocate2CImage(awidth, aheight: integer);
var
  dww : integer;
begin
  FreeImage;

  FWidth := awidth;
  FHeight := aheight;

  FColorDepth := 1;

{$ifdef Win32}

  FBMPHandle := CreateCompatibleBitmap(display, awidth, aheight);

{$else}

  // Real bitmap
  dww := awidth div 32;
  if (awidth and $1F) > 0 then inc(dww);

  FImageDataSize := dww * FHeight * 4;
  GetMem(FImageData, FImageDataSize);

  // Preparing XImage

  with FXimg do
  begin
    width := FWidth;
    height := FHeight;
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

    data := FImageData;
  end;

  XInitImage(@FXimg);

{$endif}

end;

procedure TGfxImage.AllocateMask;
var
  dww : integer;
begin
  if (FWidth < 1) or (FHeight < 1) then Exit;

  FMasked := true;

{$ifdef Win32}
  if FMaskHandle > 0 then DeleteObject(FMaskHandle);

  FMaskHandle := CreateBitmap(FWidth, FHeight, 1, 1, nil);

{$else}

  if FMaskData <> nil then FreeMem(FMaskData);

  dww := FWidth div 32;
  if (FWidth and $1F) > 0 then inc(dww);

  FMaskDataSize := dww * FHeight * 4;
  GetMem(FMaskData, FMaskDataSize);

  // Preparing XImage

  with FXimgMask do
  begin
    width := FWidth;
    height := FHeight;
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

    data := FMaskData;
  end;

  XInitImage(@FXimgMask);

{$endif}

end;

procedure TGfxImage.Invert;
var
{$ifdef Win32}
  tmpdc, srcdc : HDC;
  tmpbmp : HBITMAP;
{$else}
  p : ^Byte;
  n : integer;
{$endif}
begin
{$ifdef Win32}
  if FBMPHandle = 0 then Exit;

  writeln('inverting...');

  srcdc := CreateCompatibleDC(display);
  SelectObject(srcdc, FBMPHandle);

  tmpdc := CreateCompatibleDC(srcdc);
  tmpbmp := CreateCompatibleBitmap(srcdc,FWidth,FHeight);
  SelectObject(tmpdc, tmpbmp);

  BitBlt(tmpdc, 0,0, FWidth, FHeight, srcdc, 0, 0, NOTSRCCOPY);
  BitBlt(srcdc, 0,0, FWidth, FHeight, tmpdc, 0, 0, SRCCOPY);
  DeleteObject(tmpbmp);

  if FMaskHandle > 0 then
  begin
    SelectObject(srcdc, FMaskHandle);

    tmpbmp := CreateCompatibleBitmap(srcdc,FWidth,FHeight);
    SelectObject(tmpdc, tmpbmp);

    BitBlt(tmpdc, 0,0, FWidth, FHeight, srcdc, 0, 0, NOTSRCCOPY);
    BitBlt(srcdc, 0,0, FWidth, FHeight, tmpdc, 0, 0, SRCCOPY);
    DeleteObject(tmpbmp);
  end;

  DeleteDC(tmpdc);
  DeleteDC(srcdc);
  
{$else}
  if FImageData = nil then Exit;

  p := FImageData;
  for n:=1 to FImageDataSize do
  begin
    p^ := p^ XOR $FF;
    inc(p);
  end;

  if FMaskData <> nil then
  begin
    p := FMaskData;
    for n:=1 to FMaskDataSize do
    begin
      p^ := p^ XOR $FF;
      inc(p);
    end;
  end;
{$endif}
end;

procedure TGfxImage.CreateMaskFromSample(x, y: integer);
var
{$ifdef Win32}
  tmpdc, srcdc : HDC;
  xc,yc : integer;
  c : longword;
{$else}
  p : ^longword;
  pmsk : ^byte;
  c : longword;

  linecnt : integer;
  bcnt : integer;
  pixelcnt : integer;
  bit : byte;
  msklinelen : integer;
{$endif}
begin
  if FColorDepth = 1 then Exit;

  AllocateMask;

{$ifdef Win32}
  if FBMPHandle = 0 then Exit;

  tmpdc := CreateCompatibleDC(display);
  SelectObject(tmpdc, FMaskHandle);

  srcdc := CreateCompatibleDC(display);
  SelectObject(srcdc, FBMPHandle);

  c := GetPixel(srcdc, x,y);
  //Writeln('Sample color: ',IntToHex(c,8));

  for yc := 0 to FHeight-1 do
  begin
    for xc := 0 to FWidth-1 do
    begin
      if GetPixel(srcdc, xc, yc) = c then
      begin
        SetPixel(tmpdc, xc, yc, 0);
//        write('0');
      end else
      begin
        SetPixel(tmpdc, xc, yc, $FFFFFF);
//        write('1');
      end;
    end;
//    writeln;
  end;

  DeleteDC(tmpdc);
  DeleteDC(srcdc);
{$else}
  if (FImageData = nil) then Exit;

  p := FImageData;
  if x < 0 then inc(p,FWidth-1) else inc(p,x);
  if y < 0 then inc(p,FWidth*(FHeight-1)) else inc(p,FWidth*y);

  c := p^;  // the sample

  msklinelen := FWidth div 32;
  if (FWidth and $1F) > 0 then inc(msklinelen);

  msklinelen := msklinelen shl 2;
  
  p := FImageData;
  linecnt := 0;
  
  repeat
    pixelcnt := 0;
    bit := $80;
    pmsk := FMaskData;
    inc(pmsk, linecnt*msklinelen);

    repeat

      if bit = $80 then pmsk^ := 0;

      if p^ <> c then pmsk^ := pmsk^ or bit;

      inc(p);
      inc(pixelcnt);

      if bit = 1 then
      begin
        bit := $80;
        inc(pmsk);
      end
      else bit := bit shr 1;

    until pixelcnt >= FWidth;

    inc(linecnt);

  until linecnt >= FHeight;

{$endif}

end;

{$ifdef Win32}

procedure TGfxImage.SetWindowsBitmap(pdata, pinfoheader: pointer; startscan, scanlines : longword);
begin
  SetDIBits(display, FBMPHandle, startscan, scanlines, pdata, TBitmapInfo(pinfoheader^), DIB_RGB_COLORS);
  if FColorDepth = 1 then
  begin
    SetDIBits(display, FMaskHandle, startscan, scanlines, pdata, TBitmapInfo(pinfoheader^), DIB_RGB_COLORS);
  end;

//  FBMPHandle := CreateDIBitmap(display, TBitmapInfoHeader(pinfoheader^), CBM_INIT, pdata, TBitmapInfo(pinfoheader^), DIB_RGB_COLORS);
//  Writeln('bmp handle: ',FBMPHandle);
end;

{$else}
function TGfxImage.XImage: PXImage;
begin
  result := @FXimg;
end;

function TGfxImage.XImageMask: PXImage;
begin
  result := @FXimgMask;
end;

{$endif}

function GfxLibGetImage(const imgid : string) : TGfxImage;
var
  i : integer;
begin
  i := GfxImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then result := TGfxImage(GfxImageLibrary.Objects[i])
            else result := nil;
end;

function GfxLibAddBMP(const imgid : string; bmpdata : pointer; bmpsize : integer) : TGfxImage;
begin
  result := CreateBMPImage(bmpdata, bmpsize);
  if result <> nil then GfxLibAddImage(imgid, result);
end;

function GfxLibAddMaskedBMP(const imgid : string; bmpdata : pointer; bmpsize : integer;
           mcx, mcy : integer) : TGfxImage;
begin
  result := GfxLibAddBMP(imgid, bmpdata, bmpsize);
  if result <> nil then result.CreateMaskFromSample(mcx, mcy);
end;

function GfxLibAddImage(const imgid : string; img : TGfxImage) : boolean;
var
  i : integer;
begin
  i := GfxImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then
  begin
    GfxImageLibrary.Strings[i] := UpperCase(imgid);
    GfxImageLibrary.Objects[i] := img;
    result := false;
  end
  else
  begin
    GfxImageLibrary.AddObject(UpperCase(imgid), img);
    result := true;
  end;
end;

function GfxLibDeleteImage(const imgid : string; freeimg : boolean) : boolean;
var
  i : integer;
  img : TGfxImage;
begin
  i := GfxImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then
  begin
    if freeimg then TGfxImage(GfxImageLibrary.Objects[i]).Free;
    GfxImageLibrary.Delete(i);
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

{ TGfxFontResource }

constructor TGfxFontResource.Create(afont: TGfxFontData; aFontDesc: string);
begin
  FFont := afont;
  FFontDesc := aFontDesc;
  FRefCount := 1;
{$ifdef Win32}
  SelectObject(display, afont);
  GetTextMetrics(display, FMetrics);
{$else}{$endif}
end;

destructor TGfxFontResource.Destroy;
begin
{$ifdef Win32}
  Windows.DeleteObject(FFont);
{$else}
  XftFontClose(Display, FFont);
  //XFreeFont(display, FFont);
{$endif}
  inherited;
end;

function TGfxFontResource.Handle: TFontHandle;
begin
  result := FFont;
end;

initialization
begin
  FFontResourceList := nil;
{$ifdef Win32}
  FFocusedWindow := 0;
{$else}
  Display := nil;
  InputMethod  := nil;
  InputContext := nil;
  LastClickWindow  := 0;
  LastWinClickTime := 0;
{$endif}
end;

finalization
begin
{$ifdef Win32}{$else}
  if Display <> nil then GfxCloseDisplay;
{$endif}
end;

end.

