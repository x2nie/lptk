unit lp_main;

// Platform independent code

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils,
  lp_defs,Types,
{$ifdef Win32}
  hd_platform_win
{$endif}
{$ifdef UNIX}
  hd_platform_x11
{$endif}
  ;
  // The special keys, based on the well-known keyboard scan codes
  {$I keys.inc}  
type
  {Transfer from lp_defs}
  TCursor = lp_defs.TCursor;
    PPoint = Types.PPoint;
  TPoint = Types.TPoint;
  PRect = Types.PRect;
  TRect = Types.TRect;


type
  TOrientation = (orVertical, orHorizontal);

  TAlignment  = (taLeft, taRight, taCenter, taJustify);

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);

  TAnchor = (anLeft,anRight,anTop,anBottom);
  TAnchors = set of TAnchor;

  TClipboardKeyType = (ckNone, ckCopy, ckPaste, ckCut);

  TlpMenuItemFlags = set of (mifSelected, mifHasFocus, mifSeparator,
    mifEnabled, mifChecked, mifSubMenu);

  TWidgetStyleType = (
    wsAcceptsChildren,       // can have children in the designer
    //csCaptureMouse,          // auto capture mouse when clicked
    //csDesignInteractive,     // wants mouse events in design mode
    wsClickEvents,           // handles mouse events
    //csFramed,                // not implemented, has 3d frame
    wsSetCaption            // if Name=Caption, changing the Name changes the Caption
    {csOpaque,                // the control paints its area completely
    csDoubleClicks,          // understands mouse double clicks
    csTripleClicks,          // understands mouse triple clicks
    csQuadClicks,            // understands mouse quad clicks
    csFixedWidth,            // cannot change its width
    csFixedHeight,           // cannot change its height (for example combobox)
    csNoDesignVisible,       // is invisible in the designer
    csReplicatable,          // PaintTo works
    csNoStdEvents,           // standard events such as mouse, key, and click events are ignored.
    csDisplayDragImage,      // display images from dragimagelist during drag operation over control
    csReflector,             // not implemented, the controls respond to size, focus and dlg messages - it can be used as ActiveX control under Windows
    csActionClient,          // Action is set
    csMenuEvents,            // not implemented}
    //wsFocusable                // control will take focus when clicked with mouse.
    {csNeedsBorderPaint,      // not implemented
    csParentBackground,      // tells WinXP to paint the theme background of parent on controls background
    csDesignNoSmoothResize,  // when resizing control in the designer do not SetBounds while dragging
    csDesignFixedBounds,     // can not be moved nor resized in designer
    csHasDefaultAction,      // implements useful ExecuteDefaultAction
    csHasCancelAction,       // implements useful ExecuteCancelAction
    csNoDesignSelectable,    // can not be selected at design time
    csOwnedChildrenNotSelectable, // child controls owned by this control are NOT selectable in the designer
    csAutoSize0x0,           // if the preferred size is 0x0 then control is shrinked ot 0x0
    csAutoSizeKeepChildLeft, // when AutoSize=true do not move children horizontally
    csAutoSizeKeepChildTop,  // when AutoSize=true do not move children vertically
    csRequiresKeyboardInput  // If the device has no physical keyboard then show the virtual keyboard when this control gets focus (therefore available only to TWinControl descendents)
    }
    );
  TWidgetStyle = set of TWidgetStyleType;

  { *******************************************
      Public event properties: Event Types
    *******************************************}
  { Keyboard }
  TKeyEvent = procedure(Sender: TObject; AKey: Word; AShift: TShiftState) of object;
  TKeyCharEvent = procedure(Sender: TObject; AKeyChar: Char) of object;
  TKeyPressEvent = procedure(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean) of object;
  { Mouse }
  TMouseButton = (mbLeft, mbRight, mbMiddle);
  TMouseButtonEvent = procedure(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseMoveEvent = procedure(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseWheelEvent = procedure(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint) of object;
  { Painting }
  TPaintEvent = procedure(Sender: TObject{; const ARect: TfpgRect}) of object;
  { Exceptions }
  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;
  THintEvent = procedure(Sender: TObject; var AHint: String) of object;
const
  AllAnchors = [anLeft,anRight,anTop,anBottom];

  crDefault     = TCursor(0);
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
  crSizeAll     = TCursor(-22);
  
type
  TNotifyEvent = procedure(Sender : TObject) of object;

  TKeyPressNotifyEvent = procedure(Sender: TObject; var keycode: word; var shiftstate: word; var consumed : boolean) of object;
  TMouseNotifyEvent    = procedure(Sender: TObject; x,y : TpgfCoord; var button : word; var shiftstate : word) of object;
  
type
  TSizeParams = record
    min_width, max_width,
    min_height, max_height : TpgfCoord;
  end;

type
  TpgfFontResource = class(TpgfFontResourceImpl)
  protected
    FFontDesc : string;
    FRefCount : integer;
  public
    constructor Create(const afontdesc : string);

    function IncRefCount : integer;
    function DecRefCount : integer;

    property FontDesc : string read FFontDesc;
  end;

  TpgfFont = class(TpgfFontBase)
  private
    FFontDesc : string;
    FFontRes  : TpgfFontResource;
  public
    constructor Create(afontres : TpgfFontResource; const afontdesc : string);
    destructor Destroy; override;

    function TextWidth(const txt : widestring) : integer;

    function Ascent  : integer;
    function Descent : integer;
    function Height  : integer;

    property FontDesc : string read FFontDesc;

    property FontRes : TpgfFontResource read FFontRes;
    property Handle  : TpgfFontResource read FFontRes;
  end;

  TpgfCanvas = class;

  { TpgfWindow }

  TpgfWindow = class(TpgfWindowImpl)
  protected
    { these fields are defined in the TpgfWindowBase. They are window handle creating parameters.

    FWindowType : TWindowType;
    FWindowAttributes : TWindowAttributes;
    FTop, FLeft, FWidth, FHeight : TpgfCoord;
    FMinWidth, FMinHeight : TpgfCoord;
    }

    FParentWindow : TpgfWindow;

    FCanvas : TpgfCanvas;

  public
    constructor Create(aowner : TComponent); override;
    destructor Destroy; override;

    procedure AllocateWindowHandle;
    procedure ReleaseWindowHandle;

    procedure UpdateWindowPosition;
    
  public
    property HasHandle : boolean read HandleIsValid;

    property WindowType : TWindowType read FWindowType write FWindowType;
    property WindowAttributes : TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property ParentWindow : TpgfWindow read FParentWindow write FParentWindow;

    property Left   : TpgfCoord read FLeft write FLeft;
    property Top    : TpgfCoord read FTop write FTop;
    property Width  : TpgfCoord read FWidth write FWidth;
    property Height : TpgfCoord read FHeight write FHeight;
    property MinWidth  : TpgfCoord read FMinWidth write FMinWidth;
    property MinHeight : TpgfCoord read FMinHeight write FMinHeight;

    property Canvas : TpgfCanvas read FCanvas;

  public
    function Right  : TpgfCoord;
    function Bottom : TpgfCoord;
  end;

  { TpgfImage }

  TpgfImage = class(TpgfImageImpl)
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

    procedure AllocateImage(acolordepth, awidth, aheight : integer);
    procedure AllocateMask;

    procedure Invert;

    procedure UpdateImage;

    procedure CreateMaskFromSample(x,y : integer);

  public
    property ImageData : pointer read FImageData;
    property ImageDataSize : integer read FImageDataSize;
    property MaskData : pointer read FMaskData;
    property MaskDataSize : integer read FMaskDataSize;

    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property ColorDepth : integer read FColorDepth;
    property Masked : boolean read FMasked;
  end;

  { TpgfImages }

  TpgfImages = class
  private
    FImages : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddImage(const imgid : string; img : TpgfImage) : boolean;
    function DeleteImage(const imgid : string; freeimg : boolean) : boolean;
    function GetImage(const imgid : string) : TpgfImage;

    function AddBMP(const imgid : string; bmpdata : pointer; bmpsize : integer) : TpgfImage;
    function AddMaskedBMP(const imgid : string; bmpdata : pointer; bmpsize : integer; mcx, mcy : integer) : TpgfImage;
    
    procedure ListImages(var sl : TStringList);
  end;

  { TpgfCanvas }

  TpgfCanvas = class(TpgfCanvasImpl)
  private
    FColorText: TpgfColor;
  protected
    FBufferedDraw : boolean;
    FPersistentResources : boolean;
    
    FWindow : TpgfWindow;

    FColor : TpgfColor;
    FTextColor : TpgfColor;
    FFont : TpgfFont;
    
    FLineWidth : integer;
    FLineStyle : TpgfLineStyle;
    
    FBeginDrawCount : integer;

  public
    constructor Create(awin : TpgfWindow); reintroduce;
    destructor Destroy; override;
    
    procedure BeginDraw; overload;
    procedure BeginDraw(abuffered : boolean); overload;
    procedure EndDraw(x,y,w,h : TpgfCoord); overload;
    procedure EndDraw; overload;
    procedure FreeResources;

    procedure SetFont(fnt : TpgfFont);
    procedure SetColor(cl : TpgfColor);
    procedure SetTextColor(cl : TpgfColor);
    procedure SetLineStyle(awidth : integer; astyle : TpgfLineStyle);

    procedure DrawString(x,y : TpgfCoord; const txt : widestring);
    
    procedure Clear(col : TpgfColor);
    procedure GetWinRect(var r : TpgfRect);

    procedure FillRectangle(x,y, w,h : TpgfCoord);
    procedure FillRect(r : TpgfRect);
    procedure XORFillRectangle(col : TpgfColor; x, y, w, h : TpgfCoord);
    procedure XORFillRect(col : TpgfColor; r: TpgfRect);

    procedure FillTriangle(x1,y1, x2,y2, x3,y3 : TpgfCoord);

    procedure DrawRectangle(x,y, w,h : TpgfCoord);
    procedure DrawRect(r : TpgfRect);
    
    procedure DrawArc(x,y, w,h : TpgfCoord; a1, a2 : double);
    procedure FillArc(x,y, w,h : TpgfCoord; a1, a2 : double);

    procedure DrawLine(x1,y1,x2,y2 : TpgfCoord);

    procedure SetClipRect(const rect : TpgfRect);
    function GetClipRect : TpgfRect;
    procedure AddClipRect(const rect : TpgfRect);
    procedure ClearClipRect;

    procedure DrawImage(x,y : TpgfCoord; img : TpgfImage);
    procedure DrawImagePart(x,y : TpgfCoord; img : TpgfImage; xi,yi,w,h : integer);

  public

    procedure DrawButtonFace(x,y,w,h : TpgfCoord);
    procedure DrawControlFrame( x, y, w, h : TpgfCoord);
    procedure DrawDirectionArrow(x,y,w,h : TpgfCoord; direction : integer);

  public

    property Font : TpgfFont read FFont write SetFont;
    property Color : TpgfColor read FColor;
    property TextColor : TpgfColor read FColorText;
  end;
  
  TpgfStyle = class(TlpComponent)
  public
    DefaultFont : TpgfFont;

    MenuFont,
    MenuAccelFont,
    MenuDisabledFont : TpgfFont;
  public
    // style initialization
    constructor Create(AOwner: TComponent); override;

    procedure DrawButtonFace(canvas : TpgfCanvas; x,y,w,h : TpgfCoord); virtual;
    procedure DrawControlFrame(canvas : TpgfCanvas; x, y, w, h : TpgfCoord); virtual;
    procedure DrawDirectionArrow(canvas : TpgfCanvas; x,y,w,h : TpgfCoord; direction : integer); virtual;
    procedure   DrawString(ACanvas: TpgfCanvas; x, y: TpgfCoord; AText: string; AEnabled: boolean = True); virtual;

    { Menus }
    procedure   DrawMenuBar(ACanvas: TpgfCanvas; r: TpgfRect; ABackgroundColor: TpgfColor); virtual;
    procedure   DrawMenuRow(ACanvas: TpgfCanvas; r: TpgfRect; AFlags: TlpMenuItemFlags); virtual;
    procedure   DrawMenuItem(ACanvas: TpgfCanvas; r: TpgfRect; AFlags: TlpMenuItemFlags; AText: WideString); virtual;
    procedure   DrawMenuItemSeparator(ACanvas: TpgfCanvas; r: TpgfRect); virtual;
    procedure   DrawMenuItemImage(ACanvas: TpgfCanvas; x, y: TpgfCoord; r: TpgfRect; AFlags: TlpMenuItemFlags); virtual;
    function    GetSeparatorSize: integer; virtual;
  end;

  TpgfDisplay = class(TpgfDisplayImpl)
  protected
    FDisplayParams : string;

    FScreenWidth, FScreenHeight : integer;

    FDefaultFont : TpgfFont;

    FFontResList : TList;
    procedure FreeFontRes(afontres : TpgfFontResource);

  public
    constructor Create(const aparams : string); override;

    function GetFont(const afontdesc : string) : TpgfFont;

  public
    procedure Flush;

    procedure ProcessMessages;
    procedure WaitWindowMessage(atimeoutms : integer);

    procedure RunMessageLoop;

  public
    property Initialized : boolean read FInitialized;
    property ScreenWidth : integer read FScreenWidth;
    property ScreenHeight : integer read FScreenHeight;

  public
    property DefaultFont : TpgfFont read FDefaultFont;
  end;

type
  TlpTimer = class(TlpComponent)
  private
    FEnabled : boolean;
    FNextAlarm : TDateTime;
    FInterval: integer;
    FOnTimer: TNotifyEvent;
    procedure SetEnabled(const AValue: boolean);
  protected
    procedure DoOnTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CheckAlarm(ctime : TDateTime);
    property NextAlarm : TDateTime read FNextAlarm;
  published


    property Interval : integer read FInterval write FInterval ;
    property Enabled : boolean read FEnabled write SetEnabled;
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
  end;

type

  { TpgfCaret }

  TpgfCaret = class(TComponent)
  private
    FEnabled : boolean;
    FVisible : boolean;
    FInterval : integer;
    FCanvas  : TpgfCanvas;
    FTop, FLeft, FWidth, FHeight : TpgfCoord;
    FTimer : TlpTimer;

    procedure OnTimerTime(sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure SetCaret(acanvas : TpgfCanvas; x,y,w,h : TpgfCoord);
    procedure UnSetCaret(acanvas : TpgfCanvas);
    procedure InvertCaret;
  end;

  { TApplication }

  TApplication = class(TComponent)
  public
    procedure Initialize;
    procedure Run;
    procedure CreateForm(InstanceClass: TComponentClass; out Reference);
  end;

var
  pgfDisp   : TpgfDisplay;
  pgfStyle  : TpgfStyle;
  pgfCaret  : TpgfCaret;
  pgfImages : TpgfImages;
  pgfDesigning : boolean;

function pgfOpenDisplay(const aparams : string) : boolean;
procedure pgfCloseDisplay;

procedure pgfProcessMessages;
procedure pgfWaitWindowMessage;
procedure pgfRunMessageLoop;

function pgfGetFont(const afontdesc : string) : TpgfFont;


//-----------------------------------------------------------------------------
// Keys and shortcuts

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt, mkcMeta);

var
  MenuKeyCaps: array[TMenuKeyCap] of string;
  MenuKeyCapsInited: boolean = false;

const
  // TShortCut additions:
    scMeta = $1000;

{ ShortCut }
function  KeyToShortCut(const Key: Word; const Shift: TShiftState): TShortCut;
procedure ShortCutToKey(const ShortCut : TShortCut; out Key: Word; out Shift : TShiftState);
function ShortCutToText(const ShortCut: TShortCut): string ; overload;
function ShortCutToText(const Key: Word; const Shift: TShiftState): string ; overload;
//function ShortCutToText(Key: Word; Shift : TShiftState ): string; overload;

function TextToShortCut(const ShortCutText: string): TShortCut;
//function KeycodeToText(AKey: Word; AShiftState: TShiftState): string;
procedure InitializeMenuKeyCaps;
function GetSpecialShortCutName(ShortCut: TShortCut): string;
procedure GetAccelKey(AText : Widestring; out AAccelKey: Word; out AAccelPos: Word);

// internal message queue

const
  pgfMessageQueueSize = 512;

procedure pgfPostMessage(Sender, Dest : TObject; MsgCode : integer; var aparams : TpgfMessageParams); overload;
procedure pgfPostMessage(Sender, Dest : TObject; MsgCode : integer); overload;
procedure pgfSendMessage(Sender, Dest : TObject; MsgCode : integer; var aparams : TpgfMessageParams); overload;
procedure pgfSendMessage(Sender, Dest : TObject; MsgCode : integer); overload;

procedure pgfDeliverMessage(var msg : TpgfMessageRec);
procedure pgfDeliverMessages;
function pgfGetFirstMessage : PpgfMessageRec;
procedure pgfDeleteFirstMessage;

function pgfColorToRGB(col : TpgfColor) : TpgfColor;
function pgfGetNamedColor(col : TpgfColor) : TpgfColor;
procedure pgfSetNamedColor(colorid, rgbvalue : longword);
function pgfGetNamedFontDesc(afontid : string) : string;
procedure pgfSetNamedFont(afontid, afontdesc : string);

function utf8(const utf8str : string) : widestring;
function u8(const utf8str : string) : widestring;
function wsToUtf8(const wstr : widestring) : string;

procedure pgfInitTimers;
procedure pgfCheckTimers;
function pgfClosestTimer(ctime : TDateTime; amaxtime : integer) : integer;
procedure GetFontDescValues(Proc: TGetStrProc);
function Application: TApplication;

{ Points }
function  PtInRect(const ARect: TpgfRect; const APoint: TPoint): Boolean;
function  InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean; overload;
function InflateRect(var Rect: TpgfRect; dx: Integer; dy: Integer): Boolean; overload;

implementation

uses
  hd_imgfmt_bmp,
  hd_stdimages,
  lp_constants,
  lp_form;

var
  pgfCaretCanvas : TpgfCanvas;
  pgfCaretTimer  : TlpTimer;
  pgfApplication : TApplication; //simulation

var
  pgfTimers : TList;

  pgfNamedColors : array[0..255] of TpgfColor;
  pgfNamedFonts : TList;

type
  TNamedFontItem = class
  public
    FontID : string;
    FontDesc : string;
    constructor Create(AFontID, AFontDesc : string);
  end;


//LCLType.KeyToShortCut(Key,Shift);
function KeyToShortCut(const Key: Word; const Shift: TShiftState): TShortCut;
begin
  Result := Key;
  if (Result and $FF00) <> 0 then begin
    Result:=0;
    exit;
  end;

  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
  if ssMeta in Shift then Inc(Result,scMeta);
end;

procedure ShortCutToKey(const ShortCut: TShortCut; out Key: Word;
  out Shift : TShiftState);
begin
  Key := ShortCut and $FF;
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift,ssShift);
  if ShortCut and scAlt <> 0 then Include(Shift,ssAlt);
  if ShortCut and scCtrl <> 0 then Include(Shift,ssCtrl);
  if ShortCut and scMeta <> 0 then Include(Shift,ssMeta);
end;

function ShortCutToText(const Key: Word; const Shift: TShiftState): string ;
var
  Name: string;
  //Key: Byte;
begin
  InitializeMenuKeyCaps;
  //Key := ShortCut and $FF;
  case Key of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + Key - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + Key - $2D)];
    $30..$39: Name := Chr(Key - $30 + Ord('0'));
    $41..$5A: Name := Chr(Key - $41 + Ord('A'));
    $60..$69: Name := Chr(Key - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(Key - $6F);
  //else
//    Name := GetSpecialShortCutName(ShortCut );
  end;
  if Name <> '' then
  begin
    Result := '';
    if ssShift in Shift then Result := Result + MenuKeyCaps[mkcShift];
    if ssCtrl in Shift then Result := Result + MenuKeyCaps[mkcCtrl];
    if ssMeta in Shift then Result := Result + MenuKeyCaps[mkcMeta];
    if ssAlt in Shift then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;
function ShortCutToText(const ShortCut: TShortCut): string ;
var
  Name: string;
  Key: Byte;
begin
  InitializeMenuKeyCaps;
  Key := ShortCut and $FF;
  case Key of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + Key - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + Key - $2D)];
    $30..$39: Name := Chr(Key - $30 + Ord('0'));
    $41..$5A: Name := Chr(Key - $41 + Ord('A'));
    $60..$69: Name := Chr(Key - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(Key - $6F);
  else
    Name := GetSpecialShortCutName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scMeta <> 0 then Result := Result + MenuKeyCaps[mkcMeta];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;


procedure InitializeMenuKeyCaps;
begin
  if MenuKeyCapsInited=false then
  begin
    MenuKeyCaps[mkcBkSp]:=rsKeyBkSp;
    MenuKeyCaps[mkcTab]:=rsKeyTab;
    MenuKeyCaps[mkcEsc]:=rsKeyEsc;
    MenuKeyCaps[mkcEnter]:=rsKeyEnter;
    MenuKeyCaps[mkcSpace]:=rsKeySpace;
    MenuKeyCaps[mkcPgUp]:=rsKeyPgUp;
    MenuKeyCaps[mkcPgDn]:=rsKeyPgDn;
    MenuKeyCaps[mkcEnd]:=rsKeyEnd;
    MenuKeyCaps[mkcHome]:=rsKeyHome;
    MenuKeyCaps[mkcLeft]:=rsKeyLeft;
    MenuKeyCaps[mkcUp]:=rsKeyUp;
    MenuKeyCaps[mkcRight]:=rsKeyRight;
    MenuKeyCaps[mkcDown]:=rsKeyDown;
    MenuKeyCaps[mkcIns]:=rsKeyIns;
    MenuKeyCaps[mkcDel]:=rsKeyDel;
    MenuKeyCaps[mkcShift]:=rsKeyShift;
    MenuKeyCaps[mkcCtrl]:=rsKeyCtrl;
    MenuKeyCaps[mkcAlt]:=rsKeyAlt;
    MenuKeyCaps[mkcMeta]:=rsKeyMeta;
    MenuKeyCapsInited:=true;
  end;
end;

function GetSpecialShortCutName(ShortCut: TShortCut): string;
begin
  // ToDo
  Result := '';
end;

procedure GetAccelKey(AText : Widestring; out AAccelKey: Word; out AAccelPos: Word);
var
  i,Z : integer;
  C : Char;
  w : widestring;
begin
  AAccelKey := 0;
  AAccelPos := 0;
  Z := Length(AText);
  for i := 1 to z do
  begin
    if AText[i] = '&' then
    begin
      if (i < Z) and not (AText[i+1] in ['&',' ']) then
      begin
        w := AText[i+1];
        w := UpperCase(w);
        AAccelKey := ord(w[1]);
        AAccelPos := i + 1;
        Exit;
      end;
    end;
  end;
end;

function TextToShortCut(const ShortCutText: string): TShortCut;

  function CompareFront(var StartPos: integer; const Front: string): Boolean;
  begin
    if (Front<>'') and (StartPos+length(Front)-1<=length(ShortCutText))
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front))= 0)
    then begin
      Result:=true;
      inc(StartPos,length(Front));
    end else
      Result:=false;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  StartPos: integer;
  Name: string;
begin
  Result := 0;
  Shift := 0;
  StartPos:=1;
  InitializeMenuKeyCaps;
  while True do
  begin
    if CompareFront(StartPos, MenuKeyCaps[mkcShift]) then
      Shift := Shift or scShift
    else if CompareFront(StartPos, '^') then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcCtrl]) then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcAlt]) then
      Shift := Shift or scAlt
    else if CompareFront(StartPos, MenuKeyCaps[mkcMeta]) then
      Shift := Shift or scMeta
    else
      Break;
  end;
  if ShortCutText = '' then Exit;
  for Key := $08 to $FF do begin // Copy range from table in ShortCutToText
    Name:=ShortCutToText(Key);
    if (Name<>'') and (length(Name)=length(ShortCutText)-StartPos+1)
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0)
    then begin
      Result := Key or Shift;
      Exit;
    end;
  end;
end;

(*
function KeycodeToText(AKey: Word; AShiftState: TShiftState): string;

  function GetASCIIText: String;
  var
    c: Char;
  begin
    result := '';
    c := Chr(AKey and $ff);
    case c of
      #13:  Result := Result + rsKeyEnter;
      #127: Result := Result + rsKeyDel;
      else
        Result := Result + c;
    end;
  end;

var
  s: String;
begin
  SetLength(Result, 0);

  { The order of these three are imprortant - don't change them }
  if ssCtrl in AShiftState then
    Result := Result + rsKeyCtrl;
  if ssAlt in AShiftState then
    Result := Result + rsKeyAlt;
  if ssShift in AShiftState then
    Result := Result + rsKeyShift;
  if ssMeta in AShiftState then
    Result := Result + rskeyMeta;

  if (AKey > Ord(' ')) and (AKey < 255) then
  begin
    Result := Result + GetASCIIText;
    Exit; //==>
  end;

  case AKey of
    keyNul:           s := 'Null';
    keyBackSpace:     s := rsKeyBksp;
    keyTab:           s := rsKeyTab;
    keyLinefeed:      s := 'Linefeed';
    keyReturn:        s := rsKeyEnter;
    keyEscape:        s := rsKeyEsc;
    Ord(' '):         s := rsKeySpace;
    keyDelete:        s := rsKeyDel;
    keyVoid:          s := 'Void';
    keyBreak:         s := 'Break';
    keyScrollForw:    s := 'ScrollForw';
    keyScrollBack:    s := 'ScrollBack';
    keyBoot:          s := 'Boot';
    keyCompose:       s := 'Compose';
    keySAK:           s := 'SAK';
    keyUndo:          s := 'Undo';
    keyRedo:          s := 'Redo';
    keyMenu:          s := 'Menu';
    keyCancel:        s := 'Cancel';
    keyPrintScreen:   s := 'PrtScr';
    keyExecute:       s := 'Exec';
    keyFind:          s := 'Find';
    keyBegin:         s := 'Begin';
    keyClear:         s := 'Clear';
    keyInsert:        s := rsKeyIns;
    keySelect:        s := 'Select';
    keyMacro:         s := 'Macro';
    keyHelp:          s := 'Help';
    keyDo:            s := 'Do';
    keyPause:         s := 'Pause';
    keySysRq:         s := 'SysRq';
    keyModeSwitch:    s := 'ModeSw';
    keyUp:            s := rsKeyUp;
    keyDown:          s := rsKeyDown;
    keyLeft:          s := rsKeyLeft;
    keyRight:         s := rsKeyRight;
    keyPrior:         s := rsKeyPgUp;
    keyNext:          s := rsKeyPgDn;
    keyHome:          s := rsKeyHome;
    keyEnd:           s := rsKeyEnd;
    keyF0..keyF64:    s := 'F' + IntToStr(AKey - keyF0);
    keyP0..keyP9:     s := 'KP' + Chr(AKey - keyP0 + Ord('0'));
    keyPA..keyPF:     s := 'KP' + Chr(AKey - keyPA + Ord('A'));
    keyPPlus, keyPMinus, keyPSlash, keyPStar, keyPEqual, keyPSeparator,
      keyPDecimal, keyPParenLeft, keyPParenRight, keyPSpace, keyPEnter,
      keyPTab:        s := 'KP' + GetASCIIText;
    keyPPlusMinus:    s := 'KPPlusMinus';
    keyPBegin:        s := 'KPBegin';
    keyPF1..keyPF9:   s := 'KPF' + IntToStr(AKey - keyPF1);
    keyShiftL:        s := 'ShiftL';
    keyShiftR:        s := 'ShiftR';
    keyCtrlL:         s := 'CtrlL';
    keyCtrlR:         s := 'CtrlR';
    keyAltL:          s := 'AltL';
    keyAltR:          s := 'AltR';
    keyMetaL:         s := 'MetaL';
    keyMetaR:         s := 'MetaR';
    keySuperL:        s := 'SuperL';
    keySuperR:        s := 'SuperR';
    keyHyperL:        s := 'HyperL';
    keyHyperR:        s := 'HyperR';
    keyAltGr:         s := 'AltGr';
    keyCaps:          s := 'Caps';
    keyNum:           s := 'Num';
    keyScroll:        s := 'Scroll';
    keyShiftLock:     s := 'ShiftLock';
    keyCtrlLock:      s := 'CtrlLock';
    keyAltLock:       s := 'AltLock';
    keyMetaLock:      s := 'MetaLock';
    keySuperLock:     s := 'SuperLock';
    keyHyperLock:     s := 'HyperLock';
    keyAltGrLock:     s := 'AltGrLock';
    keyCapsLock:      s := 'CapsLock';
    keyNumLock:       s := 'NumLock';
    keyScrollLock:    s := 'ScrollLock';
    keyDeadRing:      s := 'DeadRing';
    keyDeadCaron:     s := 'DeadCaron';
    keyDeadOgonek:    s := 'DeadOgonek';
    keyDeadIota:      s := 'DeadIota';
    keyDeadDoubleAcute:     s := 'DeadDoubleAcute';
    keyDeadBreve:           s := 'DeadBreve';
    keyDeadAboveDot:        s := 'DeadAboveDot';
    keyDeadBelowDot:        s := 'DeadBelowDot';
    keyDeadVoicedSound:     s := 'DeadVoicedSound';
    keyDeadSemiVoicedSound: s := 'DeadSemiVoicedSound';
    keyDeadAcute:           s := 'DeadAcute';
    keyDeadCedilla:         s := 'DeadCedilla';
    keyDeadCircumflex:      s := 'DeadCircumflex';
    keyDeadDiaeresis:       s := 'DeadDiaeresis';
    keyDeadGrave:           s := 'DeadGrave';
    keyDeadTilde:           s := 'DeadTilde';
    keyDeadMacron:          s := 'DeadMacron';

    keyEcuSign:       s := 'Ecu';
    keyColonSign:     s := 'Colon';
    keyCruzeiroSign:  s := 'Cruzeiro';
    keyFFrancSign:    s := 'FFranc';
    keyLiraSign:      s := 'Lira';
    keyMillSign:      s := 'Mill';
    keyNairaSign:     s := 'Naira';
    keyPesetaSign:    s := 'Peseta';
    keyRupeeSign:     s := 'Rupee';
    keyWonSign:       s := 'Won';
    keyNewSheqelSign: s := 'NewShequel';
    keyDongSign:      s := 'Dong';
    keyEuroSign:      s := 'Euro';
  else
    s := '#' + IntToHex(AKey, 4);
  end;
  Result := Result + s;
end;  *)
  
{ TApplication }
function Application: TApplication;
begin
  if pgfApplication = nil then
     pgfApplication := TApplication.Create(nil);
  result := pgfApplication;
end;

procedure TApplication.Initialize;
begin
  pgfOpenDisplay('');
end;

procedure TApplication.Run;
begin
  pgfRunMessageLoop;
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; out Reference
  );
var
  Instance: TComponent;
  ok: boolean;
  //AForm: TForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  ok:=false;
  try
    Instance.Create(Self);
    if Instance is TlpForm then
       if TlpForm(instance).Visible then
           TlpForm(instance).Show;
    ok:=true;
  finally
    if not ok then begin
      TComponent(Reference) := nil;
      //if FCreatingForm=Instance then
        //FCreatingForm:=nil;
    end;
  end;

  {if (Instance is TForm) then
  begin
    AForm := TForm(Instance);
    UpdateMainForm(AForm);
    if FMainForm = AForm then
      AForm.HandleNeeded;
    if AForm.FormStyle = fsSplash then
    begin
      // show the splash form and handle the paint message
      AForm.Show;
      AForm.Invalidate;
      ProcessMessages;
    end;
  end;}
end;

constructor TNamedFontItem.Create(AFontID, AFontDesc: string);
begin
  FontID := AFontID;
  FontDesc := AFontDesc;
end;

{$include pgf_msgqueue.inc}

function utf8(const utf8str : string) : widestring;
begin
  result := UTF8Decode(utf8str);
end;

function u8(const utf8str : string) : widestring;
begin
  result := UTF8Decode(utf8str);
end;

function wsToUtf8(const wstr : widestring) : string;
begin
  result := UTF8Encode(wstr);
end;

// timers

const
  ONE_MILISEC = 1/(24*60*60*1000);

procedure pgfInitTimers;
begin
  if pgfTimers = nil then
  begin
    pgfTimers := TList.Create;
  end;
end;

procedure pgfCheckTimers;
var
  n : integer;
  ctime : TDateTime;
begin
  ctime := now;

  for n:=1 to pgfTimers.Count do
  begin
    TlpTimer(pgfTimers[n-1]).CheckAlarm(ctime);
  end;
end;

function pgfClosestTimer(ctime : TDateTime; amaxtime : integer) : integer;
var
  n : integer;
  t : TlpTimer;
  dt : TDateTime;
begin
  dt := ctime + amaxtime * ONE_MILISEC;

  for n:=1 to pgfTimers.Count do
  begin
    t := TlpTimer(pgfTimers[n-1]);
    if t.Enabled and (t.NextAlarm < dt) then dt := t.NextAlarm;
  end;

  result := trunc(0.5 + (dt - ctime) / ONE_MILISEC);
  if result < 0 then result := 0;
end;

procedure TlpTimer.SetEnabled(const AValue: boolean);
begin
  if not FEnabled and AValue then
  begin
    FNextAlarm := now + interval * ONE_MILISEC;
  end;
  FEnabled := AValue;
end;

constructor TlpTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Finterval := 1000;
  //FOnTimer := nil;
  FEnabled := false;
  pgfTimers.Add(self);
end;

destructor TlpTimer.Destroy;
var
  i : integer;
begin
  i := pgfTimers.IndexOf(self);
  if i >= 0 then pgfTimers.Delete(i);
  inherited Destroy;
end;

procedure TlpTimer.CheckAlarm(ctime: TDateTime);
begin
  if not FEnabled then Exit;

  if FNextAlarm <= ctime then
  begin
    // alarm

    // set the next alarm point
    if interval > 0 then
    begin
      while FNextAlarm <= ctime do
        FNextAlarm := FNextAlarm + interval * ONE_MILISEC;
    end;

    DoOnTimer();
  end;
end;

procedure pgfInternalInit;
begin
  writeln('PGF Interal init.');

  pgfInitTimers;

  pgfNamedFonts := TList.Create;

  pgfStyle := TpgfStyle.Create(nil);

  pgfCaret := TpgfCaret.Create(nil);
  
  pgfImages := TpgfImages.Create;
  pgfCreateStandardImages; // in pgf_stdimages.pas
end;

// STYLE SUPPORT

function pgfColorToRGB(col : TpgfColor) : TpgfColor;
begin
  if (col and $80000000) <> 0 then
  begin
    // named color
    result := pgfNamedColors[col and $FF] or (col and $7F000000);  // keeping alpha
  end
  else result := col;
end;

function pgfGetNamedColor(col : TpgfColor) : TpgfColor;
begin
  result := pgfNamedColors[col and $FF];
end;

procedure pgfSetNamedColor(colorid, rgbvalue : longword);
var
  i : longword;
begin
  if (colorid and $80000000) = 0 then Exit;
  i := colorid and $FF;
  pgfNamedColors[i] := rgbvalue;
end;

function pgfGetNamedFontDesc(afontid : string) : string;
var
  n : integer;
begin
  for n := 0 to pgfNamedFonts.Count-1 do
  begin
    if (lowercase(TNamedFontItem(pgfNamedFonts[n]).FontID) = lowercase(afontid)) then
    begin
      // found
      result := TNamedFontItem(pgfNamedFonts[n]).FontDesc;
      exit;
    end;
  end;
  
  Writeln('GetNamedFontDesc error: "'+afontid+'" is missing. Default is used.');
  result := 'Arial-10';  // default font desc
end;


procedure pgfSetNamedFont(afontid, afontdesc : string);
var
  n : integer;
begin
  n := 0;
  while (n < pgfNamedFonts.Count) and (lowercase(TNamedFontItem(pgfNamedFonts[n]).FontID) <> lowercase(afontid)) do inc(n);

  if n < pgfNamedFonts.Count then
  begin
    // already defined
    TNamedFontItem(pgfNamedFonts[n]).FontDesc := afontdesc;
  end
  else
  begin
    pgfNamedFonts.Add(TNamedFontItem.Create(afontid, afontdesc));
  end;
end;

{
function pgfFindKeyboardFocus : TpgfWidget;
begin
  Result := nil;

  if FocusRoot <> nil then
  begin
    Result := FocusRoot;
    while (Result <> nil) and (result.ActiveWidget <> nil) do result := result.ActiveWidget;
  end;
end;
}


function pgfOpenDisplay(const aparams : string) : boolean;
begin
  if (pgfDisp <> nil) {and pgfDisp.Initialized} then
    exit;

  //writeln('PGF open display...');
  pgfDisp := TpgfDisplay.Create(aparams);
  if pgfDisp.Initialized then
  begin
    result := true;
    pgfInternalInit;
  end
  else
  begin
    result := false;
    writeln('Opening display failed!');
    pgfDisp.Free;
    pgfDisp := nil;
  end;
end;

procedure pgfCloseDisplay;
begin
  if pgfDisp <> nil then
  begin
    pgfDisp.Free;
    pgfDisp := nil;
  end;
end;

procedure pgfProcessMessages;
begin
  pgfDisp.ProcessMessages;
end;

procedure pgfWaitWindowMessage;
begin
  pgfDisp.WaitWindowMessage(0);
end;

procedure pgfRunMessageLoop;
begin
  pgfDisp.RunMessageLoop;
end;

function pgfGetFont(const afontdesc: string): TpgfFont;
begin
  result := pgfDisp.GetFont(afontdesc);
end;

constructor TpgfDisplay.Create(const aparams: string);
begin
  FFontResList := TList.Create;
  FDisplayParams := aparams;

  FScreenWidth := -1;
  FScreenHeight := -1;

  inherited Create(aparams);

  if PlatformInitialized then
  begin
    FScreenWidth := GetScreenWidth;
    FScreenHeight := GetScreenHeight;
  end;

  FDefaultFont := GetFont(PGF_DEFAULT_FONT_DESC);
end;

function TpgfDisplay.GetFont(const afontdesc: string): TpgfFont;
var
  fr : TpgfFontResource;
  n : integer;
  fdesc : string;
begin
  fdesc := afontdesc;
  
  if copy(fdesc,1,1)='#' then fdesc := pgfGetNamedFontDesc(copy(afontdesc,2,length(afontdesc)));

  result := nil;

  for n := 0 to FFontResList.Count-1 do
  begin
    if TpgfFontResource(FFontResList[n]).FontDesc = fdesc then
    begin
      fr := TpgfFontResource(FFontResList[n]);
      inc(fr.FRefCount);
      //Writeln(fr.FRefCount,': ',fr.FontDesc);
      result := TpgfFont.Create(fr, afontdesc);
      Exit;
    end;
  end;

  fr := TpgfFontResource.Create(fdesc);

  if fr.HandleIsValid then
  begin
    FFontResList.Add(fr);
    Result := TpgfFont.Create(fr, afontdesc);
  end
  else
  begin
    fr.Free;
    writeln('error opening font.');
  end;
end;

procedure TpgfDisplay.FreeFontRes(afontres: TpgfFontResource);
var
  n : integer;
begin
  for n := 0 to FFontResList.Count-1 do
  begin
    if FFontResList[n] = Pointer(afontres) then
    begin
      TpgfFontResource(FFontResList[n]).Free;
      FFontResList.Delete(n);
      Exit;
    end;
  end;
end;

procedure TpgfDisplay.Flush;
begin
  DoFlush;
end;

procedure TpgfDisplay.ProcessMessages;
begin
  Flush;
  while DoMessagesPending do
  begin
    WaitWindowMessage(0);
    Flush;
  end;
end;

procedure TpgfDisplay.WaitWindowMessage(atimeoutms : integer);
begin
  DoWaitWindowMessage( pgfClosestTimer(now,atimeoutms) );
  pgfDeliverMessages;
  pgfCheckTimers;
end;

procedure TpgfDisplay.RunMessageLoop;
begin
  repeat
    WaitWindowMessage(1000);
  until false;
end;

{ TpgfFont }

constructor TpgfFont.Create(afontres: TpgfFontResource; const afontdesc: string);
begin
  FFontRes := afontres;
  FFontDesc := afontdesc;
  
  //inherited Create(afontres); // sets the FFontRes

  afontres.IncRefCount;
end;

destructor TpgfFont.Destroy;
begin
  if TpgfFontResource(FFontRes).DecRefCount <= 0 then
  begin
    pgfDisp.FreeFontRes(TpgfFontResource(FFontRes));
  end;

  inherited;
end;

function TpgfFont.TextWidth(const txt: widestring): integer;
begin
  result := FFontRes.GetTextWidth(txt);
end;

function TpgfFont.Ascent: integer;
begin
  result := FFontRes.GetAscent;
end;

function TpgfFont.Descent: integer;
begin
  result := FFontRes.GetDescent;
end;

function TpgfFont.Height: integer;
begin
  result := FFontRes.GetHeight;
end;

{ TpgfFontResource }

constructor TpgfFontResource.Create(const afontdesc: string);
begin
  inherited;
  FRefCount := 0;
end;

function TpgfFontResource.DecRefCount: integer;
begin
  dec(FRefCount);
  result := FRefCount;
end;

function TpgfFontResource.IncRefCount: integer;
begin
  inc(FRefCount);
  result := FRefCount;
end;

{ TpgfCanvas }

constructor TpgfCanvas.Create(awin: TpgfWindow);
begin
  inherited Create;
  
  FBeginDrawCount := 0;
  FWindow := awin;
  
  // options
  FBufferedDraw := true; // transparent widgets must turn this off
  FPersistentResources := false;
end;

destructor TpgfCanvas.Destroy;
begin
  if pgfCaret.FCanvas = self then
  begin
    pgfCaret.UnSetCaret(self);
  end;
  inherited Destroy;
end;

procedure TpgfCanvas.BeginDraw;
begin
  BeginDraw(FBufferedDraw);
end;

procedure TpgfCanvas.BeginDraw(abuffered: boolean);
begin
  if FBeginDrawCount < 1 then
  begin
    DoBeginDraw(FWindow, abuffered);

    SetColor(clText1);
    SetTextColor(clText1);
    SetFont(pgfDisp.DefaultFont);

    SetLineStyle(0,lsSolid);

    FBeginDrawCount := 0;
  end;
  inc(FBeginDrawCount);
end;

procedure TpgfCanvas.EndDraw(x, y, w, h: TpgfCoord);
begin
  if FBeginDrawCount > 0 then
  begin
    dec(FBeginDrawCount);
    if FBeginDrawCount = 0 then
    begin
      DoPutBufferToScreen(x,y,w,h);
      
      if not FPersistentResources then DoEndDraw;   // !!!
    end;
  end;
end;

procedure TpgfCanvas.EndDraw;
begin
  EndDraw(0,0,FWindow.Width,FWindow.Height);
end;

procedure TpgfCanvas.FreeResources;
begin
  DoEndDraw;
  FBeginDrawCount := 0;
end;

procedure TpgfCanvas.SetFont(fnt: TpgfFont);
begin
  FFont := fnt;
  DoSetFontRes(fnt.FFontRes);
end;

procedure TpgfCanvas.SetTextColor(cl: TpgfColor);
begin
  FTextColor := cl;
  DoSetTextColor(FTextColor);
end;

procedure TpgfCanvas.SetColor(cl: TpgfColor);
begin
  FColor := cl;
  DoSetColor(FColor);
end;

procedure TpgfCanvas.SetLineStyle(awidth: integer; astyle : TpgfLineStyle);
begin
  FLineWidth := awidth;
  FLineStyle := astyle;
  DoSetLineStyle(FLineWidth, FLineStyle);
end;

procedure TpgfCanvas.DrawString(x, y: TpgfCoord; const txt: widestring);
begin
  DoDrawString(x,y,txt);
end;

procedure TpgfCanvas.Clear(col: TpgfColor);
var
  ACol : TpgfColor;
  AWinRect : TpgfRect;
begin
  ACol := FColor;
  DoSetColor(col);
  DoGetWinRect(AWinRect);
  DoFillRectangle(0,0,AWinRect.Width,AWinRect.Height);
  DoSetColor(ACol);
end;

procedure TpgfCanvas.GetWinRect(var r: TpgfRect);
begin
  DoGetWinRect(r);
end;

procedure TpgfCanvas.FillRectangle(x, y, w, h: TpgfCoord);
begin
  DoFillRectangle(x,y,w,h);
end;

procedure TpgfCanvas.XORFillRectangle(col : TpgfColor; x, y, w, h : TpgfCoord);
begin
  DoXORFillRectangle(col, x, y, w, h);
end;

procedure TpgfCanvas.XORFillRect(col : TpgfColor; r: TpgfRect);
begin
  DoXORFillRectangle(col, r.Left,r.Top,r.Width,r.Height);
end;

procedure TpgfCanvas.FillRect(r: TpgfRect);
begin
  DoFillRectangle(r.Left,r.Top,r.Width,r.Height);
end;

procedure TpgfCanvas.FillTriangle(x1, y1, x2, y2, x3, y3: TpgfCoord);
begin
  DoFillTriangle(x1, y1, x2, y2, x3, y3);
end;

procedure TpgfCanvas.DrawRectangle(x, y, w, h: TpgfCoord);
begin
  DoDrawRectangle(x, y, w, h);
end;

procedure TpgfCanvas.DrawRect(r: TpgfRect);
begin
  DoDrawRectangle(r.Left,r.Top,r.Width,r.Height);
end;

procedure TpgfCanvas.DrawArc(x, y, w, h: TpgfCoord; a1, a2: double);
begin
  DoDrawArc(x,y,w,h,a1,a2);
end;

procedure TpgfCanvas.FillArc(x, y, w, h: TpgfCoord; a1, a2: double);
begin
  DoFillArc(x,y,w,h,a1,a2);
end;

procedure TpgfCanvas.DrawLine(x1, y1, x2, y2: TpgfCoord);
begin
  DoDrawLine(x1, y1, x2, y2);
end;

procedure TpgfCanvas.SetClipRect(const rect: TpgfRect);
begin
  DoSetClipRect(rect);
end;

function TpgfCanvas.GetClipRect: TpgfRect;
begin
  result := DoGetClipRect;
end;

procedure TpgfCanvas.AddClipRect(const rect: TpgfRect);
begin
  DoAddClipRect(rect);
end;

procedure TpgfCanvas.ClearClipRect;
begin
  DoClearClipRect;
end;

procedure TpgfCanvas.DrawImage(x, y: TpgfCoord; img: TpgfImage);
begin
  if img = nil then exit;
  DrawImagePart(x,y,img,0,0,img.Width,img.Height);
end;

procedure TpgfCanvas.DrawImagePart(x, y: TpgfCoord; img: TpgfImage;
  xi, yi, w, h: integer);
begin
  DoDrawImagePart(x,y, img, xi,yi, w,h);
end;

procedure TpgfCanvas.DrawButtonFace(x, y, w, h: TpgfCoord);
begin
  pgfStyle.DrawButtonFace(self, x, y, w, h);
end;

procedure TpgfCanvas.DrawControlFrame(x, y, w, h: TpgfCoord);
begin
  pgfStyle.DrawControlFrame(self, x, y, w, h);
end;

procedure TpgfCanvas.DrawDirectionArrow(x, y, w, h: TpgfCoord; direction: integer);
begin
  pgfStyle.DrawDirectionArrow(self, x, y, w, h, direction);
end;

{ TpgfWindow }


constructor TpgfWindow.Create(aowner: TComponent);
begin
  inherited Create(aowner); // initialize the platform internals

  FTop    := 0;
  FLeft   := 0;
  FWidth  := 16;
  FHeight := 16;

  FMinWidth  := 2;
  FMinHeight := 2;

  FModalForWin := nil;

  if (aowner <> nil) and (aowner is TpgfWindow) then
  begin
    FParentWindow := TpgfWindow(aowner);
    FWindowType := wtChild;
  end
  else
  begin
    FParentWindow := nil;
    FWindowType := wtWindow;
  end;

  FCanvas := TpgfCanvas.Create(self);
end;

destructor TpgfWindow.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TpgfWindow.AllocateWindowHandle;
begin
  DoAllocateWindowHandle(FParentWindow);
end;

procedure TpgfWindow.ReleaseWindowHandle;
begin
  if HasHandle then
  begin
    Canvas.FreeResources;
    DoReleaseWindowHandle;
  end;
end;

function TpgfWindow.Right: TpgfCoord;
begin
  result := FLeft + FWidth - 1;
end;

function TpgfWindow.Bottom: TpgfCoord;
begin
  result := FTop + FHeight - 1;
end;

procedure TpgfWindow.UpdateWindowPosition;
begin
  if HasHandle then
    DoUpdateWindowPosition(FLeft, FTop, FWidth, FHeight);
end;

{ TpgfImage }

procedure TpgfImage.AllocateImage(acolordepth, awidth, aheight: integer);
var
  dww : integer;
begin
  FreeImage;

  FWidth := awidth;
  FHeight := aheight;

  FColorDepth := acolordepth;

  // Real bitmap
  if FColorDepth = 1 then dww := (awidth+31) div 32
                     else dww := FWidth;

  FImageDataSize := dww * FHeight * 4;

  GetMem(FImageData, FImageDataSize);
end;

procedure TpgfImage.AllocateMask;
var
  dww : integer;
begin
  if (FWidth < 1) or (FHeight < 1) then Exit;

  FMasked := true;

  if FMaskData <> nil then FreeMem(FMaskData);

  dww := (FWidth + 31) div 32;

  FMaskDataSize := dww * FHeight * 4;
  GetMem(FMaskData, FMaskDataSize);
end;

constructor TpgfImage.Create;
begin
  inherited;

  FWidth := 0;
  FHeight := 0;
  FColorDepth := 0;

  FImageData := nil;
  FImageDataSize := 0;
  FMaskData := nil;
  FMaskDataSize := 0;
  FMasked := false;
end;

procedure TpgfImage.CreateMaskFromSample(x, y: integer);
var
  p : ^longword;
  pmsk : ^byte;
  c : longword;

  linecnt : integer;
  pixelcnt : integer;
  bit : byte;
  msklinelen : integer;
begin
  if FColorDepth = 1 then Exit;

  if (FImageData = nil) then Exit;

  AllocateMask;
  
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

end;

destructor TpgfImage.Destroy;
begin
  FreeImage;
  inherited;
end;

procedure TpgfImage.FreeImage;
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
end;

procedure TpgfImage.Invert;
var
  p : ^Byte;
  n : integer;
begin
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
end;

procedure TpgfImage.UpdateImage;
begin
  if FImageData <> nil then
    DoInitImage(FColorDepth, FWidth, FHeight, FImageData);
    
  if FMaskData <> nil then
    DoInitImageMask(FWidth, FHeight, FMaskData);
end;

{ TpgfStyle }

constructor TpgfStyle.Create;
begin
  inherited;
  // Style description

  pgfSetNamedFont('Label1',        'Arial-10');
  pgfSetNamedFont('Label2',        'Arial-10:bold');

  pgfSetNamedFont('Edit1',         'Arial-10');
  pgfSetNamedFont('Edit2',         'Courier New-10');

  pgfSetNamedFont('List',          'Arial-10');

  pgfSetNamedFont('Grid',          'Arial-9:antialias=false');
  pgfSetNamedFont('GridHeader',    'Arial-9:bold:antialias=false');

  pgfSetNamedFont('Menu',          'Arial-10');
  pgfSetNamedFont('MenuAccel',     'Arial-10:bold');
  pgfSetNamedFont('MenuDisabled',  'Arial-10:italic');


  pgfSetNamedColor( clWindowBackground,    $D4D0C8);
  pgfSetNamedColor( clBoxColor,            $FFFFFF);

  pgfSetNamedColor( clShadow1,             $808080);
  pgfSetNamedColor( clShadow2,             $404040);
  pgfSetNamedColor( clHilite1,             $E0E0E0);
  pgfSetNamedColor( clHilite2,             $FFFFFF);

  pgfSetNamedColor( clText1,               $000000);
  pgfSetNamedColor( clText2,               $000040);
  pgfSetNamedColor( clText3,               $800000);
  pgfSetNamedColor( clText4,               $404000);

  pgfSetNamedColor( clSelection,           $000080);
  pgfSetNamedColor( clSelectionText,       $FFFFFF);

  pgfSetNamedColor( clInactiveSel,         $D0D0FF);
  pgfSetNamedColor( clInactiveSelText,     $000000);

  pgfSetNamedColor( clScrollBar,           $E8E4DB);
  pgfSetNamedColor( clButtonFace,          $D4D0C8);

  pgfSetNamedColor( clListBox,             $FFFFFF);

  pgfSetNamedColor( clGridLines,           $A0A0A0);
  pgfSetNamedColor( clGridHeader,          $E0E0E0);

  pgfSetNamedColor( clWidgetFrame,         $000000);
  pgfSetNamedColor( clInactiveWgFrame,     $A0A0A0);

  pgfSetNamedColor( clTextCursor,          $000000);

  pgfSetNamedColor( clChoiceListBox,       $E8E8E8);

  pgfSetNamedColor( clUnset,               $D0D0FF);

  pgfSetNamedColor( clMenuText,            $000000);
  pgfSetNamedColor( clMenuDisabled,        $909090);

  // Global Font Objects

  DefaultFont := pgfGetFont(pgfGetNamedFontDesc('Label1'));

  MenuFont := pgfGetFont(pgfGetNamedFontDesc('Menu'));
  MenuAccelFont := pgfGetFont(pgfGetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := pgfGetFont(pgfGetNamedFontDesc('MenuDisabled'));
end;

procedure TpgfStyle.DrawButtonFace(canvas: TpgfCanvas; x, y, w, h: TpgfCoord);
begin
  canvas.SetColor(clButtonFace);
  canvas.FillRectangle(x,y, w,h);

  canvas.SetColor(clHilite1);

  Canvas.DrawLine(x,y+h-2, x,y);
  Canvas.DrawLine(x,y, x+w-1,y);

  canvas.SetColor(clHilite2);
  Canvas.DrawLine(x+1,y+h-3, x+1,y+1);
  Canvas.DrawLine(x+1,y+1,  x+w-2,y+1);

  canvas.SetColor(clShadow2);
  Canvas.DrawLine(x+w-1,y+1, x+w-1,y+h-1);
  Canvas.DrawLine(x,y+h-1, x+w-1,y+h-1);

  canvas.SetColor(clShadow1);
  Canvas.DrawLine(x+w-2,y+2, x+w-2,y+h-2);
  Canvas.DrawLine(x+1,y+h-2, x+w-2,y+h-2);
end;

procedure TpgfStyle.DrawControlFrame(canvas: TpgfCanvas; x, y, w, h: TpgfCoord);
begin
  canvas.SetColor(clShadow1);
  Canvas.DrawLine(x,y, x+w-1,y);
  Canvas.DrawLine(x,y+h-1, x,y);

  canvas.SetColor(clShadow2);
  Canvas.DrawLine(x+1,y+1,  x+w-2,y+1);
  Canvas.DrawLine(x+1,y+h-2, x+1,y+1);

  canvas.SetColor(clHilite2);
  Canvas.DrawLine(x+1,y+h-1, x+w-1,y+h-1);
  Canvas.DrawLine(x+w-1,y+1, x+w-1,y+h-1);

  canvas.SetColor(clHilite1);
  Canvas.DrawLine(x+2,y+h-2, x+w-2,y+h-2);
  Canvas.DrawLine(x+w-2,y+2, x+w-2,y+h-2);
end;

procedure TpgfStyle.DrawDirectionArrow(canvas: TpgfCanvas; x, y, w, h: TpgfCoord; direction: integer);
var
  peekx, peeky : TpgfCoord;
  basex, basey : TpgfCoord;
  side, margin : TpgfCoord;
begin
  canvas.SetColor(clText1);

  side := (w div 4) + 1;
  margin := side + 1;

  if direction < 2 then  // vertical
  begin
    peekx := x+(w div 2);
    if direction = 1 then  // down
    begin
      peeky := y+h-margin;
      basey := peeky-side;
    end
    else
    begin                  // up
      peeky := y+margin;
      basey := peeky+side;
    end;
    canvas.FillTriangle(peekx, peeky, peekx+side, basey, peekx-side, basey );
  end
  else // horizontal
  begin
    peeky := y + (h div 2);
    if direction = 3 then  // right
    begin
      peekx := x + w - margin;
      basex := peekx - side;
    end
    else                   // left
    begin
      peekx := x + margin;
      basex := peekx + side;
    end;
    canvas.FillTriangle(peekx, peeky, basex, peeky-side, basex, peeky+side );
  end;

end;

procedure TpgfStyle.DrawMenuBar(ACanvas: TpgfCanvas; r: TpgfRect; ABackgroundColor: TpgfColor);
begin
  ACanvas.Clear(ABackgroundColor);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor({clWhite}$ffffff);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;

procedure TpgfStyle.DrawMenuRow(ACanvas: TpgfCanvas; r: TpgfRect; AFlags: TlpMenuItemFlags);
begin
  ACanvas.FillRect(r);
end;

procedure TpgfStyle.DrawMenuItem(ACanvas: TpgfCanvas; r: TpgfRect;
  AFlags: TlpMenuItemFlags; AText: WideString);
begin
  //
end;

procedure TpgfStyle.DrawMenuItemSeparator(ACanvas: TpgfCanvas; r: TpgfRect);
begin
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left+1, r.Top+2, r.Right, r.Top+2);
  ACanvas.SetColor(clHilite2);
  ACanvas.DrawLine(r.Left+1, r.Top+3, r.Right, r.Top+3);
end;

procedure TpgfStyle.DrawMenuItemImage(ACanvas: TpgfCanvas; x, y: TpgfCoord; r: TpgfRect; AFlags: TlpMenuItemFlags);
var
  img: TpgfImage;
  lx: TpgfCoord;
  ly: TpgfCoord;
begin
  if mifChecked in AFlags then
  begin
    img := pgfImages.GetImage('stdimg.check');    // Do NOT localize
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(x, y, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
  if mifSubMenu in AFlags then
  begin
    img := pgfImages.GetImage('sys.sb.right');    // Do NOT localize
    lx := (r.height div 2) - 3;
    lx := r.right-lx-2;
    ly := y + ((r.Height-img.Height) div 2);
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(lx, ly, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
end;

procedure TpgfStyle.DrawString(ACanvas: TpgfCanvas; x, y: TpgfCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit; //==>
  if not AEnabled then
  begin
    ACanvas.SetTextColor(clHilite2);
    ACanvas.DrawString(x+1, y+1, AText);
    ACanvas.SetTextColor(clShadow1);
  end;
  ACanvas.DrawString(x, y, AText);
end;

function TpgfStyle.GetSeparatorSize: integer;
begin
  result := 2;
end;

{ TpgfCaret }

procedure TpgfCaret.OnTimerTime(sender: TObject);
begin
  if FEnabled then InvertCaret;
  //Writeln('Caret timer2 ...');
end;

constructor TpgfCaret.Create(AOwner: TComponent);
begin
  FEnabled := false;
  FInterval := 500;
  FCanvas  := nil;
  FTop := 0;
  FLeft := 0;
  FWidth := 1;
  FHeight := 8;
  FTimer := TlpTimer.Create(self);
  FTImer.Interval := FInterval;
  FTimer.OnTimer := self.OnTimerTime;
  FTimer.Enabled := true;
end;

destructor TpgfCaret.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TpgfCaret.SetCaret(acanvas: TpgfCanvas; x, y, w, h: TpgfCoord);
begin
  FEnabled := true;
  FVisible := false;
  FCanvas := acanvas;
  FLeft := x;
  FTop  := y;
  FWidth := w;
  FHeight := h;
  InvertCaret;
  
  FTimer.Enabled := false;
  FTimer.Interval := FInterval;
  FTimer.Enabled := true;
end;

procedure TpgfCaret.UnSetCaret(acanvas : TpgfCanvas);
begin
  if (FCanvas = acanvas) or (acanvas = nil) then
  begin
    //if FVisible and (Fcanvas <> nil) then InvertCaret;
    FEnabled := false;
    FCanvas  := nil;
  end;
end;

procedure TpgfCaret.InvertCaret;
begin
  if FCanvas = nil then Exit;
  
  FCanvas.BeginDraw(false); // we could not be sure about the buffer contents!
  FCanvas.XORFillRectangle($FFFFFF, FLeft, FTop, FWidth, FHeight);
  FVisible := not FVisible;
  FCanvas.EndDraw(FLeft, FTop, FWidth, FHeight);
end;

{ TpgfImages }

constructor TpgfImages.Create;
begin
  FImages := TStringList.Create;
end;

destructor TpgfImages.Destroy;
var
  n : integer;
begin
  for n := 0 to FImages.Count-1 do
  begin
    FImages.Objects[n].Free;
  end;
  FImages.Free;
  inherited Destroy;
end;

function TpgfImages.AddImage(const imgid: string; img: TpgfImage): boolean;
var
  i : integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    FImages.Strings[i] := LowerCase(imgid);
    FImages.Objects[i] := img;
    result := false;
  end
  else
  begin
    FImages.AddObject(LowerCase(imgid), img);
    result := true;
  end;
end;

function TpgfImages.DeleteImage(const imgid: string; freeimg: boolean): boolean;
var
  i : integer;
  img : TpgfImage;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    if freeimg then TpgfImage(FImages.Objects[i]).Free;
    FImages.Delete(i);
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TpgfImages.GetImage(const imgid: string): TpgfImage;
var
  i : integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then result := TpgfImage(FImages.Objects[i])
            else result := nil;
end;

function TpgfImages.AddBMP(const imgid: string; bmpdata: pointer; bmpsize: integer): TpgfImage;
begin
  result := CreateImage_BMP(bmpdata, bmpsize);
  if result <> nil then AddImage(imgid, result);
end;

function TpgfImages.AddMaskedBMP(const imgid: string; bmpdata: pointer; bmpsize: integer; mcx, mcy: integer): TpgfImage;
begin
  result := AddBMP(imgid, bmpdata, bmpsize);
  if result <> nil then
  begin
    result.CreateMaskFromSample(mcx, mcy);
    result.UpdateImage;
  end;
end;

procedure TpgfImages.ListImages(var sl: TStringList);
begin
  if sl <> nil then sl.Assign(FImages);
end;

procedure GetFontDescValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to pgfNamedFonts.Count-1 do
      Proc('#'+TNamedFontItem(pgfNamedFonts[I]).FontID);
end;

function  PtInRect(const ARect: TpgfRect; const APoint: TPoint): Boolean;
begin
  Result := (APoint.x >= ARect.Left) and
            (APoint.y >= ARect.Top) and
            (APoint.x <= ARect.Right) and
            (APoint.y <= ARect.Bottom);
end;            

function  InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function InflateRect(var Rect: TpgfRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    dec(Rect.Left, dx);
    dec(Rect.Top, dy);
    inc(Rect.Width, 2*dx);
    inc(Rect.Height, 2*dy);
    Result := True;
  end
  else
    Result := False;
end;

procedure TlpTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then FOnTimer(self);
end;

initialization
begin
  pgfDisp := nil;
  pgfTimers := nil;
  pgfCaret := nil;
  pgfImages := nil;
  pgfDesigning := False;
  pgfInitMsgQueue;
end;

end.

