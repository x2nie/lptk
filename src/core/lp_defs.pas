unit lp_defs;

// platform independent class definitions for platform implementations
// one big file, no includes
// only the very necessary things

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils;
  
type
  TpgfCoord = integer;     // we might use floating point coordinates in the future...

  TpgfColor = longword;

{$ifndef FPC}
  PtrInt = integer;
{$endif}

  TpgfParam = PtrInt;      // type of the parameters, 32 or 64 bits depending on the CPU architecture

type
  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);
  
  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos);
  
  TWindowAttributes = set of TWindowAttribute;
  
  TMouseCursor = (mcDefault,
    mcArrow, mcCross, mcIBeam,
    mcSizeEW, mcSizeNS, mcSizeNWSE, mcSizeNESW,
    mcMove, mcHourGlass);

const
  MOUSE_LEFT   = 1;
  MOUSE_RIGHT  = 2;
  MOUSE_MIDDLE = 4;
  
const
  ss_Shift   = $0001;
  ss_Control = $0004;
  ss_Alt     = $0008;

  ss_CapsLock   = $0002;
  ss_NumLock    = $0010;
  ss_ScrollLock = $0080;

const
  PGFM_PAINT       = 1;

  PGFM_ACTIVATE    = 2;
  PGFM_DEACTIVATE  = 3;

  PGFM_KEYPRESS    = 4;
  PGFM_KEYRELEASE  = 5;
  PGFM_KEYCHAR     = 6;

  PGFM_MOUSEDOWN   = 7;
  PGFM_MOUSEUP     = 8;
  PGFM_MOUSEMOVE   = 9;
  PGFM_DOUBLECLICK = 10;

  PGFM_MOUSEENTER  = 11;
  PGFM_MOUSEEXIT   = 12;

  PGFM_CLOSE       = 13;

  PGFM_SCROLL      = 14;
  PGFM_RESIZE      = 15;
  PGFM_MOVE        = 16;

  PGFM_POPUPCLOSE  = 17;

  PGFM_KILLME      = 9999;

const
  // The special keys, based on the well-known keyboard scan codes
  KEY_LEFT    = $FF4B;
  KEY_RIGHT   = $FF4D;
  KEY_DOWN    = $FF50;
  KEY_UP      = $FF48;

  KEY_END     = $FF4F;
  KEY_HOME    = $FF47;

  KEY_PGUP    = $FF49;
  KEY_PGDN    = $FF51;
  KEY_INSERT  = $FF52;
  KEY_DELETE  = $FF53;

  KEY_F1      = $FF3B;
  KEY_F2      = KEY_F1 + 1;
  KEY_F3      = KEY_F1 + 2;
  KEY_F4      = KEY_F1 + 3;
  KEY_F5      = KEY_F1 + 4;
  KEY_F6      = KEY_F1 + 5;
  KEY_F7      = KEY_F1 + 6;
  KEY_F8      = KEY_F1 + 7;
  KEY_F9      = KEY_F1 + 8;
  KEY_F10     = KEY_F1 + 9;

  KEY_F11     = $FF57;
  KEY_F12     = $FF58;

  // some general keys

  KEY_TAB       = $0009;
  KEY_ENTER     = $000D;
  KEY_SPACE     = $0020;
  KEY_ESC       = $001B;
  KEY_BACKSPACE = $0008;

const
  // scan codes for KeyPress/KeyRelease
  KEYSC_ENTER = $1C;
  KEYSC_SPACE = $39;  

type
  TpgfRect = object  // not class for static allocations
    top, left,
    width, height : TpgfCoord;
    procedure SetRect(aleft,atop,awidth,aheight : TpgfCoord);
    function bottom : TpgfCoord;
    function right  : TpgfCoord;
    procedure SetBottom(value : TpgfCoord);
    procedure SetRight(value : TpgfCoord);
  end;

type
  TpgfMsgParMouse = record
    x,y : TpgfCoord;
    buttons : word;
    shiftstate : word;
  end;

  TpgfMsgParKeyboard = record
    keycode : word;
    shiftstate : word;
  end;

  TpgfMessageParams = record
  case integer of
    0: (mouse : TpgfMsgParMouse);
    1: (keyboard : TpgfMsgParKeyboard);
    2: (rect : TpgfRect);
  end;

  TpgfMessageRec = record
    MsgCode : integer;
    Sender  : TObject;
    Dest    : TObject;
    Params  : TpgfMessageParams;
  end;
  PpgfMessageRec = ^TpgfMessageRec;

const
  PGF_DEFAULT_FONT_DESC = 'Arial-10';

const
  UserNamedColorStart = 128;

// named color identifiers
const
  clWindowBackground     = $80000001;
  clBoxColor             = $80000002;

  clButtonFace           = $80000003;

  clShadow1              = $80000004;
  clShadow2              = $80000005;
  clHilite1              = $80000006;
  clHilite2              = $80000007;

  clText1                = $80000008;
  clText2                = $80000009;
  clText3                = $8000000A;
  clText4                = $8000000B;

  clSelection            = $8000000C;
  clSelectionText        = $8000000D;

  clInactiveSel          = $8000000E;
  clInactiveSelText      = $8000000F;

  clScrollBar            = $80000010;

  clListBox              = $80000011;

  clGridLines            = $80000012;
  clGridHeader           = $80000013;

  clWidgetFrame          = $80000014;
  clInactiveWgFrame      = $80000015;

  clTextCursor           = $80000016;

  clChoiceListBox        = $80000017;

  clUnset		 = $80000018;

  clMenuText             = $80000019;
  clMenuDisabled         = $8000001A;
  
type
  TpgfLineStyle = (lsSolid, lsDashed);

type
  TpgfImageBase = class
  end;

  TpgfCanvasBase = class
  end;

  TpgfFontResourceBase = class
  end;

  TpgfFontBase = class
  end;

  TlpComponent =class(TComponent)
  end;

  TpgfWindowBase = class(TlpComponent)
  protected
    FWindowType : TWindowType;
    FWindowAttributes : TWindowAttributes;
    FTop, FLeft, FWidth, FHeight : TpgfCoord;
    FMinWidth, FMinHeight : TpgfCoord;

  public
    // make some setup before the window shows
    procedure AdjustWindowStyle; virtual;    // forms modify the window creation parameters
    procedure SetWindowParameters; virtual;  // invoked after the window is created
  end;

  TpgfDisplayBase = class
  public
    constructor Create(const aparams : string); virtual; abstract;
  end;


implementation

{ TpgfRect }

procedure TpgfRect.SetRect(aleft, atop, awidth, aheight: TpgfCoord);
begin
  left := aleft;
  top  := atop;
  width := awidth;
  height := aheight;
end;

function TpgfRect.bottom: TpgfCoord;
begin
  result := top + height - 1;
end;

function TpgfRect.right: TpgfCoord;
begin
  result := left + width - 1;
end;

procedure TpgfRect.SetBottom(value: TpgfCoord);
begin
  height := value - top + 1;
end;

procedure TpgfRect.SetRight(value: TpgfCoord);
begin
  width := value - left + 1;
end;

{ TpgfWindowBase }

procedure TpgfWindowBase.AdjustWindowStyle;
begin
  // does nothing here
end;

procedure TpgfWindowBase.SetWindowParameters;
begin
  // does nothing
end;

end.

