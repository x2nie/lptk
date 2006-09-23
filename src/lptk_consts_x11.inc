// lp2keycodes_x11.inc

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
  ss_Shift   = $0001;
  ss_Control = $0004;
  ss_Alt     = $0008;

  ss_CapsLock   = $0002;
  ss_NumLock    = $0010;
  ss_ScrollLock = $0080;

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
  
  
//-----------------------------  
// EXTRA constants for X11 only

const
  DOUBLECLICK_MS = 200; // the max time between left-clicks for doubleclick
  
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

