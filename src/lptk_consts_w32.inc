// lp2keycodes_w32.inc

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
  ss_Shift   = $0001;
  ss_Control = $0004;
  ss_Alt     = $0008;

  ss_CapsLock   = $0002;
  ss_NumLock    = $0010;
  ss_ScrollLock = $0080;
  
const
  WM_MOUSEWHEEL       = $020A;

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
  