{ gfxbase.pas: base functionality and OS dependent functions
  File maintainer: nvitya@freemail.hu
}
unit lptk;

{$include lptk_config.inc}

{$IFDEF BUFFERING}
 {$IFDEF x11}
   {$linklib Xext}
 {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$ifdef Win32}
    windows {$ifndef FPC},messages{$endif}
{$else}
    X, Xlib, XUtil, x11_xft
{$endif}
 ;

type
  TptkCoord = integer;     // we might use floating point coordinates in the future...

  TptkColor = longword;

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
    min_height, max_height : TptkCoord;
  end;

  TptkRect = object  // not class !
    top, left,
    width, height : TptkCoord;
    procedure SetRect(aleft,atop,awidth,aheight : TptkCoord);
    function bottom : TptkCoord;
    function right  : TptkCoord;
    procedure SetBottom(value : TptkCoord);
    procedure SetRight(value : TptkCoord);
  end;

{$ifdef Win32}
type
  TptkWinHandle   = HWND;
  TptkFontData = HFONT;
  TptkFontHandle  = HFONT;
  TptkGContext    = HDC;
  TptkDisplay  = HDC;
  {$ifndef FPC}
  WndProc = TFNWndProc;
  {$endif}
{$endif}

{$ifdef x11}
type
  TptkWinHandle   = TXID;
  TptkDisplay  = PXDisplay;
  TptkFontData = PXftFont;
  TptkFontHandle  = PXftFont;
  TptkGContext    = Xlib.TGc;
{$endif}

const
  LINEFEEDSTRING = #13#10;

{$INCLUDE lptk_intltexts.inc}

{$ifdef win32}
  {$include lptk_consts_w32.inc}
{$else}
  {$include lptk_consts_x11.inc}
{$endif}

const
  ptkMessageQueueSize = 512;

const
  MSG_KILLME = 9999;

type
  TptkMessageRec = record
    MsgCode : integer;
    Sender  : TObject;
    Dest    : TObject;
    Param1  : integer;
    Param2  : integer;
    Param3  : integer;
  end;
  PptkMessageRec = ^TptkMessageRec;

var
  ptkValidateMsgDest : boolean;

type
  TptkFontResource = class
  private
    FFont : TptkFontData;
    FRefCount : integer;
    FFontDesc : string;
    {$ifdef Win32}
      FMetrics : Windows.TEXTMETRIC;
    {$endif}
  public
    constructor Create(afont : TptkFontData; aFontDesc : string);
    destructor Destroy; override;

    function Handle : TptkFontHandle;

    property FontDesc : string read FFontDesc;

    {$ifdef Win32}
      property Metrics : Windows.TEXTMETRIC read FMetrics;
    {$endif}
  end;

  TptkFont = class
  private
    FFontRes : TptkFontResource;
    FName : string;
  public
    constructor Create(afont : TptkFontResource; AllocName : string);
    destructor Destroy; override;

    function Handle : TptkFontHandle;

    function TextWidth(txt : widestring) : integer;

    function Ascent  : integer;
    function Descent : integer;
    function Height  : integer;

    function FontDescx : string;
    
    property FontName : string read FName;
  end;

  TptkImage = class
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

  TptkCanvas = class
  private
    FWin : TptkWinHandle;
    FBufferWin : TptkWinHandle;
    FMainWin : TptkWinHandle;
    Fgc  : TptkGContext;
    FColorText : TptkColor;
    FColor     : TptkColor;
    FBackgroundColor : TptkColor;
    FCurFont   : TptkFont;
    FClipRect  : TptkRect;
    FClipRectSet : Boolean;
    FBufferClipRect : TptkRect;
    FBufferClipRectSet : Boolean;     
    FLineStyle : integer;
    FLineWidth : integer;
    FDrawOnBuffer : Boolean;
  private
    {$ifdef Win32}
      FWindowsColor : longword;

      FBrush : HBRUSH;
      FPen   : HPEN;
      FClipRegion   : HRGN;

      FBufferBrush : HBRUSH;
      FBufferPen   : HPEN;
      FBufferClipRegion   : HRGN;
      FBufferGC : TptkGContext;
      FBufferBitmap : HBitmap;
      FBufferFont : TptkFont;
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
    constructor Create(winhandle : TptkWinHandle);
    destructor Destroy; override;

    procedure MoveResizeWindow(x,y,w,h : TptkCoord);
    procedure SwapBuffer;
    procedure SetFont(fnt : TptkFont);
    procedure SetTextColor(cl : TptkColor);

    procedure SetColor(cl : TptkColor);
    procedure SetLineStyle(width : integer; dashed : boolean);

    procedure DrawString(x,y : TptkCoord; txt : widestring);

    procedure FillRectangle(x,y, w,h : TptkCoord);
    procedure FillRect(r : TptkRect);

    procedure FillTriangle(x1,y1, x2,y2, x3,y3 : TptkCoord);

    procedure DrawRectangle(x,y, w,h : TptkCoord);
    procedure DrawRect(r : TptkRect);

    procedure DrawLine(x1,y1,x2,y2 : TptkCoord);

    procedure DrawSelectionRectangle(x, y, w, h : TptkCoord);

    procedure SetClipRect(const rect : TptkRect);
    function GetClipRect : TptkRect;
    procedure AddClipRect(const rect : TptkRect);
    procedure ClearClipRect;

    procedure GetWinRect(var r : TptkRect);

    procedure Clear(col : TptkColor);

    procedure DrawImage(x,y : TptkCoord; img : TptkImage);
    procedure DrawImagePart(x,y : TptkCoord; img : TptkImage; xi,yi,w,h : integer);
  public

    property Font : TptkFont read FCurFont write SetFont;
    property TextColor : TptkColor read FColorText;
    property Color : TptkColor read FColor;
    property DrawOnBuffer : Boolean read FDrawOnBuffer write SetDrawOnBuffer;
  end;

  
function ptkOpenDisplay(DisplayName : string) : boolean;
procedure ptkCloseDisplay;

function utf8(const utf8str : string) : widestring;
function u8(const utf8str : string) : widestring;
function wsToUtf8(const wstr : widestring) : string;

function ptkColorToRGB(col : TptkColor) : TptkColor;

function ptkGetFont(desc : string) : TptkFont;
function ptkGetFontFaceList : TStringList;

procedure ptkProcessMessages;

procedure ptkWaitWindowMessage;
procedure ptkDoMessageLoop;

procedure ptkFlush;

procedure ptkActivateWindow(wh : TptkWinHandle);

procedure ptkGetAbsolutePosition(wh : TptkWinHandle; x,y : TptkCoord; var xap, yap : TptkCoord);

procedure ptkSetMouseCursor(wh : TptkWinHandle; cur : integer);

function ptkCheckClipboardKey(key, shiftstate : word) : TClipboardKeyType;

function ptkIsAlphaNum(ws : widestring) : boolean;

{$ifdef Win32}
  function ptkColorToWin(col : TptkColor) : TptkColor;
{$else}
  procedure SetXftColor(col : TptkColor; var colxft : TXftColor);
  procedure ptkSetWMOptions(wh : TptkWinHandle; aflags, afunctions, adecorations, ainputmode : longword);
  function ptkColorToX(col : TptkColor) : longword;
{$endif}

procedure ptkPostMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
procedure ptkSendMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);

procedure ptkDeliverMessage(var msg : TptkMessageRec);
procedure ptkDeliverMessages;

// Destination address validation:
procedure ptkRegisterValidMsgDest(obj : TObject);
procedure ptkUnRegisterValidMsgDest(obj : TObject);

function ptkGetFirstMessage : PptkMessageRec;
procedure ptkDeleteFirstMessage;

//procedure HideConsoleWindow;


function ptkImgLibAddImage(const imgid : string; img : TptkImage) : boolean;
function ptkImgLibDeleteImage(const imgid : string; freeimg : boolean) : boolean;
function ptkImgLibGetImage(const imgid : string) : TptkImage;

function ptkImgLibAddBMP(const imgid : string; bmpdata : pointer; bmpsize : integer) : TptkImage;
function ptkImgLibAddMaskedBMP(const imgid : string; bmpdata : pointer; bmpsize : integer; mcx, mcy : integer) : TptkImage;

var
  ScreenWidth, ScreenHeight : integer;

  Display : TptkDisplay;

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

  DefaultBackground : TptkColor;
  DefaultForeground : TptkColor;

  ptkDefaultScreen : integer;
  ptkDefaultVisual : PVisual;
  ptkDefaultColorMap : TColorMap;
  ptkRootWindow : TptkWinHandle;

  xia_clipboard        : TAtom;
  xia_motif_wm_hints   : TAtom;
  xia_wm_protocols     : TAtom;
  xia_wm_delete_window : TAtom;

  xia_wm_state       : TAtom;
  xia_wm_state_modal : TAtom;

  xia_targets        : TAtom;
{$endif}

var
  ptkImageLibrary : TStringList;   // this is public for listing

implementation

uses x11_keyconv, ptkstyle, ptkwidget, ptkform, ptkclipboard, ptkpopup, ptkstdimg, ptkbmpimage;

var
  FFontResourceList : TList;


procedure ptkInternalInit;
begin
  FFontResourceList := TList.Create;
  ptkImageLibrary := TStringList.Create;
  InitClipboard;

  ptkCreateStandardImages;
end;

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


(*
procedure ptkMoveResizeWindow(wh : TptkWinHandle; x,y,w,h : TptkCoord);
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

procedure ptkActivateWindow(wh : TptkWinHandle);
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

procedure ptkGetAbsolutePosition(wh : TptkWinHandle; x,y : TptkCoord; var xap, yap : TptkCoord);
var
{$ifdef Win32}
  pt : TPoint;
{$else}
  cw : TptkWinHandle;
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
    XTranslateCoordinates(display, wh, ptkRootWindow, x, y, @xap, @yap, @cw);
{$endif}
  end;
end;

function ptkIsAlphaNum(ws : widestring) : boolean;
var
  wc : widechar;
begin
  result := true;
  
  if length(ws) < 1 then
  begin
    result := false;
    exit;
  end;

  wc := ws[1];

  if wc > #255 then Exit;

  if (wc < '0') or
     ((wc > 'Z') and (wc < 'a')) or
     ((wc > 'z') and (wc < #$C0))
  then result := false
end;

procedure ptkSetMouseCursor(wh : TptkWinHandle; cur : integer);
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

function ptkCheckClipboardKey(key, shiftstate : word) : TClipboardKeyType;
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

function FindKeyboardFocus : TptkWidget;
begin
  Result := nil;

  if FocusRoot <> nil then
  begin
    Result := FocusRoot;
    while (Result <> nil) and (result.ActiveWidget <> nil) do result := result.ActiveWidget;
  end;
end;

function ptkColorToRGB(col : TptkColor) : TptkColor;
begin
  if (col and $80000000) <> 0 then
  begin
    // named color
    result := guistyle.GetNamedColorRGB(col) or (col and $7F000000);  // keeping alpha
  end
  else result := col;
end;

{$include lptk_msgqueue.inc}

{$ifdef Win32}
  {$include lptk_w32.inc}
{$else}
  {$include lptk_x11.inc}
{$endif}


procedure ptkProcessMessages;
{$ifdef Win32}
var
  Msg: TMsg;
{$endif}
begin
{$ifdef Win32}
  GdiFlush;
  while Windows.PeekMessageW( {$ifdef FPC}@{$endif} Msg, 0, 0, 0, PM_NOREMOVE) do
  begin
    ptkWaitWindowMessageWin;
    GdiFlush;
  end;
{$else}
  XFlush(display);
  while XPending(Display) > 0 do
  begin
    ptkWaitWindowMessageX;
    ptkDeliverMessages;
    XFlush(display);
  end;
{$endif}
end;

procedure ptkWaitWindowMessage;
begin
{$ifdef Win32}
    ptkWaitWindowMessageWin;
{$else}
    ptkWaitWindowMessageX;
{$endif}
end;

procedure ptkDoMessageLoop;
begin
  repeat
{$ifdef Win32}
    ptkWaitWindowMessageWin;
{$else}
    ptkWaitWindowMessageX;
    ptkDeliverMessages;
{$endif}
  until false;
end;

function ptkGetFont(desc : string) : TptkFont;
var
  fnt : TptkFontData;
  fr : TptkFontResource;
  n : integer;
  fdesc : string;
begin
  fdesc := desc;
  if copy(fdesc,1,1)='#' then fdesc := guistyle.GetNamedFontDesc(copy(desc,2,length(desc)));
  
  for n := 0 to FFontResourceList.Count-1 do
  begin
    if TptkFontResource(FFontResourceList[n]).FontDesc = fdesc then
    begin
      fr := TptkFontResource(FFontResourceList[n]);
      inc(fr.FRefCount);
      //Writeln(fr.FRefCount,': ',fr.FontDesc);
      result := TptkFont.Create(fr, desc);
      Exit;
    end;
  end;

{$ifdef Win32}
  fnt := WinOpenFont(fdesc);
{$else}
  fnt := XftFontOpenName(display, ptkDefaultScreen, PChar(fdesc) );
{$endif}

  if {$ifdef Win32}fnt <> 0{$else}fnt <> nil{$endif} then
  begin
    fr := TptkFontResource.Create(fnt, fdesc);
    FFontResourceList.Add(fr);
    Result := TptkFont.Create(fr, desc);
  end
  else
  begin
    writeln('error opening font.');
    Result := nil;
  end;
end;

function ptkImgLibGetImage(const imgid : string) : TptkImage;
var
  i : integer;
begin
  i := ptkImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then result := TptkImage(ptkImageLibrary.Objects[i])
            else result := nil;
end;

function ptkImgLibAddBMP(const imgid : string; bmpdata : pointer; bmpsize : integer) : TptkImage;
begin
  result := CreateBMPImage(bmpdata, bmpsize);
  if result <> nil then ptkImgLibAddImage(imgid, result);
end;

function ptkImgLibAddMaskedBMP(const imgid : string; bmpdata : pointer; bmpsize : integer;
           mcx, mcy : integer) : TptkImage;
begin
  result := ptkImgLibAddBMP(imgid, bmpdata, bmpsize);
  if result <> nil then result.CreateMaskFromSample(mcx, mcy);
end;

function ptkImgLibAddImage(const imgid : string; img : TptkImage) : boolean;
var
  i : integer;
begin
  i := ptkImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then
  begin
    ptkImageLibrary.Strings[i] := UpperCase(imgid);
    ptkImageLibrary.Objects[i] := img;
    result := false;
  end
  else
  begin
    ptkImageLibrary.AddObject(UpperCase(imgid), img);
    result := true;
  end;
end;

function ptkImgLibDeleteImage(const imgid : string; freeimg : boolean) : boolean;
var
  i : integer;
  img : TptkImage;
begin
  i := ptkImageLibrary.IndexOf(UpperCase(imgid));
  if i >= 0 then
  begin
    if freeimg then TptkImage(ptkImageLibrary.Objects[i]).Free;
    ptkImageLibrary.Delete(i);
    result := true;
  end
  else
  begin
    result := false;
  end;
end;    
  
{$include lptk_canvas.inc}

{$include lptk_image.inc}
  
{ TptkRect }

procedure TptkRect.SetRect(aleft, atop, awidth, aheight : TptkCoord);
begin
  left := aleft;
  top  := atop;
  width := awidth;
  height := aheight;
end;

function TptkRect.bottom : TptkCoord;
begin
  result := top + height - 1;
end;

function TptkRect.right : TptkCoord;
begin
  result := left + width - 1;
end;

procedure TptkRect.SetBottom(value : TptkCoord);
begin
  height := value - top + 1;
end;

procedure TptkRect.SetRight(value : TptkCoord);
begin
  width := value - left + 1;
end;

{ TptkFont }

constructor TptkFont.Create(afont : TptkFontResource; AllocName : string);
begin
  FFontRes := afont;
  FName := AllocName;
end;

destructor TptkFont.Destroy;
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

function TptkFont.Handle : TptkFontHandle;
begin
  result := FFontRes.Handle;
end;

function TptkFont.TextWidth(txt : widestring) : integer;
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
  GetTextExtentPoint32W( display, @txt[1], length(txt), ts);
  result := ts.cx;
{$else}
  XftTextExtents16(display, FFontRes.Handle, @txt[1], Length(txt), extents);
  result := extents.xOff;
{$endif}
end;

function TptkFont.Ascent : integer;
begin
{$ifdef Win32}
  result := FFontRes.Metrics.tmAscent;
{$else}
  result := FFontRes.Handle^.ascent;
{$endif}
end;

function TptkFont.Descent : integer;
begin
{$ifdef Win32}
  result := FFontRes.FMetrics.tmDescent;
{$else}
  result := FFontRes.Handle^.Descent;
{$endif}
end;

function TptkFont.Height : integer;
begin
{$ifdef Win32}
  result := FFontRes.FMetrics.tmHeight;
{$else}
  result := FFontRes.Handle^.height; //ascent + FFontRes.Handle^.descent;
{$endif}
end;

function TptkFont.FontDescx: string;
begin
  result := FFontRes.FontDesc;
end;

{ TptkFontResource }

constructor TptkFontResource.Create(afont: TptkFontData; aFontDesc: string);
begin
  FFont := afont;
  FFontDesc := aFontDesc;
  FRefCount := 1;
{$ifdef Win32}
  SelectObject(display, afont);
  GetTextMetrics(display, FMetrics);
{$else}{$endif}
end;

destructor TptkFontResource.Destroy;
begin
{$ifdef Win32}
  Windows.DeleteObject(FFont);
{$else}
  XftFontClose(Display, FFont);
  //XFreeFont(display, FFont);
{$endif}
  inherited;
end;

function TptkFontResource.Handle: TptkFontHandle;
begin
  result := FFont;
end;

//------------------------------------------------

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

  // internal message queue
  UsedFirstMessage := nil;
  UsedLastMessage  := nil;
  FreeFirstMessage := nil;
  FreeLastMessage  := nil;

  ptkValidateMsgDest := True;
  ptkInitMsgQueue;
end;


finalization
begin
{$ifdef Win32}
{$else}
  if Display <> nil then ptkCloseDisplay;
{$endif}
end;

end.

