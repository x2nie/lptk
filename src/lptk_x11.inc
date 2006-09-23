// lptk2_x11.inc

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
    Window : TptkWinHandle;
    SwapAction : PChar;
  end;

var
  InputMethod  : PXIM;
  InputContext : PXIC;

var
  LastClickWindow  : TptkWinHandle;
  LastWinClickTime : longword;

// defines:
procedure XRenderSetPictureClipRectangles(disp : PXDisplay; pic : TPicture; xorigin,yorigin : integer; rect : PXRectangle; num : integer); cdecl; external;

// redefines:
function XmbLookupString(p1 : PXIC; ev : PXKeyPressedEvent; str : PChar; len : longword; ks:PKeySym; stat:PStatus):longint;cdecl; external;

{$IFDEF BUFFERING}
function XdbeQueryExtension(ADisplay : PXDisplay; AMajor, AMinor : PInt) : PStatus; cdecl; external;
function XdbeAllocateBackBufferName(ADisplay : PXDisplay; AWindow : TptkWinHandle; ASwapAction : PChar) : TptkWinHandle; cdecl; external;
function XdbeSwapBuffers(ADisplay : PXDisplay;ASwapInfo : PXdbeSwapInfo; AScreenNums : Integer) : PStatus; cdecl; external;
function XdbeDeallocateBackBufferName(ADisplay : PXDisplay; ABuffer : TptkWinHandle) : PStatus; cdecl; external;
{$ENDIF}

function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):PXIM;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;

function ptkOpenDisplay(DisplayName : string) : boolean;
var
  wa : TXWindowAttributes;
begin
  Result := false;

  if Display <> nil then ptkCloseDisplay;
  
  Display := XOpenDisplay(PChar(@DisplayName));

  if Display = nil then Exit;

  ptkDefaultScreen := XDefaultScreen(Display);
  ptkRootWindow := XRootWindow(Display, ptkDefaultScreen);
  DefaultBackground := XBlackPixel(Display, ptkDefaultScreen);
  DefaultForeground := XWhitePixel(Display, ptkDefaultScreen);

  ptkDefaultVisual := XDefaultVisual(display, ptkDefaultScreen);
  DisplayDepth := XDefaultDepth(display, ptkDefaultScreen);

  ptkDefaultColorMap := XDefaultColorMap(display, ptkDefaultScreen);

  XGetWindowAttributes(display, ptkRootWindow, @wa);
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

  ptkInternalInit;

  result := True;
end;

procedure ptkCloseDisplay;
begin
  if Display <> nil then
  begin
    XCloseDisplay(Display);
    Display := nil;
  end;
end;

procedure ptkHideConsoleWindow;
begin
end;

procedure ptkFlush;
begin
  XFlush(display);
end;

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

function GetParentWindow(wh : TptkWinHandle; var pw, rw : TptkWinHandle) : boolean;
var
  rootw,
  parentw : TptkWinHandle;
  childs : ^TptkWinHandle;
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

function GetDecorationWindow(wh : TptkWinHandle) : TptkWinHandle;
var
  lpw, pw, rw : TptkWinHandle;
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
  wg, ewg : TptkWidget;
  ks : integer;
  uc : word;
  a : array[1..16] of char;
  ss, sr : integer;
  p : PChar;
  blockmsg : boolean;

  Popup : TptkWidget;
  frm : TptkForm;

  wh : TptkWinHandle;
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

      if ptkTopModalForm <> nil then
      begin
        wg := WidgetParentForm(ewg);
        if (wg <> nil) and (ptkTopModalForm <> wg) then blockmsg := true;
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

      if ptkTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWidget(ev.xbutton.window));
        if (wg <> nil) and (ptkTopModalForm <> wg) then blockmsg := true;
      end;

      //Writeln('Motion: x=',ev.xmotion.x,' y=',ev.xmotion.y,'  st=',ev.xmotion.state);

      if not blockmsg then
        PostMessage(nil, FindWidget(ev.xbutton.window), ev._type, ev.xmotion.x, ev.xmotion.y, ev.xmotion.state);

    end;

    // message blockings for modal windows
    MSG_CLOSE:
    begin
      if ptkTopModalForm <> nil then
      begin
        wg := WidgetParentForm(FindWidget(ev.xbutton.window));
        if (wg <> nil) and (ptkTopModalForm <> wg) then blockmsg := true;
      end;

      if not blockmsg then PostMessage(nil, FindWidget(ev.xany.window), ev._type, 0,0,0);

    end;

    ConfigureNotify:
    begin
      repeat until not XCheckTypedWindowEvent(display, ev.xany.window, ConfigureNotify, @ev);
      
      wg := FindWidget(ev.xconfigure.window);
      if (wg <> nil) and (wg is TptkForm) then
      begin
        frm := TptkForm(wg);
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

function ptkColorToX(col : TptkColor) : longword;
var
  xc : TXColor;
  c : TptkColor;
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
    XAllocColor(display, ptkDefaultColorMap, @xc);
    result := xc.pixel;
  end;
end;

procedure SetXftColor(col : TptkColor; var colxft : TXftColor);
var
  c : TptkColor;
begin
  c := ptkColorToRGB(col);

  colxft.color.blue  := (c and $000000FF) shl 8;
  colxft.color.green := (c and $0000FF00);
  colxft.color.red   := (c and $00FF0000) shr 8;

  colxft.color.alpha := (c and $7F000000) shr 15;
  colxft.color.alpha := colxft.color.alpha xor $FFFF;  // invert: 0 in ptkColor means not translucent

  colxft.pixel := 0;
end;

procedure ptkSetWMOptions(wh : TptkWinHandle; aflags, afunctions, adecorations, ainputmode : longword);
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

function ptkGetFontFaceList : TStringList;
var
  pfs : PFcFontSet;
  ppat : PPFcPattern;
  n : integer;
  s : string;
  pc : PChar;
  fl : TStringList;
begin
  pfs := XftListFonts(display, ptkDefaultScreen,
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



