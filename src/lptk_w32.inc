// lptk2_w32.inc

function ptkOpenDisplay(DisplayName : string) : boolean;
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
    lpfnWndProc := WndProc(@ptkWindowProc);
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
    lpfnWndProc := WndProc(@ptkWindowProc);
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

  ptkInternalInit;

  if pos('-HIDECONSOLE',UpperCase(CmdLine)) > 0 then ptkHideConsoleWindow;

  result := true;
end;


procedure ptkCloseDisplay;
begin
end;

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

procedure ptkHideConsoleWindow;
var
  h : HWND;
begin
  h := GetConsoleWindowHandle;
  //Writeln('Console window: ', h);

  if h <> 0 then
    SetWindowPos(h, 0, 0,0,0,0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOZORDER or SWP_NOMOVE);
end;

procedure ptkFlush;
begin
  GdiFlush;
end;

function ptkColorToWin(col : TptkColor) : TptkColor;
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

function ptkWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
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
        ptkSetMouseCursor(wg.WinHandle, wg.MouseCursor);
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


    WM_CLOSE:  SendMessage(nil, wg, MSG_CLOSE, wParam, lParam, 0);

    WM_PAINT:  SendMessage(nil, wg, MSG_PAINT, wParam, lParam, 0);

  else
    Result := Windows.DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
end;

procedure ptkWaitWindowMessageWin;
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

function WinOpenFont(desc : string) : TptkFontData;
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

function ptkGetFontFaceList : TStringList;
var
  LFont: TLogFont;
begin
  result := TStringList.Create;
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(display, {$ifdef FPC}@{$endif}LFont, @MyFontEnumerator, LongInt(result), 0);
  result.Sort;
end;
