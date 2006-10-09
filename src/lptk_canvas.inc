// lptk2_canvas.inc

{ TptkCanvas }

constructor TptkCanvas.Create(winhandle : TptkWinHandle);
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
  rw : TptkWinHandle;
  x,y : integer;
  bw,d : longword;
  event_base, error_base : Integer;
begin
  FWin := winhandle;
  FDrawOnBuffer := False;
  FBackgroundColor := clBoxColor;
  Fgc := XCreateGc(display, FWin, 0, @GcValues);
  FCurFont := ptkstyle.DefaultFont;
  SetTextColor(clText1);

  FLineStyle := LineSolid;
  FLineWidth := 0;

//  FWinRect.Top := 0;
//  FWinRect.Left := 0;
//  XGetGeometry(display, FWin, @rw, @x,@y, @(FWinRect.width), @(FWinRect.height), @bw, @d);
  {$IFDEF BUFFERING}
  FBufferWin := XdbeAllocateBackBufferName(Display, FWin,nil);
  if FBufferWin > 0 then
     FXftDrawBuffer := XftDrawCreate(display, FBufferWin, XDefaultVisual(display, ptkDefaultScreen), XDefaultColormap(display, ptkDefaultScreen));
  {$ELSE}
         FBufferWin := -1;
         FXftDrawBuffer := nil;
  {$ENDIF}
  FXftDraw := XftDrawCreate(display, FWin, XDefaultVisual(display, ptkDefaultScreen), XDefaultColormap(display, ptkDefaultScreen));
  FClipRegion := XCreateRegion;
end;
{$endif}

procedure TptkCanvas.SetDrawOnBuffer(AValue : Boolean);
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
procedure TptkCanvas._ReCreateBuffer(AWidth, AHeight : Integer);
begin
  {$IFDEF BUFFERING}
    ptkFlush;
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

procedure TptkCanvas.MoveResizeWindow(x,y,w,h : TptkCoord);
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
     FXftDrawBuffer := XftDrawCreate(display, FBufferWin, XDefaultVisual(display, ptkDefaultScreen), XDefaultColormap(display, ptkDefaultScreen));
{$endif}
end;

procedure TptkCanvas.SwapBuffer;
{$IFDEF win32}
var
  ARect : TptkRect;
begin
  GetWinRect(ARect);
  BitBlt(Fgc, 0,0, ARect.Width+1, ARect.Height+1, FBufferGC, 0, 0, SRCCOPY);
//  _ReCreateBuffer(ARect.width, ARect.Height);
end;
{$ELSE}
{$IFDEF BUFFERING}
var
   SwapInfo : TXdbeSwapInfo;
   TmpWinHandle : TptkWinHandle;
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

destructor TptkCanvas.Destroy;
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

procedure TptkCanvas.SetFont(fnt : TptkFont);
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

procedure TptkCanvas.SetTextColor(cl : TptkColor);
begin
  FColorText := cl;
{$ifdef Win32}
  {$IFDEF BUFFERING}
  Windows.SetTextColor(FBufferGC, ptkColorToWin(cl));
  {$ENDIF}
  Windows.SetTextColor(Fgc, ptkColorToWin(cl));
{$else}
  SetXftColor(cl,FColorTextXft);
{$endif}
end;

procedure TptkCanvas.SetLineStyle(width : integer; dashed : boolean);
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

procedure TptkCanvas.SetColor(cl: TptkColor);
begin
{$ifdef Win32}
  DeleteObject(FBrush);
  DeleteObject(FPen);

  FWindowsColor := ptkColorToWin(cl);

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
  XSetForeGround(display, Fgc, ptkColorToX(cl) );
{$endif}
end;

procedure TptkCanvas.DrawString(x, y : TptkCoord; txt : widestring);
begin
  if length(txt) < 1 then exit;
{$ifdef Win32}
  if DrawOnBuffer then
    windows.TextOutW(FBufferGC, x,y{+FCurFont.Ascent}, @txt[1], length16(txt))
  else
    windows.TextOutW(Fgc, x,y{+FCurFont.Ascent}, @txt[1], length16(txt));
{$else}
  if DrawOnBuffer then
     XftDrawString16(FXftDrawBuffer, FColorTextXft, FCurFont.Handle, x,y+FCurFont.Ascent, @txt[1], Length(txt) )
  else
     XftDrawString16(FXftDraw, FColorTextXft, FCurFont.Handle, x,y+FCurFont.Ascent, @txt[1], Length(txt) )
{$endif}
end;

procedure TptkCanvas.FillRectangle(x, y, w, h : TptkCoord);
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

procedure TptkCanvas.FillRect(r: TptkRect);
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

procedure TptkCanvas.FillTriangle(x1, y1, x2, y2, x3, y3: TptkCoord);
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

procedure TptkCanvas.DrawRectangle(x, y, w, h: TptkCoord);
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
end;  
{$else}
begin
  if DrawOnBuffer then
     XDrawRectangle(display, FBufferWin, Fgc, x,y,w-1,h-1)
  else
     XDrawRectangle(display, Fwin, Fgc, x,y,w-1,h-1);   // transformed into polyline requests!
end;
{$endif}

procedure TptkCanvas.DrawRect(r : TptkRect);
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

procedure TptkCanvas.DrawLine(x1, y1, x2, y2 : TptkCoord);
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

procedure TptkCanvas.DrawSelectionRectangle(x, y, w, h : TptkCoord);
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
  hb := CreateSolidBrush(ptkColorToWin(ptkColorToRGB(clSelection) xor $00FFFFFF));
  SetROP2(Fgc, R2_XORPEN);
  Windows.FillRect(Fgc, wr, hb);
  SetROP2(Fgc, R2_COPYPEN);
  DeleteObject(hb);
}
{$else}
begin
  XSetForeGround(display, Fgc, ptkColorToX(ptkColorToRGB(clSelection) xor $00FFFFFF));
  XSetFunction(display, Fgc, GXxor);
  if DrawOnBuffer then
     XFillRectangle(display, FBufferWin, Fgc, x,y, w, h)
  else
      XFillRectangle(display, Fwin, Fgc, x,y, w, h);
  XSetForeGround(display, Fgc, 0);
  XSetFunction(display, Fgc, GXcopy);
{$endif}
end;

procedure TptkCanvas.SetClipRect(const rect : TptkRect);
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

function TptkCanvas.GetClipRect : TptkRect;
// added by aegluke
begin
  if DrawOnBuffer then
     result := FBufferClipRect
  else
    result := FClipRect;
end;

procedure TptkCanvas.AddClipRect(const rect: TptkRect);
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

procedure TptkCanvas.ClearClipRect;
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
  r : TptkRect;
begin
  GetWinRect(r);
  SetClipRect(r);
  if DrawOnBuffer then FBufferClipRectSet := False
  else FClipRectSet := False;
end;
{$endif}

procedure TptkCanvas.GetWinRect(var r: TptkRect);
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
  rw : TptkWinHandle;
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

procedure TptkCanvas.Clear(col : TptkColor);
{$ifdef Win32}
var
  r : windows.TRECT;
  br : HBRUSH;
begin
  GetClientRect(FWin,r);
  inc(r.Bottom,10);
  br := CreateSolidBrush(ptkColorToWin(col));
  if DrawOnBuffer then
    windows.FillRect(FBufferGC, r, br)
  else
    windows.FillRect(Fgc, r, br);
  DeleteObject(br);
end;
{$else}
var
  ACol : TptkColor;
  AWinRect : TptkRect;
begin
  ACol := FColor;
  SetColor(col);
  GetWinRect(AWinRect);
  FillRectangle(0,0,AWinRect.Width,AWinRect.Height);
  SetColor(ACol);
end;
{$endif}

procedure TptkCanvas.DrawImage(x, y: TptkCoord; img: TptkImage);
begin
  if img = nil then exit;
  DrawImagePart(x,y,img,0,0,img.width,img.height);
end;

procedure TptkCanvas.DrawImagePart(x, y: TptkCoord; img: TptkImage; xi, yi, w,h: integer);
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
  ARect : TptkRect;
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

procedure TptkCanvas.DrawButtonFace(x,y,w,h : TptkCoord);
begin
  ptkstyle.DrawButtonFace(self,x,y,w,h);
end;

procedure TptkCanvas.DrawControlFrame( x, y, w, h : TptkCoord);
begin
  ptkstyle.DrawControlFrame(self, x,y,w,h);
end;

procedure TptkCanvas.DrawDirectionArrow(x,y,w,h : TptkCoord; direction : integer);
begin
  ptkstyle.DrawDirectionArrow(self, x,y,w,h, direction);
end;

