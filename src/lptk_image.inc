// lptk2_image.inc

{ TptkImage }

constructor TptkImage.Create;
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

destructor TptkImage.Destroy;
begin
  FreeImage;
  inherited Destroy;
end;

procedure TptkImage.FreeImage;
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

procedure TptkImage.LoadFromFile(AFileName : String);
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

procedure TptkImage.AllocateRGBImage(awidth, aheight: integer);
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

procedure TptkImage.Allocate2CImage(awidth, aheight: integer);
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

procedure TptkImage.AllocateMask;
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

procedure TptkImage.Invert;
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

procedure TptkImage.CreateMaskFromSample(x, y: integer);
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

procedure TptkImage.SetWindowsBitmap(pdata, pinfoheader: pointer; startscan, scanlines : longword);
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
function TptkImage.XImage: PXImage;
begin
  result := @FXimg;
end;

function TptkImage.XImageMask: PXImage;
begin
  result := @FXimgMask;
end;

{$endif}
