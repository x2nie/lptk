{ gfxbmpimage.pas: BMP format image parser
  File maintainer: nvitya@freemail.hu

History:
}

unit gfxbmpimage;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase;

procedure ReadImage_BMP(img : TGfxImage; bmp: pointer; bmpsize: longword);

function CreateBMPImage(bmp : pointer; bmpsize: longword) : TGfxImage;

implementation

function CreateBMPImage(bmp : pointer; bmpsize: longword) : TGfxImage;
begin
  result := TGfxImage.Create;
  ReadImage_BMP(result, bmp, bmpsize);
end;

type
  // Windows BMP format description:

  TBMPHeaderRec = packed record
    signature  : word;
    filesize   : longword;
    reserved   : longword;
    dataoffset : longword;
  end;
  PBMPHeaderRec = ^TBMPHeaderRec;

  TBMPInfoHeaderRec = packed record
    headersize : longword; // = 40
    width      : longword;
    height     : longword;
    planes     : word;
    bitcount   : word;
    compression: longword;
    imagesize  : longword; // bytes in the image data (after the color table)

    XpixelsPerM : longword;
    YpixelsPerM : longword;

    ColorsUsed  : longword;
    ColorsImportant : longword;
  end;
  PBMPInfoHeaderRec = ^TBMPInfoHeaderRec;

  // Then follows the Color Table if bitcount <= 8

  TBMPColorTableRec = packed record
    red   : byte;
    green : byte;
    blue  : byte;
    reserved : byte;
  end;

  // Then follows the image data
  // Every line padded to 32 bits
  // The lines stored bottom-up

type
  PByte = ^byte;
  Pword = ^word;
  Plongword = ^longword;

procedure ReadImage_BMP(img : TGfxImage; bmp: pointer; bmpsize: longword);
var
  bh : PBMPHeaderRec;
  ih : PBMPInfoHeaderRec;
  p : PByte;

  ppal : plongword;
  pcol : Plongword;
  palsize : integer;
  pdata : PByte;

  b : byte;
  bit : byte;
  bcnt : byte;

  linecnt  : longword;
  pixelcnt : longword;

  pdest : Plongword;
  
  depth : integer;

  function GetPalColor(cindex : longword) : longword;
  var
    pc : Plongword;
  begin
    pc := ppal;
    inc(pc,cindex);
    result := pc^;
  end;

  function ConvertColor(cin : longword) : longword;
  begin
{$ifndef Win32}
    if DisplayDepth = 16 then result := GfxColorToX(cin)
    else
{$endif}
      result := cin;
  end;

begin
  if img = nil then Exit;
  
  img.FreeImage;
  
  p := bmp;
  PByte(bh) := p;
  ppal := nil;
  if bh^.filesize <> bmpsize then Exit;

  pdata := bmp;
  inc(pdata,bh^.dataoffset);

  inc(p, SizeOf(TBMPHeaderRec));

  PByte(ih) := p;

  depth := ih^.bitcount;

  if depth > 1 then
  begin
    // color image
    img.AllocateRGBImage(ih^.width, ih^.height);
  end
  else
  begin
    // 1-bit bitmap
    //Writeln('the 1-BIT bitmaps aren''t supported yet !!!');
    img.Allocate2CImage(ih^.width, ih^.height);
    img.AllocateMask;
    //Exit;
  end;

{$ifdef Win32}
  // its the Windows native format
  img.SetWindowsBitmap(pdata, ih, 0, ih^.height);
  Exit;
{$else}

{$endif}

  //Writeln('width: ',img.width,' height: ',img.height,' depth: ',depth);
  //Writeln('compression: ',ih^.compression);

  inc(p, SizeOf(TBMPInfoHeaderRec));

  if ih^.bitcount <= 8 then
  begin
    // reading color palette
    case ih^.bitcount of
      1 : palsize := 2;
      4 : palsize := 16;
    else
      // 256
      palsize := 256;
    end;

    GetMem(ppal,palsize * SizeOf(longword));

    pcol := ppal;
    pixelcnt := 0;
    while integer(p) < integer(pdata) do
    begin
      pcol^ := convertcolor(Plongword(p)^);
      //Writeln('color: ',HexStr(pcol^,8));
      inc(pcol);
      inc(Plongword(p));
      inc(pixelcnt);
    end;

    //writeln(pixelcnt,' colors loaded.');

  end;

  pdest := img.ImageData;

  inc(pdest, img.Width*(img.Height-1));  // bottom-up line order

  p := bmp;
  inc(p, bh^.dataoffset);

  // reading the data...
  case ih^.bitcount of
  1:    begin
          // direct line transfer
          //writeln('reading 1-bit color bitmap');

          linecnt := 0;
          
          bcnt := img.width div 32;
          if (img.width and $1F) > 0 then inc(bcnt);
          
          pdest := img.ImageData;
          inc(pdest, bcnt*(img.Height-1));  // bottom-up line order
          
          repeat

            move(p^,pdest^,bcnt*4);

            inc(p, bcnt*4);
            dec(pdest,bcnt);

            inc(linecnt);
            
          until linecnt >= img.height;

          //Writeln(linecnt,' lines loaded.');
          
          move(img.ImageData^,img.MaskData^,img.ImageDataSize);

          img.Invert;

        end;
        
{ reading 1 bit into 32 bit:
  1:    begin
          // monochrome
          writeln('reading monochrome');

          linecnt := 0;

          repeat
            // parse one line..
            bit := $80;
            pixelcnt := 0;
            bcnt := 0;

            repeat
              if bit = $80 then
              begin
                b := p^;
                inc(p);
                inc(bcnt);
              end;

              if (b and bit) <> 0 then pdest^ := GetPalColor(1) else pdest^ := GetPalColor(0);
              inc(pdest);

              inc(pixelcnt);
              if bit = 1 then bit := $80 else bit := bit shr 1;

            until pixelcnt >= img.width;

            while (bcnt mod 4) <> 0 do
            begin
              inc(bcnt);
              inc(p);
            end;

            inc(linecnt);

            dec(pdest, img.width*2);  // go to next line

          until linecnt >= img.height;

          Writeln(linecnt,' lines loaded.');

        end;
}
  4:    begin
          //writeln('reading 4-bit color');

          linecnt := 0;

          repeat
            // parse one line..
            bit := 0;
            pixelcnt := 0;
            bcnt := 0;

            repeat
              if bit = 0 then
              begin
                b := (p^ shr 4) and $0F;
              end
              else
              begin
                b := p^ and $0F;
                inc(p);
                inc(bcnt);
              end;

              //write(HexStr(b,1));
              //if bit=1 then write(' ');

              pdest^ := GetPalColor(b);
              inc(pdest);

              inc(pixelcnt);

              bit := bit xor 1;

            until pixelcnt >= img.Width;

            //writeln;

            while (bcnt mod 4) <> 0 do
            begin
              inc(bcnt);
              inc(p);
            end;

            inc(linecnt);

            dec(pdest, img.Width*2);  // go to next line

          until linecnt >= img.Height;

          //Writeln(linecnt,' lines loaded.');

        end;

  8:    begin
          //writeln('reading 8-bit color');

          linecnt := 0;

          repeat
            // parse one line..
            pixelcnt := 0;

            repeat
              pdest^ := GetPalColor(p^);
              inc(p);
              inc(pdest);

              inc(pixelcnt);

            until pixelcnt >= img.Width;

            while (pixelcnt mod 4) <> 0 do
            begin
              inc(pixelcnt);
              inc(p);
            end;

            inc(linecnt);

            dec(pdest, img.Width*2);  // go to next line

          until linecnt >= img.Height;

          //Writeln(linecnt,' lines loaded.');

        end;


  24:   begin
          // truecolor
          //writeln('reading truecolor');

          linecnt := 0;

          repeat
            // parse one line..
            pixelcnt := 0;

            repeat
              pdest^ := p^;
              inc(p);
              pdest^ := pdest^ or (longword(p^) shl 8);
              inc(p);
              pdest^ := pdest^ or (longword(p^) shl 16);
              inc(p);

              inc(pdest);

              inc(pixelcnt);

            until pixelcnt >= img.Width;

            pixelcnt := img.Width * 3;

            while (pixelcnt mod 4) <> 0 do
            begin
              inc(pixelcnt);
              inc(p);
            end;

            inc(linecnt);

            dec(pdest, img.Width*2);  // go to next line

          until linecnt >= img.Height;

          //Writeln(linecnt,' lines loaded.');

        end;
  else
    writeln('Unsupported BMP format!');
  end;

  if ppal <> nil then FreeMem(ppal);

  //Writeln('BMP loaded.');
end;

end.

