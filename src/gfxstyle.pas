{ gfxstyle.pas: drawing style parameters, system colors etc
  File maintainer: nvitya@freemail.hu

History:
}
unit gfxstyle;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase;
  
// named colors
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
  
  LastColorIndex         = $00000017;

type
{$ifdef Win32}{$else}
  TColorCacheRec = record
    rgb : dword;
    xcolor : dword;
  end;
{$endif}

  TGfxStyle = class
  public
    LabelFont1,
    LabelFont2,
    EditFont1,
    EditFont2,

    ListFont,

    GridFont,
    GridHeaderFont   : TGfxFont;

    constructor Create;
    destructor Destroy; override;

    function DefaultFont : TGfxFont;

    function GetNamedColorRGB(col : TGfxColor) : TGfxColor;

{$ifdef Win32}{$else}
    public
      NamedColors : array[1..LastColorIndex] of TColorCacheRec;

      function GetNamedXColor(col : TGfxColor) : dword;
      procedure AllocateNamedColors;
{$endif}

  end;

function guistyle : TGfxStyle;

procedure DrawButtonFace(canvas : TGfxCanvas; x,y,w,h : TGfxCoord);
procedure DrawDirectionArrow(canvas : TGfxCanvas; x,y,w,h : TGfxCoord; direction : integer);
  
implementation

{$ifdef Win32}{$else}
uses X, XLib;
{$endif}

var
  fguistyle : TGfxStyle;

function guistyle : TGfxStyle;
begin
  if fguistyle = nil then fguistyle := TGfxStyle.Create;
  result := fguistyle;
end;

{ TGfxStyle }

constructor TGfxStyle.Create;
begin
  LabelFont1 := GfxGetFont('Arial-10');
  LabelFont2 := GfxGetFont('Arial-10:bold');

  EditFont1 := GfxGetFont('Arial-10');
  EditFont2 := GfxGetFont('Courier-10');

  ListFont  := GfxGetFont('Arial-10');

  GridFont       := GfxGetFont('arial-9:antialias=false');
  GridHeaderFont := GfxGetFont('arial-9:bold:antialias=false');
  
{$ifdef Win32}{$else}
  AllocateNamedColors;
{$endif}
end;

function TGfxStyle.GetNamedColorRGB(col : TGfxColor) : TGfxColor;
begin
  case col of
//                                    $rrggbb
    clWindowBackground:     result := $C0C0C0;
    clBoxColor:             result := $FFFFFF;

    clShadow1:              result := $808080;
    clShadow2:              result := $202020;
    clHilite1:              result := $E0E0E0;
    clHilite2:              result := $FFFFFF;

    clText1:                result := $000000;
    clText2:                result := $000040;
    clText3:                result := $800000;
    clText4:                result := $404000;

    clSelection:            result := $000080;
    clSelectionText:        result := $FFFFFF;

    clInactiveSel:          result := $D0D0FF;
    clInactiveSelText:      result := $000000;

    clScrollBar:            result := $D0D0D0;
    clButtonFace:           result := $C0C0C0;

    clListBox:              result := $FFFFFF;

    clGridLines:            result := $A0A0A0;
    clGridHeader:           result := $E0E0E0;

    clWidgetFrame:          result := $000000;
    clInactiveWgFrame:      result := $A0A0A0;
    
    clTextCursor:           result := $000000;

    clChoiceListBox:        result := $E8E8E8;
    clUnset:		    result := clUnset;
  else
    result := 0;
  end;
end;

{$ifdef Win32}{$else}
procedure TGfxStyle.AllocateNamedColors;
var
  n : integer;
  xc : TXColor;
  c : dword;
begin
  for n:=1 to LastColorIndex do
  begin
    c := GetNamedColorRGB(longword($80000000) + longword(n));
    NamedColors[n].rgb := c;
    xc.blue  := (c and $000000FF) shl 8;
    xc.green := (c and $0000FF00);
    xc.red   := (c and $00FF0000) shr 8;

    XAllocColor(display, DefaultColorMap, @xc);
    NamedColors[n].xcolor := xc.pixel;
  end;
end;

function TGfxStyle.GetNamedXColor(col: TGfxColor): dword;
var
  c : dword;
begin
  c := col and $FFFF;
  if (c >= 1) and (c <= LastColorIndex) then
  begin
    result := NamedColors[c].xcolor;
  end
  else result := 0;
end;
{$endif}

destructor TGfxStyle.Destroy;
begin
  LabelFont1.Free;
  LabelFont2.Free;
  EditFont1.Free;
  EditFont2.Free;

  ListFont.Free;

  GridFont.Free;
  GridHeaderFont.Free;
end;

function TGfxStyle.DefaultFont : TGfxFont;
begin
  Result := LabelFont1;
end;

procedure DrawButtonFace(canvas : TGfxCanvas; x, y, w, h : TGfxCoord);
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

procedure DrawDirectionArrow(canvas : TGfxCanvas; x, y, w, h : TGfxCoord; direction : integer);
var
  peekx, peeky : TGfxCoord;
  basex, basey : TGfxCoord;
  side, margin : TGfxCoord;
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

initialization
begin
  fguistyle := nil;
end;

end.

