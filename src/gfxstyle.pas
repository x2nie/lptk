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

const
  UserNamedColorStart = 128;

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

  clMenuText             = $80000019;
  clMenuDisabled         = $8000001A;

type

  TGfxStyle = class
  private
    FNamedColors : array[0..255] of longword;
  public
    LabelFont1,
    LabelFont2,
    EditFont1,
    EditFont2,

    ListFont,

    GridFont,
    GridHeaderFont   : TGfxFont;

    MenuFont,
    MenuAccelFont,
    MenuDisabledFont : TGfxFont;

    constructor Create;
    destructor Destroy; override;

    function DefaultFont : TGfxFont;

    function GetNamedColorRGB(col : TGfxColor) : TGfxColor;

  public

    procedure SetNamedColor(colorid, rgbvalue : longword);

  end;

function guistyle : TGfxStyle;

procedure DrawButtonFace(canvas : TGfxCanvas; x,y,w,h : TGfxCoord);
procedure DrawControlFrame(canvas : TGfxCanvas; x, y, w, h : TGfxCoord);
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
  EditFont2 := GfxGetFont('Courier New-10');

  ListFont  := GfxGetFont('Arial-10');

  GridFont       := GfxGetFont('arial-9:antialias=false');
  GridHeaderFont := GfxGetFont('arial-9:bold:antialias=false');

  MenuFont := GfxGetFont('arial-10');
  MenuAccelFont := GfxGetFont('arial-10:bold');
  MenuDisabledFont := GfxGetFont('arial-10:italic');


  SetNamedColor( clWindowBackground,    $C0C0C0);
  SetNamedColor( clBoxColor,            $FFFFFF);

  SetNamedColor( clShadow1,             $808080);
  SetNamedColor( clShadow2,             $202020);
  SetNamedColor( clHilite1,             $E0E0E0);
  SetNamedColor( clHilite2,             $FFFFFF);

  SetNamedColor( clText1,               $000000);
  SetNamedColor( clText2,               $000040);
  SetNamedColor( clText3,               $800000);
  SetNamedColor( clText4,               $404000);

  SetNamedColor( clSelection,           $000080);
  SetNamedColor( clSelectionText,       $FFFFFF);

  SetNamedColor( clInactiveSel,         $D0D0FF);
  SetNamedColor( clInactiveSelText,     $000000);

  SetNamedColor( clScrollBar,           $D0D0D0);
  SetNamedColor( clButtonFace,          $C0C0C0);

  SetNamedColor( clListBox,             $FFFFFF);

  SetNamedColor( clGridLines,           $A0A0A0);
  SetNamedColor( clGridHeader,          $E0E0E0);

  SetNamedColor( clWidgetFrame,         $000000);
  SetNamedColor( clInactiveWgFrame,     $A0A0A0);

  SetNamedColor( clTextCursor,          $000000);

  SetNamedColor( clChoiceListBox,       $E8E8E8);

  SetNamedColor( clUnset,               $D0D0FF);

  SetNamedColor( clMenuText,            $000000);
  SetNamedColor( clMenuDisabled,        $909090);
end;

function TGfxStyle.GetNamedColorRGB(col : TGfxColor) : TGfxColor;
begin
  result := FNamedColors[col and $FF];
end;

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

procedure DrawControlFrame(canvas : TGfxCanvas; x, y, w, h : TGfxCoord);
begin
  canvas.SetColor(clShadow1);
  Canvas.DrawLine(x,y, x+w-1,y);
  Canvas.DrawLine(x,y+h-1, x,y);

  canvas.SetColor(clShadow2);
  Canvas.DrawLine(x+1,y+1,  x+w-2,y+1);
  Canvas.DrawLine(x+1,y+h-2, x+1,y+1);

  canvas.SetColor(clHilite2);
  Canvas.DrawLine(x+1,y+h-1, x+w-1,y+h-1);
  Canvas.DrawLine(x+w-1,y+1, x+w-1,y+h-1);

  canvas.SetColor(clHilite1);
  Canvas.DrawLine(x+2,y+h-2, x+w-2,y+h-2);
  Canvas.DrawLine(x+w-2,y+2, x+w-2,y+h-2);
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

procedure TGfxStyle.SetNamedColor(colorid, rgbvalue: longword);
var
  i : longword;
begin
  if (colorid and $80000000) = 0 then Exit;

  i := colorid and $FF;

  FNamedColors[i] := rgbvalue;
end;

initialization
begin
  fguistyle := nil;
end;

end.

