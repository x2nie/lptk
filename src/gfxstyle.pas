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

  TNamedFontItem = class
  public
    FontID : string;
    FontDesc : string;
    constructor Create(AFontID, AFontDesc : string);
  end;

  TGfxStyle = class
  private
    FNamedColors : array[0..255] of longword;
    FNamedFonts : TList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNamedColorRGB(col : TGfxColor) : TGfxColor;
    function GetNamedFontDesc(afontid : string) : string;
    function CreateNamedFontx(afontid : string) : TGfxFont;
  public

    procedure SetNamedColor(colorid, rgbvalue : longword);
    procedure SetNamedFont(afontid, afontdesc : string);
    
  public
    // Global font objects - never freed!
    
    DefaultFont : TGfxFont;
    
    MenuFont,
    MenuAccelFont,
    MenuDisabledFont : TGfxFont;
    
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
  FNamedFonts := TList.Create;
  
  // Style description
  
  SetNamedFont('Label1',        'Arial-10');
  SetNamedFont('Label2',        'Arial-10:bold');

  SetNamedFont('Edit1',         'Arial-10');
  SetNamedFont('Edit2',         'Courier New-10');

  SetNamedFont('List',          'Arial-10');

  SetNamedFont('Grid',          'Arial-9:antialias=false');
  SetNamedFont('GridHeader',    'Arial-9:bold:antialias=false');

  SetNamedFont('Menu',          'Arial-10');
  SetNamedFont('MenuAccel',     'Arial-10:bold');
  SetNamedFont('MenuDisabled',  'Arial-10:italic');


  SetNamedColor( clWindowBackground,    $D4D0C8);
  SetNamedColor( clBoxColor,            $FFFFFF);

  SetNamedColor( clShadow1,             $808080);
  SetNamedColor( clShadow2,             $404040);
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

  SetNamedColor( clScrollBar,           $E8E4DB);
  SetNamedColor( clButtonFace,          $D4D0C8);

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
  
  // Global Font Objects
  
  DefaultFont := GfxGetFont(GetNamedFontDesc('Label1'));
  
  MenuFont := GfxGetFont(GetNamedFontDesc('Menu'));
  MenuAccelFont := GfxGetFont(GetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := GfxGetFont(GetNamedFontDesc('MenuDisabled'));
end;

function TGfxStyle.GetNamedColorRGB(col : TGfxColor) : TGfxColor;
begin
  result := FNamedColors[col and $FF];
end;

function TGfxStyle.GetNamedFontDesc(afontid: string): string;
var
  n : integer;
begin
  n:=0;
  while (n < FNamedFonts.Count) and
        (lowercase(TNamedFontItem(FNamedFonts[n]).FontID) <> lowercase(afontid)) do inc(n);

  if n < FNamedFonts.Count then
  begin
    // found
    result := TNamedFontItem(FNamedFonts[n]).FontDesc;
  end
  else
  begin
    Writeln('GetNamedFontDesc error: "'+afontid+'" is missing. Default is used.');
    result := 'Arial-10';  // default font desc
  end;
end;

function TGfxStyle.CreateNamedFontx(afontid: string): TGfxFont;
begin
  result := GfxGetFont(GetNamedFontDesc(afontid));
end;

destructor TGfxStyle.Destroy;
var
  n : integer;
begin
  for n:=0 to FNamedFonts.Count-1 do TNamedFontItem(FNamedFonts[n]).Free;
  FNamedFonts.Free;
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

procedure TGfxStyle.SetNamedFont(afontid, afontdesc: string);
var
  n : integer;
begin
  n:=0;
  while (n < FNamedFonts.Count) and (lowercase(TNamedFontItem(FNamedFonts[n]).FontID) <> lowercase(afontid)) do inc(n);
  
  if n < FNamedFonts.Count then
  begin
    // already defined
    TNamedFontItem(FNamedFonts[n]).FontDesc := afontdesc;
  end
  else
  begin
    FNamedFonts.Add(TNamedFontItem.Create(afontid, afontdesc));
  end;
end;

{ TNamedFontItem }

constructor TNamedFontItem.Create(AFontID, AFontDesc: string);
begin
  FontId := afontid;
  FontDesc := afontdesc;
end;

initialization
begin
  fguistyle := nil;
end;

end.

