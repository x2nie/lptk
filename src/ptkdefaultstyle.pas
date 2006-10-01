{ $Id$ }

unit ptkdefaultstyle;

{$include lptk_config.inc}

interface

uses
  lptk;

type
  TptkDefaultStyle = class(TptkStyleBase)
  public
    // style initialization
    constructor Create; virtual;
    
  public
    // Global font objects

    DefaultFont : TptkFont;
    
    MenuFont,
    MenuAccelFont,
    MenuDisabledFont : TptkFont;
  public
    procedure DrawButtonFace(canvas : TptkCanvas; x,y,w,h : TptkCoord); override;
    procedure DrawControlFrame(canvas : TptkCanvas; x, y, w, h : TptkCoord); override;
    procedure DrawDirectionArrow(canvas : TptkCanvas; x,y,w,h : TptkCoord; direction : integer); override;
  end;

implementation

{ TptkDefaultStyle }

constructor TptkDefaultStyle.Create;
begin
  // Style description
  
  ptkSetNamedFont('Label1',        'Arial-10');
  ptkSetNamedFont('Label2',        'Arial-10:bold');

  ptkSetNamedFont('Edit1',         'Arial-10');
  ptkSetNamedFont('Edit2',         'Courier New-10');

  ptkSetNamedFont('List',          'Arial-10');

  ptkSetNamedFont('Grid',          'Arial-9:antialias=false');
  ptkSetNamedFont('GridHeader',    'Arial-9:bold:antialias=false');

  ptkSetNamedFont('Menu',          'Arial-10');
  ptkSetNamedFont('MenuAccel',     'Arial-10:bold');
  ptkSetNamedFont('MenuDisabled',  'Arial-10:italic');


  ptkSetNamedColor( clWindowBackground,    $D4D0C8);
  ptkSetNamedColor( clBoxColor,            $FFFFFF);

  ptkSetNamedColor( clShadow1,             $808080);
  ptkSetNamedColor( clShadow2,             $404040);
  ptkSetNamedColor( clHilite1,             $E0E0E0);
  ptkSetNamedColor( clHilite2,             $FFFFFF);

  ptkSetNamedColor( clText1,               $000000);
  ptkSetNamedColor( clText2,               $000040);
  ptkSetNamedColor( clText3,               $800000);
  ptkSetNamedColor( clText4,               $404000);

  ptkSetNamedColor( clSelection,           $000080);
  ptkSetNamedColor( clSelectionText,       $FFFFFF);

  ptkSetNamedColor( clInactiveSel,         $D0D0FF);
  ptkSetNamedColor( clInactiveSelText,     $000000);

  ptkSetNamedColor( clScrollBar,           $E8E4DB);
  ptkSetNamedColor( clButtonFace,          $D4D0C8);

  ptkSetNamedColor( clListBox,             $FFFFFF);

  ptkSetNamedColor( clGridLines,           $A0A0A0);
  ptkSetNamedColor( clGridHeader,          $E0E0E0);

  ptkSetNamedColor( clWidgetFrame,         $000000);
  ptkSetNamedColor( clInactiveWgFrame,     $A0A0A0);

  ptkSetNamedColor( clTextCursor,          $000000);

  ptkSetNamedColor( clChoiceListBox,       $E8E8E8);

  ptkSetNamedColor( clUnset,               $D0D0FF);

  ptkSetNamedColor( clMenuText,            $000000);
  ptkSetNamedColor( clMenuDisabled,        $909090);
  
  // Global Font Objects
  
  DefaultFont := ptkGetFont(ptkGetNamedFontDesc('Label1'));
  
  MenuFont := ptkGetFont(ptkGetNamedFontDesc('Menu'));
  MenuAccelFont := ptkGetFont(ptkGetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := ptkGetFont(ptkGetNamedFontDesc('MenuDisabled'));
end;


procedure TptkDefaultStyle.DrawButtonFace(canvas : TptkCanvas; x, y, w, h : TptkCoord);
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

procedure TptkDefaultStyle.DrawControlFrame(canvas : TptkCanvas; x, y, w, h : TptkCoord);
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

procedure TptkDefaultStyle.DrawDirectionArrow(canvas : TptkCanvas; x, y, w, h : TptkCoord; direction : integer);
var
  peekx, peeky : TptkCoord;
  basex, basey : TptkCoord;
  side, margin : TptkCoord;
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

end.

