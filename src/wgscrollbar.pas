{ wgscrollbar.pas: Scrollbar widget
  File maintainer: nvitya@freemail.hu

History:
}
unit wgscrollbar;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget;

type
  TScrollNotifyEvent = procedure(Sender: TObject; position : integer) of object;

  TwgScrollBar = class(TWidget)
  private
         FRePaint : Boolean;
  protected

    FSliderPos, FSliderLength : TGfxCoord;

    FSliderDragging : boolean;
    FSliderDragPos  : TGfxCoord;

    procedure DrawButton(x,y,w,h : TGfxCoord; direction : integer);
    procedure DrawSlider(recalc : boolean);

    procedure DrawButtonFace(x,y,w,h : TGfxCoord);
    procedure DrawDirectionArrow(x,y,w,h : TGfxCoord; direction : integer);

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    procedure PositionChange(d : integer);
  public
    OnScroll : TScrollNotifyEvent;

  public

    Orientation : TOrientation;

    Min, Max : integer;
    SliderSize : double;  // 0-1
    Position : integer;

    constructor Create(AOwner : TComponent); override;
  
    procedure RePaint; override;
    procedure RepaintSlider;
    
  end;

implementation

uses {$ifdef Win32}Windows,{$else}{$endif}
     popupwindow, gfxstyle;

{ TwgScrollBar }

constructor TwgScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := orVertical;
  FBackGroundColor := clScrollBar;
  Min := 0;
  Max := 100;
  Position := 10;
  SliderSize := 0.5;
  OnScroll := nil;
  FSliderPos := 0;
  FSliderDragging := false;
  FSliderLength := 10;
end;

procedure TwgScrollBar.RePaint;
begin
  if FWinHandle <= 0 then Exit;
//  inherited RePaint;
  //Canvas.Clear;
  Canvas.DrawOnBuffer := True;
  FRePaint := True;
  if Orientation = orVertical then
  begin
    DrawButton(0,0,width,width, 0);
    DrawButton(0,height-width,width,width, 1);
  end
  else
  begin
    DrawButton(0,0,height,height, 2);
    DrawButton(width-height,0,height,height, 3);
  end;
  
  DrawSlider(true);
  FRePaint := False;
  Canvas.SwapBuffer;
end;

procedure TwgScrollBar.RepaintSlider;
begin
  if WinHandle <= 0 then Exit;
  DrawSlider(true);
end;

procedure TwgScrollBar.DrawButton(x, y, w, h: TGfxCoord; direction: integer);
begin
  DrawButtonFace(x, y, w, h);
  DrawDirectionArrow(x,y,w,h, direction);
end;

procedure TwgScrollBar.DrawSlider(recalc : boolean);
var
  area : TGfxCoord;
  mm : TGfxCoord;
begin
  Canvas.DrawOnBuffer := FRePaint;
  if SliderSize > 1 then SliderSize := 1;

  canvas.SetColor(BackgroundColor);

  if Orientation = orVertical then
  begin
    canvas.FillRectangle(0,width,width,height-width-width);
    area := height - (width shl 1);
  end
  else
  begin
    canvas.FillRectangle(height,0,width-height-height,height);
    area := width - (height shl 1);
  end;
  
  if recalc then
  begin
    if Position > Max then Position := Max;
    if Position < min then Position := Min;
    
    FSliderLength := trunc(area*SliderSize);
    if FSliderLength < 8 then FSliderLength := 8;
    area := area - FSliderLength;
    mm := Max - Min;
    if mm = 0 then FSliderPos := 0
              else FSliderPos := Trunc(area * ((Position-min) / mm));
  end;

  if Orientation = orVertical then
    DrawButtonFace(0,width+FSliderPos,width,FSliderLength)
  else
    DrawButtonFace(height+FSliderPos,0,FSliderLength,height);
end;

procedure TwgScrollBar.DrawButtonFace(x, y, w, h: TGfxCoord);
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

procedure TwgScrollBar.DrawDirectionArrow(x, y, w, h: TGfxCoord; direction: integer);
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

procedure TwgScrollBar.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseDown(x, y, button, shiftstate);

  if Orientation = orVertical then
  begin
    if y <= width then PositionChange(-1)
    else if y >= height-width then PositionChange(1)
    else if (y >= width+FSliderPos) and (y <= width+FSliderPos+FSliderLength) then
    begin
      FSliderDragging := true;
      FSliderDragPos := y;
      DrawSlider(false);
    end;
  end
  else
  begin
    if x <= height then PositionChange(-1)
    else if x >= width-height then PositionChange(1)
    else if (x >= height+FSliderPos) and (x <= height+FSliderPos+FSliderLength) then
    begin
      FSliderDragging := true;
      {$ifdef Win32}
      SetCapture(WinHandle);
      {$else}{$endif}
      FSliderDragPos := x;
      DrawSlider(false);
    end;
  end;

end;

procedure TwgScrollBar.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  FSliderDragging := false;
  {$ifdef Win32}
  ReleaseCapture();
  if PopupListFirst <> nil then SetCapture(PopupListFirst.WinHandle);
  {$else}{$endif}
end;

procedure TwgScrollBar.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  d, area : integer;

  newp, ppos : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if (not FSliderDragging) or ((btnstate and 1) = 0) then
  begin
    FSliderDragging := false;
    Exit;
  end;

  if Orientation = orVertical then
  begin
    d := y - FSliderDragPos;
    FSliderDragPos := y;
    area := Height - (width shl 1) - FSliderLength;
  end
  else
  begin
    d := x - FSliderDragPos;
    FSliderDragPos := x;
    area := Width - (height shl 1) - FSliderLength;
  end;
  
//Writeln('d=',d);
  ppos := FSliderPos;
  FSliderPos := FSliderPos + d;
  if FSliderPos < 0 then FSliderPos := 0;
  if FSliderPos > area then FSliderPos := area;
  
  if ppos <> FSliderPos then DrawSlider(false);

  if area <> 0 then newp := Min + trunc((Max - Min) * FSliderPos/area)
               else newp := Min;

  if newp <> Position then
  begin
    Position := newp;
    if Assigned(OnScroll) then OnScroll(self, Position);
  end;
end;

procedure TwgScrollBar.PositionChange(d: integer);
begin
  Position := Position + d;
  if Position < Min then Position := Min;
  if Position > Max then Position := Max;
  
  DrawSlider(true);
  
  if Assigned(OnScroll) then OnScroll(self, Position);
  
end;

end.

