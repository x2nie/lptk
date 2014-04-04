unit lp_scrollbar;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_widget;

type
  TScrollNotifyEvent = procedure(Sender: TObject; position : integer) of object;

  TwgScrollBar = class(TpgfWidget)
  protected

    FSliderPos, FSliderLength : TpgfCoord;

    FSliderDragging : boolean;
    FSliderDragPos, FSliderDragStart : TpgfCoord;

    procedure DrawButton(x,y,w,h : TpgfCoord; const imgname : string);
    procedure DrawSlider(recalc : boolean);

    procedure HandleLMouseDown(x,y : integer; shiftstate : word); override;
    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    procedure HandlePaint; override;

    procedure PositionChange(d : integer);
  public
    OnScroll : TScrollNotifyEvent;

  public

    Orientation : TOrientation;

    Min, Max : integer;
    SliderSize : double;  // 0-1
    Position : integer;

    constructor Create(AOwner : TComponent); override;

    procedure RepaintSlider;

  end;

implementation

{ TwgScrollBar }

constructor TwgScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := orVertical;
  Min := 0;
  Max := 100;
  Position := 10;
  SliderSize := 0.5;
  OnScroll := nil;
  FSliderPos := 0;
  FSliderDragging := false;
  FSliderLength := 10;
end;

procedure TwgScrollBar.HandlePaint;
begin
  Canvas.BeginDraw;

  if Orientation = orVertical then
  begin
    DrawButton(0,0,width,width, 'sys.sb.up');
    DrawButton(0,height-width,width,width, 'sys.sb.down');
  end
  else
  begin
    DrawButton(0,0,height,height, 'sys.sb.left');
    DrawButton(width-height,0,height,height, 'sys.sb.right');
  end;

  DrawSlider(true);

  Canvas.EndDraw;
end;

procedure TwgScrollBar.RepaintSlider;
begin
  if not HasHandle then Exit;

  DrawSlider(true);
end;

procedure TwgScrollBar.DrawButton(x, y, w, h: TpgfCoord; const imgname : string);
var
  img : TpgfImage;
begin
  canvas.DrawButtonFace(x, y, w, h);
  canvas.SetColor(clText1);
  img := pgfImages.GetImage(imgname);
  if img <> nil then
  begin
    canvas.DrawImage(x + w div 2 - (img.Width div 2), y + h div 2 - (img.Height div 2), img);
    //canvas.DrawDirectionArrow(x,y,w,h, direction);
  end;
end;

procedure TwgScrollBar.DrawSlider(recalc : boolean);
var
  area : TpgfCoord;
  mm : TpgfCoord;
begin
  canvas.BeginDraw;

  if SliderSize > 1 then SliderSize := 1;

  canvas.SetColor(clScrollBar);

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
  begin
    canvas.DrawButtonFace(0,width+FSliderPos,width,FSliderLength);

    canvas.EndDraw(0,width,width,height-width-width);
  end
  else
  begin
    canvas.DrawButtonFace(height+FSliderPos,0,FSliderLength,height);

    canvas.EndDraw(height,0,width-height-height,height);
  end;
end;

procedure TwgScrollBar.HandleLMouseDown(x, y: integer; shiftstate: word);
begin
  inherited;

  if Orientation = orVertical then
  begin
    if y <= width then PositionChange(-1)
    else if y >= height-width then PositionChange(1)
    else if (y >= width+FSliderPos) and (y <= width+FSliderPos+FSliderLength) then
    begin
      FSliderDragging := true;
      FSliderDragPos := y;
    end;
  end
  else
  begin
    if x <= height then PositionChange(-1)
    else if x >= width-height then PositionChange(1)
    else if (x >= height+FSliderPos) and (x <= height+FSliderPos+FSliderLength) then
    begin
      FSliderDragging := true;
      FSliderDragPos := x;
    end;
  end;

  if FSliderDragging then
  begin
(*
    {$ifdef Win32}
    SetCapture(WinHandle);
    {$else}{$endif}
*)
    FSliderDragStart := FSliderPos;
    DrawSlider(false);
  end;
end;

procedure TwgScrollBar.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  inherited;
  FSliderDragging := false;
(*
  {$ifdef Win32}
  ReleaseCapture();
  if PopupListFirst <> nil then SetCapture(PopupListFirst.WinHandle);
  {$else}{$endif}
*)
end;

procedure TwgScrollBar.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  d, area : integer;

  newp, ppos : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if (not FSliderDragging) or ((btnstate and MOUSE_LEFT) = 0) then
  begin
    FSliderDragging := false;
    Exit;
  end;

  if Orientation = orVertical then
  begin
    d := y - FSliderDragPos;
    area := Height - (width shl 1) - FSliderLength;
  end
  else
  begin
    d := x - FSliderDragPos;
    area := Width - (height shl 1) - FSliderLength;
  end;

//Writeln('d=',d);
  ppos := FSliderPos;

  FSliderPos := FSliderDragStart + d;

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

