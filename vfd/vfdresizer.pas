{ Copyright (c) 2003, Nagy Viktor 

 Widget resizer widgets
}

unit vfdresizer;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform;

type
  TwgResizer = class(TWidget)
  protected
    wgdesigner : TObject;

  public

    direction : integer;

    FDragging : boolean;
    FDragPosX, FDragPosY : TGfxCoord;

    constructor Create(ACompDesigner : TObject; adirection : integer); reintroduce;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleMouseMove(x,y : integer; btnstate : word; shiftstate : word); override;

    procedure Show;

    procedure RePaint; override;

  end;

implementation

uses vfddesigner, vfdforms;

{ TwgResizer }

constructor TwgResizer.Create(ACompDesigner : TObject; adirection : integer);
begin
  inherited Create(TWidgetDesigner(aCompDesigner).Widget.Parent);
  FBackgroundColor := $404040;
  wgdesigner := aCompDesigner;
  FDragging := false;
  width  := 5;
  height := 5;
  direction := adirection;
  case direction of
    1: MouseCursor := CUR_DIR_NWSE;
    2: MouseCursor := CUR_DIR_NS;
    3: MouseCursor := CUR_DIR_NESW;
    4: MouseCursor := CUR_DIR_EW;
    5: MouseCursor := CUR_DIR_NWSE;
    6: MouseCursor := CUR_DIR_NS;
    7: MouseCursor := CUR_DIR_NESW;
    8: MouseCursor := CUR_DIR_EW;
  end;
end;

procedure TwgResizer.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
begin
  //inherited;
  FDragging := true;
  FDragPosX := x;
  FDragPosy := y;
end;

procedure TwgResizer.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  //inherited;
  FDragging := false
end;

procedure TwgResizer.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: word);
var
  dx,dy : integer;
  gridc : integer;
  wgd : TWidgetDesigner;
begin
  //inherited;

  if not FDragging then Exit;
  dx := x - FDragPosX;
  dy := y - FDragPosY;

  wgd := TWidgetDesigner(wgdesigner);
  gridc := GridResolution;

  dx := dx - dx mod gridc;
  dy := dy - dy mod gridc;

  case direction of
    1:  wgd.Widget.MoveResizeBy(dx,dy,-dx,-dy);
    2:  wgd.Widget.MoveResizeBy(0,dy,0,-dy);
    3:  wgd.Widget.MoveResizeBy(0,dy,dx,-dy);
    4:  wgd.Widget.MoveResizeBy(0,0,dx,0);
    5:  wgd.Widget.MoveResizeBy(0,0,dx,dy);
    6:  wgd.Widget.MoveResizeBy(0,0,0,dy);
    7:  wgd.Widget.MoveResizeBy(dx,0,-dx,dy);
    8:  wgd.Widget.MoveResizeBy(dx,0,-dx,0);
  end;
  wgd.UpdateResizerPositions;
  wgd.FormDesigner.UpdatePropWin;
end;

procedure TwgResizer.Show;
begin
  DoShow;
end;

procedure TwgResizer.RePaint;
begin
  inherited;
  Canvas.Clear(FBackgroundColor);
end;

end.
