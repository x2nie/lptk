{ wggrid.pas: Grid widget, inherited by wgdbgrid
  File maintainer: nvitya@freemail.hu

History:
}

unit wggrid;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget, wgscrollbar;

type

  TFocusChangeNotify = procedure(Sender : TObject; row,col : integer) of object;
  TRowChangeNotify = procedure(Sender : TObject; row : integer) of object;

  TwgGrid = class(TWidget)
  private
    procedure SetFocusCol(const Value: integer);
    procedure SetFocusRow(const Value: integer);
  protected
    FHeaderFont : TGfxFont;
    FFont : TGfxFont;

    FTemp : integer;

    FPrevRow, FPrevCol : integer;

    function GetColumnWidth(col : integer) : TGfxCoord; virtual;
    procedure SetColumnWidth(col : integer; cwidth : TgfxCoord); virtual;

  public
    FRowHeight : TGfxCoord;
    FHeaderHeight : TGfxCoord;
    FMargin : integer;

    FFocusCol : integer;
    FFocusRow : integer;

    FFirstRow : integer;
    FFirstCol : integer;

    HeadersOn : boolean;
    RowSelect : boolean;

    FVScrollBar : TwgScrollBar;

    FColResizing : boolean;
    FResizedCol  : integer;
    FDragPos : integer;

    property FocusCol : integer read FFocusCol write SetFocusCol;
    property FocusRow : integer read FFocusRow write SetFocusRow;

    property RowHeight : TGfxCoord read FRowHeight;
    
    property HeaderFont : TGfxFont read FHeaderFont;
    property Font : TGfxFont read FFont;

    property ColumnWidth[aCol : integer] : TgfxCoord read GetColumnWidth write SetColumnWidth;

    function ColumnCount : integer; virtual;
    function RowCount : integer; virtual;

    procedure FollowFocus;

    function VisibleLines : integer;
    function VisibleWidth : integer;

    procedure UpdateScrollBar;

    procedure DoShow; override;

    procedure VScrollBarMove(Sender: TObject; position : integer);

    procedure CheckFocusChange;

  public
    constructor Create(AOwner : TComponent); override;

    procedure RePaint; override;
    
    procedure Update;

    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); virtual;
    procedure DrawHeader(col : integer; rect : TGfxRect; flags : integer); virtual;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleWindowScroll(direction, amount : integer); override;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    
    procedure HandleDoubleClick(x,y : integer; btnstate, shiftstate : word); override;

    procedure HandleResize(dwidth, dheight : integer); override;

  public

    OnFocusChange : TFocusChangeNotify;
    OnRowChange   : TRowChangeNotify;
    
    OnDoubleClick : TMouseNotifyEvent;

  end;

implementation

uses gfxstyle; //, xlib, x, xutil;

{ TwgGrid }

function TwgGrid.ColumnCount: integer;
begin
  result := 7;
end;

function TwgGrid.RowCount: integer;
begin
  result := 24;
end;

function TwgGrid.GetColumnWidth(col: integer): TGfxCoord;
begin
  if col = 2 then result := FTemp else result := 60+(col*16);
end;

procedure TwgGrid.SetColumnWidth(col : integer; cwidth: TgfxCoord);
begin
  if (col = 2) and (cwidth <> FTemp) then
  begin
    FTemp := cwidth;
    Repaint;
  end;
end;

procedure TwgGrid.FollowFocus;
var
  n : integer;
  w : TGfxCoord;
begin
  if (RowCount > 0) and (FFocusRow < 1) then FFocusRow := 1;
  if FFocusRow > RowCount then FFocusRow := RowCount;

  if (ColumnCount > 0) and (FFocusCol < 1) then FFocusCol := 1;
  if FFocusCol > ColumnCount then FFocusCol := ColumnCount;

  if FFirstRow < 1 then FFirstRow := 1;
  if FFirstCol < 1 then FFirstCol := 1;

  if FFocusRow < FFirstRow then
  begin
    FFirstRow := FFocusRow;
  end
  else
  begin
    if FFirstRow + VisibleLines - 1 < FFocusRow then
    begin
      FFirstRow := FFocusRow - VisibleLines + 1;
    end;
  end;
  
  if FFocusCol < FFirstCol then
  begin
    FFirstCol := FFocusCol;
  end
  else
  begin
    w := 0;
    for n := FFocusCol downto FFirstCol do
    begin
      w := w + ColumnWidth[n]+1;
      if w > VisibleWidth then
      begin
        if n = FFocusCol then FFirstCol := n else FFirstCol := n+1;
        break;
      end;
    end;
  end;
  
  UpdateScrollBar;
end;

function TwgGrid.VisibleLines: integer;
var
  hh : integer;
begin
  if HeadersOn then hh := FHeaderHeight+1 else hh := 0;
  result := (self.Height - 2*FMargin - hh) div (FRowHeight+1)
end;

function TwgGrid.VisibleWidth: integer;
var
  sw : integer;
begin
  if FVScrollBar.Visible then sw := FVScrollBar.width-1 else sw := 0;
  result := self.Width - FMargin*2 - sw;
end;

procedure TwgGrid.UpdateScrollBar;
begin

  FVScrollBar.Visible := (RowCount > VisibleLines);

  if FVScrollBar.Visible then
  begin
    FVScrollBar.Min := 1;
    FVScrollBar.SliderSize := VisibleLines / RowCount;
    FVScrollBar.Max := RowCount-VisibleLines+1;
    FVScrollBar.Position := FFirstRow;

    if FVScrollBar.WinHandle > 0 then
      FVScrollBar.RePaintSlider;
  end;

end;

procedure TwgGrid.DoShow;
begin
  FVScrollBar.SetDimensions(width-18,0,18,height);
  inherited DoShow;
  UpdateScrollBar;
  CheckFocusChange;
end;

procedure TwgGrid.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstRow <> position then
  begin
    FFirstRow := position;
    repaint;
  end;
end;

constructor TwgGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable := true;
  FFocusCol := 1;
  FPrevCol := 0; //FFocusCol;
  FFocusRow := 1;
  FPrevRow := 0; //FFocusRow;
  FFirstRow := 1;
  FFirstCol := 1;
  FMargin := 1;
  FFont := guistyle.GridFont;
  FHeaderFont := guistyle.GridHeaderFont;
  HeadersOn := true;
  RowSelect := false;
  
  FRowHeight := FFont.Height + 2;
  FHeaderHeight := FHeaderFont.Height + 2;

  FBackgroundColor := clBoxColor;
  
  FColResizing := false;
  
  FVScrollBar := TwgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := {$ifdef FPC}@{$endif}VScrollBarMove;

  FTemp := 50;

  OnFocusChange := nil;
  OnRowChange := nil;
  OnDoubleClick := nil;
end;

procedure TwgGrid.RePaint;
var
  row,col : integer;
  r,r2 : TGfxRect;
  clr : TGfxRect;
begin
  if WinHandle <= 0 then Exit;
//  inherited RePaint;

//  Canvas.Clear;
  canvas.ClearClipRect;
  if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
  Canvas.DrawRectangle(0,0,width,height);

  clr.SetRect(FMargin,FMargin, VisibleWidth, height-2*FMargin);
  r := clr;
  
  if (ColumnCount > 0) and HeadersOn then
  begin
    // Drawing headers
    r.Height := RowHeight;

    Canvas.SetFont(FHeaderFont);
    for col := FFirstCol to ColumnCount do
    begin
      r.width := ColumnWidth[col];

      canvas.SetClipRect(clr);

      // drawing grid lines
      canvas.SetColor(clGridLines);
      canvas.DrawLine(r.Left,r.Bottom+1,r.Right+1,r.Bottom+1);
      canvas.DrawLine(r.Right+1,r.Top,r.Right+1,r.Bottom+1);

      canvas.AddClipRect(r);
      canvas.SetColor(clGridHeader);
      canvas.FillRect(r);

      canvas.SetTextColor(clText1);
      DrawHeader(col,r,0);

      r.Left := r.Left + r.Width + 1;

      if r.Left >= clr.Right then break;
    end;
    
    r.Top := r.Top + r.Height + 1;
  end;

  if (RowCount > 0) and (ColumnCount > 0) then
  begin
    // Drawing items
    Canvas.SetFont(FFont);

    r.Height := RowHeight;

    for row := FFirstRow to RowCount do
    begin
      r.Left := FMargin;
      for col := FFirstCol to ColumnCount do
      begin
        r.width := ColumnWidth[col];

        canvas.SetClipRect(clr);

        // drawing grid lines
        canvas.SetColor(clGridLines);
        canvas.DrawLine(r.Left,r.Bottom+1,r.Right+1,r.Bottom+1);
        canvas.DrawLine(r.Right+1,r.Top,r.Right+1,r.Bottom+1);

        canvas.AddClipRect(r);

        if (row = FFocusRow) and (RowSelect or (col = FFocusCol)) then
        begin
          if FFocused then
          begin
            canvas.SetColor(clSelection);
            canvas.SetTextColor(clSelectionText);
          end
          else
          begin
            canvas.SetColor(clInactiveSel);
            canvas.SetTextColor(clInactiveSelText);
          end;
        end
        else
        begin
          canvas.SetColor(BackgroundColor);
          canvas.SetTextColor(clText1);
        end;

        canvas.FillRect(r);

        DrawCell(row,col,r,0);

        r.Left := r.Left + r.Width + 1;

        if r.Left >= clr.Right then break;
      end;

      r.Top := r.Top + r.Height + 1;

      if r.Top >= clr.Bottom then break;

    end;
  end; // item drawing 

  canvas.SetClipRect(clr);
  canvas.SetColor(BackgroundColor);

  // clearing after the last column
  if r.Left <= clr.Right then
  begin
    r2.Left := r.Left;
    r2.Top := clr.Top;
    r2.SetRight(clr.Right);
    r2.Height := clr.Height;
    canvas.FillRect(r2);
  end;

  // clearing after the last row
  if r.Top <= clr.Bottom then
  begin
    r.Left := clr.Left;
    r.Width := clr.Width;
    r.SetBottom(clr.Bottom);
    canvas.FillRect(r);
  end;

end;

procedure TwgGrid.Update;
begin
  UpdateScrollBar;
  FollowFocus;
  RePaint;
end;

procedure TwgGrid.DrawCell(row, col: integer; rect: TGfxRect; flags: integer);
var
  s : string16;
begin
  s := Str8To16('Cellg('+IntToStr(row)+','+IntToStr(col)+')');
  canvas.DrawString16(rect.left+1,FFont.Ascent+rect.top+1,s);
  //Canvas.DrawRectangle(rect.left+1,rect.top+1,rect.width-1,rect.height-1);
end;

procedure TwgGrid.DrawHeader(col: integer; rect: TGfxRect; flags: integer);
var
  s : string16;
begin
  s := Str8To16('Head '+IntToStr(col));
  canvas.DrawString16(rect.left + (rect.width div 2) - (FHeaderFont.TextWidth16(s) div 2),
                     FHeaderFont.Ascent+rect.top+1, s);
  //Canvas.DrawRectangle(rect.left+1,rect.top+1,rect.width-1,rect.height-1);
end;

procedure TwgGrid.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  w : integer;
begin
  consumed := true;
  case keycode of
{
    $FF1B: begin
             halt(0);  // for testing..
           end;
}
    KEY_UP:
           begin // up
             if FFocusRow > 1 then
             begin
               dec(FFocusRow);
               FollowFocus;
               RePaint;
               //DoChange;
             end;
           end;
    KEY_DOWN:
           begin // down
             if FFocusRow < RowCount then
             begin
               inc(FFocusRow);
               FollowFocus;
               RePaint;
               //DoChange;
             end;
           end;

    KEY_LEFT:
           begin // left
             if RowSelect then FFocusCol := FFirstCol;
             if FFocusCol > 1 then
             begin
               dec(FFocusCol);
               FollowFocus;
               RePaint;
               //DoChange;
             end;
           end;
    KEY_RIGHT:
           begin // right
             if RowSelect then
             begin
               w := 0;
               FFocusCol := FFirstCol;
               while FFocusCol < ColumnCount do
               begin
                 inc(w, ColumnWidth[FFocusCol]+1);
                 if w + ColumnWidth[FFocusCol+1]+1 > VisibleWidth then Break;
                 inc(FFocusCol);
               end;
             end;

             if FFocusCol < ColumnCount then
             begin
               inc(FFocusCol);
               FollowFocus;
               RePaint;
               //DoChange;
             end;
           end;
    KEY_PGUP:
           begin // pgup
             dec(FFocusRow,VisibleLines);
             if FFocusRow < 1 then FFocusRow := 1;
             FollowFocus;
             RePaint;
             //DoChange;
           end;
    KEY_PGDN:
           begin // pgdown
             inc(FFocusRow,VisibleLines);
             if FFocusRow > RowCount then FFocusRow := RowCount;
             FollowFocus;
             RePaint;
             //DoChange;
           end;
    KEY_HOME:
           begin // home
             FFocusCol := 1;
             FollowFocus;
             RePaint;
             //DoChange;
           end;
    KEY_END:
           begin // end
             FFocusCol := ColumnCount;
             FollowFocus;
             RePaint;
             //DoChange;
           end;
{
    $FF0D: begin // enter
             DoSelect;
           end;
}
  else
    consumed := false;
  end;
  
  CheckFocusChange;
end;

procedure TwgGrid.HandleWindowScroll(direction, amount: integer);
var
  pfl, pfc : integer;
begin
  inherited HandleWindowScroll(direction, amount);

  pfl := FFirstRow;
  pfc := FFirstCol;

  if direction = 0 then
  begin
    dec(FFirstRow, amount);
  end;
  if direction = 1 then
  begin
    inc(FFirstRow, amount);
  end;
  if FFirstRow > RowCount - VisibleLines + 1 then FFirstRow := RowCount - VisibleLines + 1;
  if FFirstRow < 1 then FFirstRow := 1;
  
  if Direction = 2 then
  begin
    if FFirstCol > 1 then dec(FFirstCol);
  end;
  if Direction = 3 then
  begin
    if FFirstCol < ColumnCount then inc(FFirstCol);
  end;

  if (pfl <> FFirstRow) or (pfc <> FFirstCol) then
  begin
    UpdateScrollBar;
    Repaint;
  end;

end;

procedure TwgGrid.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
var
  hh : integer;
  n : integer;

  cw : integer;
  prow, pcol : integer;
begin
  inherited HandleMouseDown(x, y, button, shiftstate);
  
  if (ColumnCount < 0) or (RowCount < 1) then Exit;

  pcol := FFocusCol;
  prow := FFocusRow;

  // searching the appropriate character position

  if HeadersOn then hh := FHeaderHeight else hh := 0;
  
  if HeadersOn and (y <= FMargin + hh) then
  begin
    //Writeln('header click...');
    
    cw := 0;
    for n:=FFirstCol to ColumnCount do
    begin
      inc(cw, ColumnWidth[n]+1);
      if (FMargin+cw - 4 <= x) and (x <= FMargin+cw + 4) then
      begin
        Writeln('column resize...');
        //FFocusCol := n;
        
        FColResizing := true;
        FResizedCol := n;
        FDragPos := x;

        break;
      end
      else if (cw > FMargin + VisibleWidth) and (x >= FMargin + VisibleWidth-4) then
      begin
        FColResizing := true;
        FResizedCol := n;
        FDragPos := x;
        SetColumnWidth(FResizedCol, ColumnWidth[FResizedCol] - (cw+FMargin-x) );

        break;

      end;
      
      if cw > VisibleWidth then Break;
    end;
    
  end
  else
  begin
    FFocusRow := FFirstRow + (y - FMargin - hh) div (FRowHeight+1);
    if FFocusRow > RowCount then FFocusRow := RowCount;
    
    cw := 0;
    for n:=FFirstCol to ColumnCount do
    begin
      inc(cw, ColumnWidth[n]+1);
      if FMargin+cw >= x then
      begin
        FFocusCol := n;
        break;
      end;
    end;
  end;

  if (prow <> FFocusRow) or (pcol <> FFocusCol) then
  begin
    FollowFocus;
    Repaint;
  end;
  
  if FColResizing then
  begin
    MouseCursor := CUR_DIR_EW;
  end;

  CheckFocusChange;
end;

procedure TwgGrid.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);

//design functionality:
  if FColResizing then Writeln('Column ',FResizedCol,' width = ',ColumnWidth[FResizedCol]);

  FColResizing := False;
  MouseCursor := CUR_DEFAULT;
end;

procedure TwgGrid.HandleMouseMove(x, y: integer; btnstate, shiftstate : word);
var
  hh, cw, n : integer;
  cresize : boolean;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if (ColumnCount < 0) or (RowCount < 1) then Exit;

  if FColResizing then
  begin
    if (btnstate and 1) = 0 then FColResizing := false
    else
    begin
      SetColumnWidth(FResizedCol, ColumnWidth[FResizedCol]+x-FDragPos);
      FDragPos := x;
    end;
  end
  else if HeadersOn then
  begin
    cresize := false;
    hh := FHeaderHeight;

    if (y <= FMargin + hh) then
    begin
      cw := 0;
      for n:=FFirstCol to ColumnCount do
      begin
        inc(cw, ColumnWidth[n]+1);
        if ((FMargin+cw - 4 <= x) and (x <= FMargin+cw + 4)) or
           (cw > FMargin + VisibleWidth) and (x >= FMargin + VisibleWidth-4)   then
        begin
          cresize := true;
          break;
        end;

        if cw > VisibleWidth then Break;
      end;

    end;

    if cresize then MouseCursor := CUR_DIR_EW else MouseCursor := CUR_DEFAULT;

  end;

end;

procedure TwgGrid.HandleDoubleClick(x, y: integer; btnstate, shiftstate: word);
begin
  inherited HandleDoubleClick(x, y, btnstate, shiftstate);
  
  if Assigned(OnDoubleClick) then OnDoubleClick(self, x,y, btnstate, shiftstate);
end;

procedure TwgGrid.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);

  FVScrollBar.Height := Height;
  FVScrollBar.left := Width - FVScrollBar.width;
  FVScrollBar.UpdateWindowPosition;

  UpdateScrollBar;
end;


procedure TwgGrid.CheckFocusChange;
begin

  if ((FPrevCol <> FFocusCol) and not RowSelect) or (FPrevRow <> FFocusRow) then
    if Assigned(OnFocusChange) then OnFocusChange(self,FFocusRow,FFocusCol);

  if (FPrevRow <> FFocusRow) then
    if Assigned(OnRowChange) then OnRowChange(self,FFocusRow);

  FPrevCol := FFocusCol;
  FPrevRow := FFocusRow;
end;

procedure TwgGrid.SetFocusCol(const Value: integer);
begin
  FFocusCol := Value;
  if FFocusCol < 1 then FFocusCol := 1;
  if FFocusCol > ColumnCount then FFocusCol := ColumnCount;
  FollowFocus;
  CheckFocusChange;
end;

procedure TwgGrid.SetFocusRow(const Value: integer);
begin
  FFocusRow := Value;
  if FFocusRow < 1 then FFocusRow := 1;
  if FFocusRow > RowCount then FFocusRow := RowCount;
  FollowFocus;
  CheckFocusChange;
  if FWinHandle > 0 then Update;
end;

end.

