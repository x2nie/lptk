{ wggrid.pas: Grid widget, inherited by wgdbgrid
  File maintainer: nvitya@freemail.hu

History:
}

// $Log: wggrids.pas,v $
// Revision 1.1  2012/09/16 16:36:01  nvitya
// bjorn changes
//
// Revision 1.20  2004/05/06 17:35:47  nvitya
// division by zero error fix
//
// Revision 1.19  2004/04/24 23:59:58  nvitya
// Font handling changes
//
// Revision 1.18  2004/04/24 12:53:43  nvitya
// better control frames
//
// Revision 1.17  2004/04/22 10:12:21  nvitya
// multiple updates
//
// Revision 1.16  2003/12/23 17:14:15  aegluke
// DrawGrid-changes
//
// Revision 1.15  2003/12/23 08:24:23  aegluke
// avoid artifacts on directory change
//
// Revision 1.14  2003/12/20 15:13:01  aegluke
// wgFileGrid-Changes
//
// Revision 1.13  2003/12/11 12:56:43  aegluke
// Bugfix VisibleLines, FHScrollbar-Dependend by now
//
//
// Revision 1.11  2003/12/11 11:57:50  aegluke
// Scrollbar-Changes

{ 11.09.2012 pbm put wgGrid, wgCustomGrid, and wgStringGrid in the same file called wgGrids
* renamed TwgGrids to TwgGridsImpl because it is Ansestor to TwgCustomGrid - not a decendant
}

unit wggrids;

{$include pgf_config.inc}
{$define Delphi} //* pbm temp solution to @ and not @

interface

uses
  Classes, SysUtils, pgf_defs, pgf_main, pgf_widget, wgscrollbar;

type

  TFocusChangeNotify = procedure(Sender : TObject; row,col : integer) of object;
  TRowChangeNotify = procedure(Sender : TObject; row : integer) of object;

  TwgGridImpl = class(TpgfWidget)
  private
    FDrawGrid : boolean;
    function GetFontName: string;
    function GetHeaderFontName: string;
    procedure SetFocusCol(const Value: integer);
    procedure SetFocusRow(const Value: integer);
    procedure SetFontName(const AValue: string);
    procedure SetHeaderFontName(const AValue: string);
  protected
    FHeaderFont : TpgfFont;
    FFont : TpgfFont;
    FBackgroundColor: TpgfColor;

    FTemp : integer;

    FPrevRow, FPrevCol : integer;

    function GetColumnWidth(col : integer) : TpgfCoord; virtual;
    procedure SetColumnWidth(col : integer; cwidth : TpgfCoord); virtual;
    procedure SetDrawGrid(AValue : Boolean);
    function GetColumnCount : integer; virtual;
    function GetRowCount : integer; virtual;
    procedure DrawCell(row,col : integer; rect : TpgfRect; flags : integer); virtual;
    procedure DrawHeader(col : integer; rect : TpgfRect; flags : integer); virtual;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleLMouseDown(x,y : integer; shiftstate : word); override;
    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleDoubleClick(x,y : integer; btnstate, shiftstate : word); override;
    procedure FollowFocus; virtual;

  public
    FRowHeight : TpgfCoord;
    FHeaderHeight : TpgfCoord;
    FMargin : integer;

    FFocusCol : integer;
    FFocusRow : integer;

    FFirstRow : integer;
    FFirstCol : integer;

    HeadersOn : boolean;
    RowSelect : boolean;

    FVScrollBar : TwgScrollBar;
    FHScrollBar : TwgScrollBar;

    FColResizing : boolean;
    FResizedCol  : integer;
    FDragPos : integer;

    property DrawGrid : boolean read FDrawGrid write SetDrawGrid;
    property FocusCol : integer read FFocusCol write SetFocusCol;
    property FocusRow : integer read FFocusRow write SetFocusRow;

    property RowHeight : TpgfCoord read FRowHeight;

    property Font : TpgfFont read FFont;
    property HeaderFont : TpgfFont read FHeaderFont;

    property ColumnWidth[aCol : integer] : TpgfCoord read GetColumnWidth write SetColumnWidth;

    property ColumnCount : integer read GetColumnCount;
    property RowCount : integer read GetRowCount;

    function VisibleLines : integer;
    function VisibleWidth : integer;

    procedure UpdateScrollBar;

    procedure HandleShow; override;

    procedure VScrollBarMove(Sender: TObject; position : integer);

    procedure HScrollBarMove(Sender : TObject; position : integer);

    procedure CheckFocusChange;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

    procedure Update;

//*    procedure HandleWindowScroll(direction, amount : integer); override;
    procedure HandleResize(dwidth, dheight : integer); override;
  public

    OnFocusChange : TFocusChangeNotify;
    OnRowChange   : TRowChangeNotify;

    OnDoubleClick : TMouseNotifyEvent;

  published

    property FontName : string read GetFontName write SetFontName;
    property HeaderFontName : string read GetHeaderFontName write SetHeaderFontName;
    
  end;

{TwgCustomGrid}

  
type
  TGridColumn = class
  public
    Width : integer;
    Title : WideString;
    Alignment : TAlignment;
    
    constructor Create;
  end;

  TwgCustomGrid = class(TwgGridImpl)
  private
    function GetColumns(index : integer): TGridColumn;
  protected
    FRowCount : integer;
    FColumns : TList;
    
    function GetColumnCount : integer; override;
    procedure SetColumnCount(value : integer);

    function GetRowCount : integer; override;
    procedure SetRowCount(value : integer);

    function GetColumnWidth(col : integer) : TpgfCoord; override;
    procedure SetColumnWidth(col : integer; cwidth : TpgfCoord); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property RowCount : integer read GetRowCount write SetRowCount;

    property ColumnCount : integer read GetColumnCount write SetColumnCount;
    
    procedure DrawHeader(col : integer; rect : TpgfRect; flags : integer); override;
    
    function AddColumn(ATitle : WideString; Awidth : integer) : TGridColumn;
    function AddColumn8(ATitle : string; Awidth : integer) : TGridColumn;

    property Columns[index : integer] : TGridColumn read GetColumns;

  end;

type
    TStringColumn = class
	Cells : TStringList;
	width : TpgfCoord;
	Title : WideString;
	constructor Create;
	destructor Destroy; override;
    end;
        
    TwgStringGrid = class(TwgGridImpl)
	private
	    FColumns : TList;
	    FDefaultColumnWidth : TpgfCoord;
	    FRowCount : integer;
	    FColumnCount : integer;
	    function GetCell(aColumn, aRow : Longword) : WideString;
	    procedure SetCell(aColumn, aRow : Longword; aValue : WideString);
	protected
	    function GetColumnWidth(aCol : Integer) : integer; override;	    
	    procedure SetColumnWidth(aCol : Integer; aWidth : Integer); override;
     
	    function GetColumnCount : integer; override;

	    function GetRowCount : integer; override;	    

	    procedure DrawCell(aRow, aCol : Integer; aRect : TpgfRect; aFlags : integer); override;
	    procedure DrawHeader(aCol : integer; aRect : TpgfRect; aFlags : integer); override;
	    
	    procedure SetColumnTitle(aColumn : integer; aValue : WideString);
	    function GetColumnTitle(aColumn : integer) : WideString;
	    
	    procedure SetCell8(aColumn, aRow : longword; aValue : string);
	    function GetCell8(aColumn, aRow : Longword) : string;
	public	    	    
	    constructor Create(aOwner : TComponent); override;
	    destructor Destroy; override;
	    property ColumnTitle[aColumn : integer] : WideString read GetColumnTitle write SetColumnTitle;
	    property ColumnWidth[aColumn : integer] : integer read GetColumnWidth write SetColumnWidth;
	    property Cells[aColumn, aRow : Longword] : WideString read GetCell write SetCell;
	    property Cells8[aColumn, aRow : Longword] : string read GetCell8 write SetCell8;
	    property DefaultColumnWidth : TpgfCoord read FDefaultColumnWidth write FDefaultColumnWidth;

	    procedure SetRowCount(aValue : integer);
            property RowCount : integer read GetRowCount write SetRowCount;

	    procedure SetColumnCount(aValue : integer);
            property ColumnCount : integer read GetColumnCount write SetColumnCount;
    end;


implementation

//* uses gfxstyle; //, xlib, x, xutil;

{ TwgGridImpl }

procedure TwgGridImpl.SetDrawGrid(AValue : Boolean);
begin
  if AValue <> FDrawGrid then
  begin
    FDrawGrid := AValue;
    RePaint;
  end;
end;

function TwgGridImpl.GetColumnCount: integer;
begin
  result := 7;
end;

function TwgGridImpl.GetRowCount: integer;
begin
  result := 24;
end;

function TwgGridImpl.GetColumnWidth(col: integer): TpgfCoord;
begin
  if col = 2 then result := FTemp else result := 60+(col*16);
end;

procedure TwgGridImpl.SetColumnWidth(col : integer; cwidth: TpgfCoord);
begin
  if (col = 2) and (cwidth <> FTemp) then
  begin
    FTemp := cwidth;
    UpdateScrollBar;
    Repaint;
  end;
end;

procedure TwgGridImpl.FollowFocus;
var
  n : integer;
  w : TpgfCoord;
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

function TwgGridImpl.VisibleLines: integer;
var
  hh : integer;
begin
  if FHScrollBar.Visible then hh := FHScrollbar.Height else hh := 0;
  if HeadersOn then hh := hh + FHeaderHeight+1;
  result := (self.Height - 2*FMargin - hh) div (FRowHeight+1)
end;

function TwgGridImpl.VisibleWidth: integer;
var
  sw : integer;
begin
  if FVScrollBar.Visible then sw := FVScrollBar.width-1 else sw := 0;
  result := self.Width - FMargin*2 - sw;
end;

procedure TwgGridImpl.UpdateScrollBar;
var
  i : integer;
  vw : integer;
  cw : integer;
begin
  // insert to support horizontal scrollbar - aegluke
  vw := VisibleWidth;
  cw := 0;
  for i := 1 to ColumnCount do cw := cw + ColumnWidth[i];
  FHScrollBar.Visible := cw > vw;
  if FHScrollbar.Visible then
  begin
    FHScrollBar.Min := 1;
    FHScrollBar.SliderSize := 0.2;
    FHScrollBar.Max := ColumnCount;
    FHScrollBar.Position := FFocusCol;
  end;

  FVScrollBar.Visible := (RowCount > VisibleLines);

  if FVScrollBar.Visible then
  begin
    FVScrollBar.Min := 1;
    if RowCount > 0
      then FVScrollBar.SliderSize := VisibleLines / RowCount
      else FVScrollBar.SliderSize := 0;
      
    FVScrollBar.Max := RowCount-VisibleLines+1;
    FVScrollBar.Position := FFirstRow;

//*    if FVScrollBar.WinHandle > 0 then 
    FVScrollBar.RePaintSlider;
  end;
  if FHScrollBar.Visible then
  begin
    if FVScrollBar.Visible
      then FHScrollBar.SetPosition(1,height - 18,width - FVScrollbar.width - 1, 18)
      else FHScrollBar.SetPosition(1,height - 18,width - 1, 18);
       
//*    if FHScrollBar.WinHandle > 0 then 
    FHScrollBar.RepaintSlider;
  end;
end;

procedure TwgGridImpl.HandleShow;
begin
  FVScrollBar.SetPosition(width-18,0,18,height);
  if FVScrollBar.Visible then
     FHScrollBar.SetPosition(1,height - 18,width - FVScrollbar.width - 1, 18)
  else
      FHScrollBar.SetPosition(1,height - 18,width - 1, 18);
  inherited HandleShow;
  UpdateScrollBar;
  CheckFocusChange;
end;

procedure TwgGridImpl.HScrollBarMove(Sender : TObject; position : integer);
begin
  if FFirstCol <> position then
  begin
    if Position < 1 then Position := 1;
    FFirstCol := position;
    RePaint;
  end;
end;

procedure TwgGridImpl.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstRow <> position then
  begin
    FFirstRow := position;
    repaint;
  end;
end;

constructor TwgGridImpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DrawGrid := true;
  Focusable := true;
  FWidth := 120;
  FHeight := 80;
  FFocusCol := 1;
  FPrevCol := 0; //FFocusCol;
  FFocusRow := 1;
  FPrevRow := 0; //FFocusRow;
  FFirstRow := 1;
  FFirstCol := 1;
  FMargin := 2;
  FFont := pgfGetFont('#Grid');
  FHeaderFont := pgfGetFont('#GridHeader');
  HeadersOn := true;
  RowSelect := false;

  FRowHeight := FFont.Height + 2;
  FHeaderHeight := FHeaderFont.Height + 2;

  FBackgroundColor := clBoxColor;

  FColResizing := false;

  FVScrollBar := TwgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := {$ifndef Delphi}@{$endif}VScrollBarMove;

  FHScrollBar := TwgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHSCrollBar.OnScroll := {$ifndef Delphi}@{$endif}HScrollBarMove;
  
  FTemp := 50;

  OnFocusChange := nil;
  OnRowChange := nil;
  OnDoubleClick := nil;
end;

destructor TwgGridImpl.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

procedure TwgGridImpl.HandlePaint;
var
  row,col : integer;
  r,r2 : TpgfRect;
  clr : TpgfRect;
begin
  if WinHandle <= 0 then Exit;
//  inherited RePaint;

//*  Canvas.DrawOnBuffer := true;
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  Canvas.DrawControlFrame(0,0,width,height);
  r.SetRect(2, 2, width - 4, height - 4);
  canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectAngle(2,2,width-4,height-4);

  clr.SetRect(FMargin,FMargin, VisibleWidth, height-2*FMargin);
  r := clr;

  if (ColumnCount > 0) and HeadersOn then
  begin
    // Drawing headers
    r.Height := FHeaderHeight;

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
        if DrawGrid then Canvas.SetColor(clGridLines)
                  	else Canvas.SetColor(FBackgroundColor);
                   
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
          canvas.SetColor(FBackgroundColor);
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
  canvas.SetColor(FBackgroundColor);

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

//*  Canvas.SwapBuffer;
  Canvas.EndDraw;
end;

procedure TwgGridImpl.Update;
begin
  UpdateScrollBar;
  FollowFocus;
  RePaint;
end;

procedure TwgGridImpl.DrawCell(row, col: integer; rect: TpgfRect; flags: integer);
var
  s : WideString;
begin
  s := u8('Cellg('+IntToStr(row)+','+IntToStr(col)+')');
  canvas.DrawString(rect.left+1, rect.top+1, s);
  //Canvas.DrawRectangle(rect.left+1,rect.top+1,rect.width-1,rect.height-1);
end;

procedure TwgGridImpl.DrawHeader(col: integer; rect: TpgfRect; flags: integer);
var
  s : WideString;
begin
  s := u8('Head '+IntToStr(col));
  canvas.DrawString(rect.left + (rect.width div 2) - (FHeaderFont.TextWidth(s) div 2),
                     rect.top+1, s);
//  Canvas.DrawRectangle(rect.left+1,rect.top+1,rect.width-1,rect.height-1);
end;

procedure TwgGridImpl.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
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

{ //* pbm is it need under pasGF ?????
procedure TwgGridImpl.HandleWindowScroll(direction, amount: integer);
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
}

procedure TwgGridImpl.HandleLMouseDown(x, y: integer; shiftstate: word);
var
  hh : integer;
  n : integer;

  cw,nw : integer;
  prow, pcol : integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if (ColumnCount < 0) or (RowCount < 1) then Exit;

  pcol := FFocusCol;
  prow := FFocusRow;

  // searching the appropriate character position

  if HeadersOn then hh := FHeaderHeight+1 else hh := 0;

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
        nw := ColumnWidth[FResizedCol] - (cw+FMargin-x);
        if nw > 0 then SetColumnWidth(FResizedCol, nw );

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
//*    MouseCursor := CUR_DIR_EW;
  end;

  CheckFocusChange;
end;

procedure TwgGridImpl.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  inherited HandleLMouseUp(x, y, shiftstate);

//design functionality:
  if FColResizing then Writeln('Column ',FResizedCol,' width = ',ColumnWidth[FResizedCol]);

  FColResizing := False;
//*  MouseCursor := CUR_DEFAULT;
end;

procedure TwgGridImpl.HandleMouseMove(x, y: integer; btnstate, shiftstate : word);
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
      cw := ColumnWidth[FResizedCol]+x-FDragPos;
      if cw < 1 then cw := 1;
      SetColumnWidth(FResizedCol, cw );
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

//*    if cresize then MouseCursor := CUR_DIR_EW else MouseCursor := CUR_DEFAULT;

  end;

end;

procedure TwgGridImpl.HandleDoubleClick(x, y: integer; btnstate, shiftstate: word);
begin
  inherited HandleDoubleClick(x, y, btnstate, shiftstate);

  if Assigned(OnDoubleClick) then OnDoubleClick(self, x,y, btnstate, shiftstate);
end;

procedure TwgGridImpl.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);

  FVScrollBar.Height := Height;
  FVScrollBar.left := Width - FVScrollBar.width;
  FVScrollBar.UpdateWindowPosition;

  UpdateScrollBar;
end;


procedure TwgGridImpl.CheckFocusChange;
begin

  if ((FPrevCol <> FFocusCol) and not RowSelect) or (FPrevRow <> FFocusRow) then
    if Assigned(OnFocusChange) then OnFocusChange(self,FFocusRow,FFocusCol);

  if (FPrevRow <> FFocusRow) then
    if Assigned(OnRowChange) then OnRowChange(self,FFocusRow);

  FPrevCol := FFocusCol;
  FPrevRow := FFocusRow;
end;

procedure TwgGridImpl.SetFocusCol(const Value: integer);
begin
  FFocusCol := Value;
  if FFocusCol < 1 then FFocusCol := 1;
  if FFocusCol > ColumnCount then FFocusCol := ColumnCount;
  FollowFocus;
  CheckFocusChange;
end;

function TwgGridImpl.GetFontName: string;
begin
  result := FFont.FontDesc;
end;

function TwgGridImpl.GetHeaderFontName: string;
begin
  result := FHeaderFont.FontDesc;
end;

procedure TwgGridImpl.SetFocusRow(const Value: integer);
begin
  FFocusRow := Value;
  if FFocusRow < 1 then FFocusRow := 1;
  if FFocusRow > RowCount then FFocusRow := RowCount;
  FollowFocus;
  CheckFocusChange;
  if FWinHandle > 0 then Update;
end;

procedure TwgGridImpl.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
//*  if Windowed then 
  RePaint;
end;

procedure TwgGridImpl.SetHeaderFontName(const AValue: string);
begin
  FHeaderFont.Free;
  FHeaderFont := pgfGetFont(AValue);
//*  if Windowed then 
  RePaint;
end;


{ TwgCustomGrid }

function TwgCustomGrid.GetColumns(index : integer): TGridColumn;
begin
  if (Index < 0) or (Index > FColumns.Count - 1)
    then result := nil
    else Result := TGridColumn(FColumns[index]);
end;

function TwgCustomGrid.GetColumnWidth(col: integer): TpgfCoord;
begin
  if (col > 0) and (col <= ColumnCount) then
    Result := TGridColumn(FColumns[col-1]).Width
  else
    result := 10;
end;

procedure TwgCustomGrid.SetColumnWidth(col: integer; cwidth: TpgfCoord);
begin
  with TGridColumn(FColumns[col-1]) do
  begin
    if width <> cwidth then
    begin
      width := cwidth;
      if width < 1 then width := 1;
      UpdateScrollBar;
      self.repaint;
    end;
  end;
end;

constructor TwgCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TList.Create;
  FRowCount := 5;
end;

destructor TwgCustomGrid.Destroy;
begin
  SetColumnCount(0);
  FColumns.Free;
  inherited Destroy;
end;

function TwgCustomGrid.GetRowCount: integer;
begin
  Result := FRowCount;
end;

procedure TwgCustomGrid.SetRowCount(value: integer);
begin
  FRowCount := value;
  if FFocusRow > FRowCount then
  begin
    FFocusRow := FRowCount;
    FollowFocus;
  end;
  if FWinHandle > 0 then Update;
end;

function TwgCustomGrid.GetColumnCount: integer;
begin
  Result := FColumns.Count;
end;

procedure TwgCustomGrid.SetColumnCount(value: integer);
var
  n : integer;
begin
  n := FColumns.Count;
  if (n = value) or (value < 0) then Exit;
  
  if n < value then
  begin
    // adding columns
    while n < value do
    begin
      AddColumn('',50);
      inc(n);
    end;
  end
  else
  begin
    while n > value do
    begin
      TGridColumn(FColumns.Items[n-1]).Free;
      dec(n);
      FColumns.Count := n;
    end;
  end;
  if FWinHandle > 0 then Update;
end;

procedure TwgCustomGrid.DrawHeader(col: integer; rect: TpgfRect; flags: integer);
var
  s : WideString;
  x : integer;
begin
  s := TGridColumn(FColumns[col-1]).Title;
  x := (rect.width div 2) - (FHeaderFont.TextWidth(s) div 2);
  if x < 1 then x := 1;
  canvas.DrawString(rect.left + x, rect.top+1, s);
end;

function TwgCustomGrid.AddColumn(ATitle: WideString; Awidth: integer): TGridColumn;
begin
  result := TGridColumn.Create;
  result.Title := ATitle;
  result.Width := awidth;
  FColumns.Add(result);
  if FWinHandle > 0 then Update;
end;

function TwgCustomGrid.AddColumn8(ATitle: string; Awidth: integer): TGridColumn;
begin
  result := AddColumn(u8(ATitle),awidth);
end;

{ TGridColumn }

constructor TGridColumn.Create;
begin
  Width := 30;
  Title := '';
  Alignment := TaLeft; //* pbm or should it be Align := alLeft
end;

{TwgStringGrid}

destructor TwgStringGrid.Destroy;
begin
    ColumnCount := 0;
    RowCount := 0;
    inherited Destroy;
end;

function TwgStringGrid.GetCell8(aColumn, aRow : Longword) : string;
begin
    result := wstoutf8(Cells[aColumn, aRow]);
end;

procedure TwgStringGrid.SetCell8(aColumn, aRow : Longword; aValue : string);
begin
    Cells[aColumn, aRow] := u8(aValue);
end;

procedure TwgStringGrid.SetColumnTitle(aColumn : integer; aValue : WideString);
var
    aCalc : integer;
begin
    aCalc := aColumn - FColumns.Count + 1;
    if aCalc > 0 then
	Cells[aColumn,0] := '';
    if aValue <> TStringColumn(FColumns[aColumn]).Title then
    begin
	if aColumn+1 > FColumnCount then FColumnCount := aColumn + 1;
	TStringColumn(FColumns[aColumn]).Title := aValue;
	RePaint;
    end;
end;

function TwgStringGrid.GetColumnTitle(aColumn : integer) : WideString;
begin
    if FColumns.Count - 1 < aColumn then result := ''
    else result := TStringColumn(FColumns[aColumn]).Title;
end;

procedure TwgStringGrid.SetRowCount(aValue : integer);
var
    i, i1 : integer;
//    TmpCol : TStringColumn;
    aCalc : integer;
    SL : TStringList;
begin
    if aValue <> FRowCount then
    begin
	if aValue < FRowCount then
	begin
	    // loeschen und freigeben der zeilen
	    for i := 0 to FColumns.Count - 1 do
	    begin
		
		aCalc := TStringColumn(FColumns[i]).Cells.Count - aValue;
		if aCalc > 0 then
		begin
		    sl := TStringColumn(FColumns[i]).Cells;
		    for i1 := 1 to aCalc do
			sl.Delete(sl.Count-1);
		end;
	    end;
	end;
	FRowCount := aValue;
	RePaint;
    end;
end;

procedure TwgStringGrid.SetColumnCount(aValue : integer);
var
    i : integer;
    aCalc : integer;
//    TmpCol : TStringColumn;
begin
    if aValue <> FColumnCount then
    begin
	// freigeben der spalten
	if aValue < FColumnCount then
	begin
	    aCalc := FColumns.Count - aValue;
	    if aCalc > 0 then
	    begin
		for i := 1 to aCalc do
		begin
		    TStringColumn(FColumns[i]).Destroy;
		    FColumns.Delete(FColumns.Count-1);
		end;
	    end;
	end;
	FColumnCount := aValue;
	RePaint;
    end;
end;

procedure TwgStringGrid.DrawHeader(aCol : integer; aRect : TpgfRect; aFlags : integer);
var
    aCell : WideString;
begin
    {$IFDEF DEBUG}
    writeln('DrawHeader');
    {$ENDIF}
    aCell := ColumnTitle[aCol-1];
    Canvas.DrawString(aRect.Left + aRect.Width div 2 - HeaderFont.TextWidth(aCell) div 2, aRect.top + 1, aCell);
end;

procedure TwgStringGrid.SetColumnWidth(aCol : Integer; aWidth : Integer);
var
    aCalc : integer;
    i : integer;    
    TmpCol : TStringColumn;
begin
    {$IFDEF DEBUG}
    writeln('SetColumnWidth');
    {$ENDIF}
    aCalc := aCol - FColumns.Count;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	begin
	    TmpCol := TStringColumn.Create;
	    TmpCol.Width := DefaultColumnWidth;
	    TmpCol.Cells := TStringList.Create;
	    FColumns.Add(TmpCol);
	end;
    end;
    if TStringColumn(FColumns[aCol]).Width <> aWidth then
    begin
	TStringColumn(FColumns[aCol]).Width := aWidth;
	RePaint;
    end;
end;

function TwgStringGrid.GetColumnWidth(aCol : Integer) : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetColumnWidth');
    {$ENDIF}
    if aCol > FColumns.Count - 1 then result := DefaultColumnWidth
    else result := TStringColumn(FColumns[aCol]).Width;
end;

function TwgStringGrid.GetColumnCount : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetColumnCount');
    {$ENDIF}
    result := FColumnCount;
end;

function TwgStringGrid.GetRowCount : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetRowCount: ',FRowCount);
    {$ENDIF}
    result := FRowCount;
end;

procedure TwgStringGrid.DrawCell(aRow, aCol : Integer; aRect : TpgfRect; aFlags : integer);
begin
    {$IFDEF DEBUG}
    writeln('DrawCell:',aRow,',',aCol,': ',Cells[aCol-1,aRow-1]);
    {$ENDIF}
    Canvas.DrawString(aRect.Left + 1, aRect.top + 1, Cells[aCol-1, aRow-1]);
end;

function TwgStringGrid.GetCell(aColumn, aRow : Longword) : WideString;
var
    diff : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetCell:',aColumn,',',aRow);
    {$ENDIF}
    if aColumn > FColumns.Count - 1 then
	result := ''
    else
    begin
	diff := (TStringColumn(FColumns[aColumn]).Cells.Count - 1) - integer(aRow);
	if diff < 0 then
	    result := ''
	else
	begin
	    result := TStringColumn(FColumns[aColumn]).Cells[aRow];
	end;
    end;
end;

procedure TwgStringGrid.SetCell(aColumn, aRow : Longword; aValue : WideString);
var
    aCalc : integer;
    TmpCol : TStringColumn;
    i : Longword;
begin
    {$IFDEF DEBUG}
    writeln('SetCell:',aColumn,',',aRow);
    {$ENDIF}    
    aCalc := aColumn - FColumns.Count + 1;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	begin
	    TmpCol := TStringColumn.Create;
	    TmpCol.Width := DefaultColumnWidth;
	    FColumns.Add(TmpCol);
	end;
    end;
    aCalc := aRow - TStringColumn(FColumns[aColumn]).Cells.Count + 1;
    if aCalc > 0 then
    begin
	for i := 1 to aCalc do
	    TStringColumn(FColumns[aColumn]).Cells.Append('');
    end;
    TStringColumn(FColumns[aColumn]).Cells[aRow] := aValue;    
    if aColumn > FColumnCount - 1 then FColumnCount := aColumn + 1;
    if aRow > FRowCount - 1 then FRowCount := aRow + 1;
end;

constructor TwgStringGrid.Create(aOwner : TComponent); 
begin
    {$IFDEF DEBUG}
    writeln('Create');
    {$ENDIF}
    inherited Create(aOwner);
    FColumns := TList.Create;
    DefaultColumnWidth := 100;
    ColumnCount := 10;
    RowCount := 10;
end;

// TStringColumn

constructor TStringColumn.Create;
begin
    Cells := TStringList.Create;
    Width := 100;
end;

destructor TStringColumn.Destroy;
begin
    Cells.Destroy;
end;

end.

