unit wgtree;

{
    feature-requests or bugs? - mail to: erik@grohnwaldt.de
    History
// $Log$
// Revision 1.27  2004/01/15 11:48:12  aegluke
// Windows Buffering Changes
//
// Revision 1.26  2004/01/14 13:06:17  aegluke
// visiblity-change of GetNodeHeight
// Bugfix in RePaint, line for single nodes missed
//
// Revision 1.25  2004/01/14 08:22:52  aegluke
// Add recourse-param to FindSubNode
//
// Revision 1.24  2004/01/13 19:27:44  aegluke
// removed litte mistakes on win32
//
// Revision 1.23  2004/01/13 17:40:41  aegluke
// speed-improvements
//
// Revision 1.22  2004/01/13 10:38:28  aegluke
// Doublebuffer-Support
//
// Revision 1.21  2004/01/10 16:21:03  aegluke
// HandleKeyPress-Bugfix
// TwgTreeNode - Inactive-Selection-Color-Support added
//
// Revision 1.20  2004/01/02 20:50:26  aegluke
// Bugfix
//
// Revision 1.19  2004/01/02 16:12:02  aegluke
// Drawing-Lines Bug fixed
//
// Revision 1.18  2004/01/02 11:07:20  aegluke
// ShowColumns-support fixed, ScrollBar-handling fixed
//
// Revision 1.17  2003/12/30 15:30:44  aegluke
// Message-Cleanup
//
// Revision 1.16  2003/12/30 13:37:40  aegluke
// ImageList-support - ShowColumns broken
//
// Revision 1.15  2003/12/27 18:19:02  aegluke
// gfxImage preparations
//
// Revision 1.14  2003/12/20 15:13:39  aegluke
// BugFixes
//
// Revision 1.13  2003/12/08 07:41:21  aegluke
// Windows-Fixes - GUI not responding
//
// Revision 1.12  2003/11/04 09:19:47  aegluke
// DrawString16 changes
//
// Revision 1.11  2003/10/30 11:21:30  aegluke
// DoExpand and onExpand-Event
//
// Revision 1.10  2003/10/29 18:01:06  aegluke
// jump to selected node, expands up to selected node
//
    29.10.2003	0.9a	Doubleclick-Handling implemented
    28.10.2003	0.9	TwgTreeNode.Clear added - removes all subnodes - recourse
    20.07.2003	0.8a	bugfix for scrollbar-handling
    16.07.2003	0.8	bugfix for: TwgTreeNode.Remove, TwgTreeNode.UnregisterSubNode
    25.06.2003	0.7	fixed error in DoChange, visual enhancements, fixed bug in keyboard selection handling
    20.06.2003	0.6	use of the clUnset-Color, it replaces the ColorSet-properties in the treenodes
    19.06.2003	0.5	nodecolor can set for every node - if not set color setting of the parent is used
                      - feature-request from gunter burchard
    15.06.2003	0.4	functions GetFirst/LastSubNode changed to properties First/LastSubNode
                    fixed FindSubNode-Bug
                    columns for resizing space between each sub-level
                    visual enhancements
    08.05.2003 	0.3	AppendText are now functions. it returns the new node
    06.05.2003	0.2	Bugfix in FindSubNode
    04.05.2003	0.1	Initial Release
}

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ENDIF}

{//$DEFINE DEBUG}

interface

uses gfxwidget, gfxbase, schar16, classes, sysutils, wgscrollbar, gfximagelist;

type
  PWgTreeColumnWidth = ^TwgTreeColumnWidth;
  TwgTreeColumnWidth = record
    next: PwgTreeColumnWidth;
    width: word;
  end;


  TwgTreeNode = class
  private
    FFirstSubNode: TwgTreeNode; // the subnodes - for list implementation
    FLastSubNode: TwgTreeNode;

    FText: string16;
    FParent: TwgTreeNode;
    FNext: TwgTreeNode;
    FPrev: TwgTreeNode;
    FCollapsed: boolean;

    FSelColor: TgfxColor;
    FTextColor: TgfxColor;
    FSelTextColor: TgfxColor;
    FInactSelColor : TgfxColor;
    FInactSelTextColor : TgfxColor;
    FImageIndex : integer;
    
    procedure SetText(aValue: string16);
    procedure SetParent(aValue: TwgTreeNode);
    procedure SetText8(aValue: string);
    procedure SetCollapsed(aValue: boolean);
    procedure DoRePaint;

    function GetText8: string;
    procedure SetInactSelTextColor(aValue: TgfxColor);
    procedure SetInactSelColor(aValue: TgfxColor);
    procedure SetSelTextColor(aValue: TgfxColor);
    procedure SetSelColor(aValue: TgfxColor);
    procedure SetTextColor(aValue: TgfxColor);
  public
    constructor Create;
    destructor Destroy; override;

    procedure UnregisterSubNode(aNode: TwgTreeNode);

    procedure Append(aValue: TwgTreeNode);
    function FindSubNode(aValue: string16; ARecourse : Boolean): TwgTreeNode;
    function AppendText(aValue: string16): TwgTreeNode;
    function AppendText8(aValue: string): TwgTreeNode;
    function GetMaxDepth: integer;
    function GetMaxVisibleDepth: integer;

    procedure Collapse;
    procedure Expand;
    function Count: integer;
    function CountRecurse: integer;
    procedure Remove(aNode: TwgTreeNode);

    // Color-Settings
    function ParentTextColor: TgfxColor;
    function ParentSelTextColor: TgfxColor;
    function ParentSelColor: TgfxColor;
    function ParentInactSelTextColor : TgfxColor;
    function ParentInactSelColor : TgfxColor;

    procedure Clear;
    // removes all subnodes recourse

    property Collapsed: boolean read FCollapsed write SetCollapsed;
    property Next: TwgTreeNode read FNext write FNext;
    property Prev: TwgTreeNode read FPrev write FPrev;
    property Text: string16 read FText write SetText;
    property Text8: string read GetText8 write SetText8;
    property Parent: TwgTreeNode read FParent write SetParent;
    property FirstSubNode: TwgTreeNode read FFirstSubNode;
    property LastSubNode: TwgTreeNode read FLastSubNode;
    property ImageIndex : integer read FImageIndex write FImageIndex;

    // color-settings
    property TextColor: TgfxColor read FTextColor write SetTextColor;
    property SelColor: TgfxColor read FSelColor write SetSelColor;
    property SelTextColor: TgfxColor read FSelTextColor write SetSelTextColor;
    property InactSelColor : TgfxColor read FInactSelColor write SetInActSelColor;
    property InactSelTextColor : TgfxColor read FInactSelTextColor write SetInactSelTextColor;
  end;

  TTreeExpandEvent = procedure(aSender: TObject; Node: TwgTreeNode) of object;

  TwgTree = class(TWidget)
  private
    FRootNode: TwgTreeNode;
    FSelection: TwgTreeNode; // currently selected node
    FDefaultColumnWidth: word;
    FFirstColumn: PwgTreeColumnWidth; // the list for column widths
    FFont: TgfxFont;
    FShowColumns: boolean;
    FHScrollbar: TwgScrollbar;
    FVScrollbar: TwgScrollbar;
    FImageList : TgfxImageList;
    FShowImages : boolean;
    
    FXOffset: integer; // for repaint and scrollbar-calculation
    FYOffset: integer;

    FColumnHeight: integer; // height of the column header
    FMoving: boolean;
    FMovingPos: integer;
    FMovingCol: integer;

    procedure FHScrollbarMove(aSender: TObject; Position: integer);
    procedure FVScrollbarMove(aSender: TObject; Position: integer);

    function GetRootNode: TwgTreeNode;
    function VisibleWidth: integer;
    function VisibleHeight: integer;
    procedure SetSelection(aValue: TwgTreeNode);
    procedure SetDefaultColumnWidth(aValue: word);
    procedure SetShowColumns(aValue: boolean);
    function GetNodeHeightSum: integer;
    function MaxNodeWidth: integer;
    procedure UpdateScrollbars;
    function NodeIsVisible(node: TwgTreeNode): boolean;
    function GetAbsoluteNodeTop(aNode: TwgTreeNode): integer; // returns the node-top in pixels
    procedure ReSetScrollbar;
    function GetNodeWidth(ANode : TwgTreeNode) : Integer;     // width of a node inclusive image
  protected
    FColumnLeft : TList;
    function StepToRoot(aNode: TwgTreeNode): integer;
    function NextVisualNode(aNode: TwgTreeNode): TwgTreeNode;
    function PrevVisualNode(aNode: TwgTreeNode): TwgTreeNode;
    function SpaceToVisibleNext(aNode: TwgTreeNode): integer; // the nodes between the given node and the direct next node
    procedure HandleMouseUp(x, y: integer; button: word; shiftstate: word); override;
    procedure HandleMouseDown(x, y: integer; button: word; shiftstate: word); override;
    procedure HandleDoubleClick(x, y: integer; button: word; shiftstate: word); override;
    procedure DoChange; virtual;
    procedure DoExpand(aNode: TwgTreeNode); virtual;
    procedure PreCalcColumnLeft;
    function GetColumnLeft(AIndex : integer) : integer;
    procedure SetShowImages(AValue : Boolean);
  public
    OnChange: TNotifyEvent;
    OnExpand: TTreeExpandEvent;
    constructor Create(aOwner: TComponent); override;
    procedure SetColumnWidth(aindex, awidth: word);
    function GetNodeHeight : integer;    
    function GetColumnWidth(aIndex: word): word; // the width of a column - aIndex of the rootnode = 0
    procedure RePaint; override;
    procedure DoShow; override;
    procedure HandleResize(dWidth, dHeight: integer); override;
    procedure HandleKeyPress(var KeyCode: word; var ShiftState: word; var consumed: boolean); override;
    property ShowImages : boolean read FShowImages write SetShowImages;
    property ShowColumns: boolean read FShowColumns write SetShowColumns;
    property RootNode: TwgTreeNode read GetRootNode;
    property Selection: TwgTreeNode read FSelection write SetSelection;
    property DefaultColumnWidth: word read FDefaultColumnWidth write SetDefaultColumnWidth;
    property Font: TgfxFont read FFont;
    property ImageList : TgfxImageList read FImageList write FImageList;
  end;

implementation

uses gfxstyle;

{ TwgTree }

type
  PColumnLeft = ^integer;

procedure TwgTree.SetShowImages(AValue : Boolean);
begin
     {$IFDEF DEBUG}
     writeln('TwgTree.SetShowImages');
     {$ENDIF}
     if AValue <> FShowImages then
     begin
          FShowImages := AValue;
          UpdateScrollbars;
          RePaint;
     end;
end;

function TwgTree.GetNodeWidth(ANode : TwgTreeNode) : integer;
var
   AImage : TgfxImageItem;
begin
     {$IFDEF DEBUG}
     writeln('TwgTree.GetNodeWidth');
     {$ENDIF}
     if ANode = nil then
        result := 0
     else
     begin
          result := FFont.TextWidth16(ANode.Text) + 2;
          if ShowImages and (ImageList <> nil) then
          begin
               if ANode.ImageIndex > -1 then
               begin
                    AImage := ImageList.Item[ANode.ImageIndex];
                    if AImage <> nil then result := result + AImage.Image.Width + 2;
               end;
          end;
     end;
end;

function TwgTree.GetNodeHeight : integer;
begin
    {$IFDEF DEBUG}
    writeln('TwgTree.GetNodeHeight');
    {$ENDIF}
    result := FFont.Height+5;
end;

procedure TwgTree.DoExpand(aNode: TwgTreeNode);
begin
    {$IFDEF DEBUG}
    writeln('TwgTree.DoExpand');
    {$ENDIF}
    if Assigned(onExpand) then
	onExpand(self, aNode);
end;

procedure TwgTree.SetSelection(aValue: TwgTreeNode);
begin
{$IFDEF DEBUG}
  writeln('TwgTree.SetSelection');
{$ENDIF}
  if aValue <> FSelection then
  begin
    FSelection := aValue;
    if aValue <> nil then
    begin
      aValue := aValue.parent;
      while aValue <> nil do // expandiert rekursiv bis zur rootnode runter
      begin
        aValue.Expand;
        DoExpand(aValue);
        aValue := aValue.parent;
      end;
    end;
    if GetAbsoluteNodeTop(Selection) + GetNodeHeight - FVScrollbar.Position > VisibleHeight then
    begin
      FVScrollbar.Position := GetAbsoluteNodeTop(Selection) + GetNodeHeight - VisibleHeight;
      FYOffset := FVScrollbar.Position;
      UpdateScrollBars;
      if Windowed then FVScrollbar.Canvas.DrawOnBuffer := True;
      FVScrollbar.RePaint;
    end;
    if GetAbsoluteNodeTop(Selection) - FVScrollbar.Position < 0 then
    begin
      FVScrollbar.Position := GetAbsoluteNodeTop(Selection);
      FYOffset := FVScrollbar.Position;
      UpdateScrollbars;
      if Windowed then FVScrollBar.Canvas.DrawOnBuffer := True;
      FVScrollbar.RePaint;
    end;
  end;
end;

function TwgTree.GetAbsoluteNodeTop(aNode: TwgTreeNode): integer;
var
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.GetAbsoluteNodeTop');
{$ENDIF}
  i := 0;
  while (aNode <> nil) and (aNode <> RootNode) do
  begin
    aNode := PrevVisualNode(aNode);
    inc(i);
  end;
  result := (i - 1) * GetNodeHeight;
end;

function TwgTree.NodeIsVisible(node: TwgTreeNode): boolean;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.NodeIsVisible');
  {$ENDIF}
  result := true;
  if node = nil then
  begin
    result := false;
    exit;
  end;
  node := node.parent;
  while node <> nil do
  begin
    if node.collapsed and (node.parent <> nil) then
      result := false;
    node := node.parent;
  end;
end;

procedure TwgTree.DoChange;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.DoChange');
  {$ENDIF}
  if Assigned(OnChange) then
    OnChange(self);
end;

procedure TwgTree.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
var
  XPos: integer;
  i: integer;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.HandleMouseDown');
  {$ENDIF}
  inherited HandleMouseDown(x, y, button, shiftstate);
  if button <> 1 then
    exit; // only left click allowed
  if ShowColumns then
  begin
    x := x + FXOffset;
    xpos := 0;
    i := 0;
    while xpos + 2 < x do
    begin
      inc(i);
      xpos := xpos + GetColumnWidth(i);
    end;
    if (x > xpos - 2) and (x < xpos + 2) then
    begin
      FMoving := true;
      FMovingPos := xpos;
      FMovingCol := i;
      SetColumnWidth(i, GetColumnWidth(i));
    end;
  end;
  RePaint;
end;

procedure TwgTree.HandleDoubleClick(x, y: integer; button: word; shiftstate: word);
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.HandleDoubleClick');
  {$ENDIF}
  HandleMouseUp(x, y, button, shiftstate);
  inherited HandleDoubleClick(x, y, button, shiftstate);
  if Selection.Collapsed then
  begin
    Selection.Expand;
    DoExpand(Selection);
  end
  else
    Selection.Collapse;
  RePaint;
end;

procedure TwgTree.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
var
  col: integer;
  i: integer;
  w: integer;
  i1: integer;
  last: TwgTreeNode;
  node: TwgTreeNode;
  cancel: boolean;
  OldSel: TwgTreeNode;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.HandleMouseUp');
  {$ENDIF}
  inherited HandleMouseUp(x, y, button, shiftstate);
  node := nil;
  if button <> 1 then
    exit;
  OldSel := Selection;
  if FMoving then // column resize
  begin
    FMoving := false;
    x := x + FXOffset;
    SetColumnWidth(FMovingCol, GetColumnWidth(FMovingCol) + x - FMovingPos);
    FMoving := false;
{$IFDEF DEBUG}
    writeln('New Column Size: ', GetColumnWidth(FMovingCol) + x - FMovingPos, ' for Column: ', FMovingCol);
    writeln(GetColumnWidth(FMovingCol));
{$ENDIF}
  end
  else
  begin
    if ShowColumns then
      col := FColumnHeight
    else
      col := 0;
    y := y - col - 1 + FYOffset;
    i := 0;
    x := x + FXOffset;
    cancel := false;
    last := RootNode;
    while not (((i - 1) * GetNodeHeight - 2 <= y) and ((i) * GetNodeHeight + 2 >= y)) do
    begin
      node := NextVisualNode(last);
      if node = nil then
        exit;
      if node = last then
      begin
        Cancel := true;
        break;
      end;
      inc(i);
      last := node;
    end;
    if not cancel then
    begin
      // +/- or node-selection?
      w := 0;
      i1 := StepToRoot(node);
      w := GetColumnLeft(StepToRoot(node));
      if (x >= w - GetColumnWidth(i1) div 2 - 3) and (x <= w - GetColumnWidth(i1) div 2 + 6) then
      // collapse or expand?
      begin // yes
        if node.count > 0 then
        begin
          if node.collapsed then
          begin
            node.expand;
            DoExpand(node);
          end
          else
            node.collapse;
          ReSetScrollBar;
          RePaint;
        end;
      end
      else
      begin
        if x > w - GetColumnWidth(i1) div 2 + 6 then
          Selection := node;
      end;
    end;
  end;
  if OldSel <> Selection then
    DoChange;
end;

procedure TwgTree.HandleKeyPress(var KeyCode: word; var shiftstate: word; var consumed: boolean);
var
  h: TwgTreeNode;
  oldSelection: TwgTreeNode;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.HandleKeyPress');
  {$ENDIF}
  OldSelection := Selection;
  case KeyCode of
    KEY_RIGHT:
      begin
        Consumed := True;
        Selection.Collapsed := false;
        DoExpand(Selection);
        ResetScrollbar;
        RePaint;
      end;
    KEY_LEFT:
      begin
        Consumed := True;
        Selection.Collapsed := true;
        ResetScrollbar;
        RePaint;
      end;
    KEY_UP:
      begin
        if Selection = nil then
          Selection := RootNode.FirstSubNode
        else
          if Selection <> RootNode then
          begin
            if NodeIsVisible(selection) then
            begin
              h := PrevVisualNode(Selection);
              if (h <> RootNode) and (h <> nil) then
                Selection := h;
            end
            else
            begin
              Selection := RootNode.FirstSubNode;
            end;
          end;
          Consumed := True;
      end;
    KEY_DOWN:
      begin
        Consumed := True;
        if Selection = nil then
          Selection := RootNode.FirstSubNode
        else
        begin
          if NodeIsVisible(selection) then
          begin
            h := NextVisualNode(Selection);
            if (h <> nil) then
              Selection := h;
          end
          else
            Selection := RootNode.FirstSubNode;
        end;
      end;
  else
    Consumed := false;
  end;
  if Selection <> OldSelection then
    DoChange;
  if not Consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

function TwgTree.MaxNodeWidth: integer;
var
  h: TwgTreeNode;
  w: integer;
  i1: integer;
  i: integer;
  r: integer;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.MaxNodeWidth');
  {$ENDIF}
  result := 0;
  h := RootNode.FirstSubNode;
  r := 0;
  while h <> nil do
  begin
    w := GetColumnLeft(StepToRoot(h));
    if r < w + GetNodeWidth(h) then
      r := w + GetNodeWidth(h);
    if (not h.collapsed) and (h.count > 0) then
      h := h.FirstSubNode
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := r + 4;
            exit;
          end;
        end;
        h := h.next;
      end;
    end;
  end;
end;

procedure TwgTree.HandleResize(dWidth, dHeight: integer);
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.HandleResize');
  {$ENDIF}
  inherited HandleResize(dwidth, dheight);
  ReSetScrollbar;
  RePaint;
end;

procedure TwgTree.FHScrollbarMove(aSender: TObject; Position: integer);
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.FHScrollbarMove');
  {$ENDIF}
  FXOffset := Position;
  RePaint;
end;

procedure TwgTree.FVScrollbarMove(aSender: TObject; Position: integer);
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.FVScrollbarMove');
  {$ENDIF}
  FYOffset := Position;
  RePaint;
end;

procedure TwgTree.UpdateScrollbars;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.UpdateScrollbars');
  {$ENDIF}
  FVScrollbar.Visible := VisibleHeight < GetNodeHeightSum * GetNodeHeight;
  FVScrollbar.Min := 0;
  FVScrollbar.Max := (GetNodeHeightSum - 1) * GetNodeHeight;
  FHScrollbar.Min := 0;
  FHScrollbar.Max := MaxNodeWidth - VisibleWidth + FVScrollbar.Width;
  FHScrollbar.Visible := MaxNodeWidth > Width - 2;
  if not FVScrollbar.Visible then
  begin
    FVScrollbar.Position := 0;
    FYOffset := 0;
  end;
  if not FHScrollbar.Visible then
  begin
    FHScrollbar.Position := 0;
    FXOffset := 0;
  end;
end;

procedure TwgTree.ReSetScrollbar;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.ReSetScrollbar');
  {$ENDIF}
  UpdateScrollBars;
  if FHScrollbar.Visible then
    FVScrollbar.SetDimensions(Width - 19, 1, 18, Height - 2 - 18)
  else
    FVScrollbar.SetDimensions(Width - 19, 1, 18, Height - 2);
  FHScrollbar.SetDimensions(1, Height - 19, Width - 2, 18);
end;

procedure TwgTree.DoShow;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.DoShow');
  {$ENDIF}
  ReSetScrollbar;
  inherited DoShow;
end;

function TwgTree.SpaceToVisibleNext(aNode: TwgTreeNode): integer;
var
  h: TwgTreeNode;
  i: integer;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.SpaceToVisibleNext');
  {$ENDIF}
  result := 0;
  i := 0;
  if aNode.next = nil then
    exit;
  h := aNode;
  while h <> aNode.next do
  begin
    inc(i);
    if (h.count > 0) and (not h.collapsed) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      while h.next = nil do
        h := h.parent;
      h := h.next;
    end;
  end;
  result := i;
end;

procedure TwgTree.PreCalcColumnLeft;
var
    Aleft : TgfxCoord;
    ACounter : integer;
    AColumnLeft : PColumnLeft;
begin
    {$IFDEF DEBUG}
    writeln('TwgTree.preCalcColumnWidth');
    {$ENDIf}
    if FColumnLeft = nil then
	FColumnLeft := TList.Create
    else
	FColumnLeft.Clear;
    for ACounter := 0 to FColumnLeft.Count - 1 do  // Freeing Memory
    begin
         AColumnLeft := FColumnLeft[ACounter];
         Dispose(AColumnLeft);
    end;
    Aleft := 0;
    for ACounter := 1 to RootNode.getMaxDepth do
    begin
	AColumnLeft := new(PColumnLeft);
	AColumnLeft^ := Aleft;
	FColumnLeft.Add(AColumnLeft);
	Aleft := ALeft + GetColumnWidth(ACounter);
    end;
end;

function TwgTree.GetColumnLeft(AIndex : integer) : integer;
var
   AColumnLeft : PColumnLeft;
begin
     if FColumnLeft = nil then
        PreCalcColumnLeft;
     if AIndex > FColumnLeft.Count - 1 then
     begin
          AColumnLeft := FColumnLeft[FColumnLeft.Count - 1];
          result := AColumnLeft^;
     end
     else
     begin
         AColumnLeft := FColumnLeft[AIndex];
         result := AColumnLeft^;
     end;
end;

procedure TwgTree.RePaint;
var
  r: TgfxRect;
  h: TwgTreeNode;
  i: integer;
  i1: integer;
  w: integer;
  YPos: integer;
  col: integer;
  ACenterPos : integer;
  AImageItem : TgfxImageItem;
  AVisibleHeight : Integer;
label
  label_next;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.RePaint');
  {$ENDIF}
  if FWinHandle <= 0 then exit;
  i1 := 0;
  PreCalcColumnLeft;
  UpdateScrollbars;
  AVisibleHeight := VisibleHeight;
  Canvas.DrawOnBuffer := True;
//  Canvas.ClearClipRect;
  Canvas.Clear(BackgroundColor);
  if FFocused then
    Canvas.SetColor(clWidgetFrame)
  else
    Canvas.SetColor(clInactiveWGFrame);
  Canvas.DrawRectangle(0, 0, Width, Height); // border
  r.SetRect(0,0,VisibleWidth-2, VisibleHeight-2);
  if ShowColumns then // draw the column header?
  begin
    r.SetRect(1, 1, VisibleWidth, FColumnHeight);
    Canvas.SetClipRect(r);
    Canvas.SetColor(clHilite1);
    Canvas.DrawLine(1, 1, VisibleWidth, 1);
    Canvas.DrawLine(1, 1, 1, FColumnHeight - 1);
    Canvas.SetColor(clHilite2);
    Canvas.DrawLine(1, 2, VisibleWidth - 1, 2);
    Canvas.DrawLine(2, 2, FColumnHeight - 1, 2);
    Canvas.SetColor(clShadow2);
    Canvas.DrawLine(1, FColumnHeight, VisibleWidth, FColumnHeight);
    Canvas.DrawLine(VisibleWidth, 1, VisibleWidth, FColumnHeight);
    Canvas.SetColor(clShadow1);
    Canvas.DrawLine(2, FColumnHeight - 1, VisibleWidth - 1, FColumnHeight - 1);
    Canvas.DrawLine(VisibleWidth - 1, 2, VisibleWidth - 1, FColumnHeight - 1);
    Canvas.SetColor(clGridHeader);
    Canvas.FillRectangle(3, 3, VisibleWidth - 4, FColumnHeight - 4);
    Canvas.SetColor(clWidgetFrame);
    
    w := 0;
    r.SetRect(3, 2, VisibleWidth - 4, FColumnHeight - 2);
    Canvas.SetClipRect(r);
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w - FXOffset, 2, w - FXOffset, FColumnHeight - 1);
    end;
    Canvas.SetColor(clShadow1);
    w := 0;
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w - 1 - FXOffset, 3, w - 1 - FXOffset, FColumnHeight - 2);
    end;
    Canvas.SetColor(clHilite2);
    w := 0;
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w + 1 - FXOffset, 3, w + 1 - FXOffset, FColumnHeight - 2);
    end;    
  end;
  if ShowColumns then
  begin
    r.SetRect(1, 1 + FColumnHeight, VisibleWidth, VisibleHeight); // drawing rectangle for nodes and lines
    col := FColumnHeight;
  end
  else
  begin
    r.SetRect(1, 1, VisibleWidth, VisibleHeight);
    col := 0;
  end;
//  Canvas.ClearClipRect;
  Canvas.SetClipRect(r);

  // draw the nodes with lines
  h := RootNode.FirstSubNode;
  Canvas.SetTextColor(RootNode.ParentTextColor);
  YPos := 0;
  while h <> nil do
  begin
    Canvas.SetTextColor(h.ParentTextColor);
    // lines with + or -
    w := GetColumnLeft(StepToRoot(h));
    YPos := YPos + GetNodeHeight;
    ACenterPos := YPos - FYOffset + col - GetNodeHeight + (GetNodeHeight div 2);
    if ACenterPos > FHScrollbar.Position - GetNodeHeight then
    begin
         if h = Selection then // draw the selection rectangle and text
         begin
              if Focused then
              begin
                   Canvas.SetColor(h.ParentSelColor);
                   Canvas.SetTextColor(h.ParentSelTextColor);
              end
              else
              begin
                   Canvas.SetColor(h.ParentInactSelColor);
                   Canvas.SetTextColor(h.ParentInActSelTextColor);
              end;
              Canvas.FillRectangle(w - FXOffset, YPos - FYOffset + col - GetNodeHeight + FFont.Ascent div 2 - 2, GetNodeWidth(h), GetNodeHeight);
              if (ImageList <> nil) and  ShowImages then
              begin
                   AImageItem := ImageList.Item[h.ImageIndex];
                   if AImageItem <> nil then
                   begin
                     Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image,0,0,16,16);
                     Canvas.DrawString16(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
                   end
                   else
                       Canvas.DrawString16(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
              end
              else
                  Canvas.DrawString16(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
              Canvas.SetTextColor(h.ParentTextColor);
         end
         else
         begin
         if (ImageList <> nil) and  ShowImages then
         begin
                 AImageItem := ImageList.Item[h.ImageIndex];
                 if AImageItem <> nil then
                 begin
                      Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image,0,0,16,16);
                      Canvas.DrawString16(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
                 end
                 else
                     Canvas.DrawString16(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
            end
            else
                Canvas.DrawString16(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
         end;
         Canvas.SetColor(clText1);
         if h.Count > 0 then // subnodes?
         begin
              Canvas.DrawRectangle(w - FXOffset - GetColumnWidth(i1) div 2 - 3, ACenterPos - 3, 9, 9);
              if h.Collapsed then // draw a "+"
              begin
                   Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 3, ACenterPos + 1);
                   Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 1, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos + 3);
              end
              else
              begin // draw a "-"
                    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 3, ACenterPos + 1);
              end;
         end
         else
         begin
//              if (h.next <> nil) or (h.prev <> nil) then
              // draw the line in front of a single node
              begin
                   Canvas.SetColor(clText1);
                   Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,  ACenterPos + 1, w - FXOffset - 3,  ACenterPos + 1);
              end;
         end;
         if h.prev <> nil then
         begin
         // line up to the previous node
            if h.prev.count > 0 then
            begin
                 if h.count > 0 then
                    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 6)
                 else
                     Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 6)
            end
            else
            begin
                 if h.count > 0 then
                    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 2)
                 else
                     Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 2)
            end
         end
         else
         begin
              if h.count > 0 then
                 Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3)
              else
                  Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3);
         end;
    end;
    if AVisibleHeight > ACenterPos + GetNodeHeight then
    begin
         if h.count > 0 then
         begin
              if not h.collapsed then h := h.FirstSubNode
              else goto label_next;
         end
         else
         begin
              label_next:
              if h.next <> nil then
                 h := h.next // next node
              else
              begin
                   while h.next = nil do // or recurse next node per parent
                   begin
                        h := h.parent;
                        if (h = nil) or (h = rootnode) then
                        begin
                             Canvas.SwapBuffer;
                             exit;
                        end;
                   end;
                   h := h.next;
              end;
         end;
    end
    else
    begin
         // Draw Lines up to the parent nodes
      ACenterPos := ACenterPos + GetNodeHeight;
      while h <> RootNode do
      begin
         w := GetColumnLeft(StepToRoot(h));
         if h.next <> nil then
         begin
              h := h.next;
              if h.prev.count > 0 then
                  Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, GetAbsoluteNodeTop(h.prev) - FYOffset + 6 + GetNodeHeight div 2)
              else
                  Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, GetAbsoluteNodeTop(h.prev) - FYOffset + 2 + GetNodeHeight div 2);
         end;
         h := h.parent;
      end;
      Canvas.SwapBuffer;
      exit;
    end;
  end;
  Canvas.SwapBuffer;
end;

function TwgTree.PrevVisualNode(aNode: TwgTreeNode): TwgTreeNode;
var
  no: TwgTreeNode;
begin
  {$IFDEF DEBUG}
  writeln('TwgTree.PrevVisualNode');
  {$ENDIF}
  no := aNode;
  if aNode.prev <> nil then
  begin
    result := aNode.Prev;
    aNode := aNode.prev;
    while (not aNode.Collapsed) and (aNode.Count > 0) do
    begin
      result := aNode.LastSubNode;
      aNode := aNode.LastSubNode;
    end;
  end
  else
  begin
    if aNode.Parent <> nil then
      result := aNode.Parent
    else
      result := no;
  end;
end;

function TwgTree.NextVisualNode(aNode: TwgTreeNode): TwgTreeNode;
label
  nextnode;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.NextVisualNode');
{$ENDIF}
  result := nil;
  if aNode.Collapsed then
  begin
    nextnode:
    if aNode.Next <> nil then
    begin
      result := aNode.Next;
      exit;
    end
    else
    begin
      while aNode.next = nil do
      begin
        aNode := aNode.Parent;
        if aNode = nil then
          exit;
      end;
      result := aNode.Next;
      exit;
    end;
  end
  else
  begin
    if aNode.Count > 0 then
    begin
      result := aNode.FirstSubNode;
      exit;
    end
    else
      goto nextnode;
  end;
end;

function TwgTree.StepToRoot(aNode: TwgTreeNode): integer;
var
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.StepToRoot');
{$ENDIF}
  i := -1;
  while aNode <> nil do
  begin
    aNode := aNode.parent;
    inc(i);
  end;
  result := i;
end;

procedure TwgTree.SetShowColumns(aValue: boolean);
begin
{$IFDEF DEBUG}
  writeln('TwgTree.SetShowColumns');
{$ENDIF}
  if FShowColumns <> aValue then
  begin
    FShowColumns := aValue;
    RePaint;
  end;
end;

function TwgTree.VisibleHeight: integer;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.VisibleHeight');
{$ENDIF}
  result := 0;
  if FShowColumns then
  begin
    if MaxNodeWidth > Width - 2 then
      result := Height - 2 - FHScrollbar.Height - FColumnHeight
    else
      result := Height - 2 - FColumnHeight;
  end
  else
    if MaxNodeWidth > width - 2 then
      result := Height - 2 - FHScrollbar.Height
    else
      result := Height - 2;
end;

function TwgTree.VisibleWidth: integer;
begin
  if GetNodeHeightSum * (GetNodeHeight) > Height - 2 then
    VisibleWidth := Width - 2 - FVScrollbar.Width
  else
    result := Width - 2;
end;

function TwgTree.GetNodeHeightSum: integer;
var
  h: TwgTreeNode;
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.GetNodeHeightSum');
{$ENDIF}
  h := RootNode;
  i := -1;
  while h <> nil do
  begin
    inc(i);
    if (not h.Collapsed) and (h.Count > 0) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := i;
            exit;
          end;
        end;
        h := h.next;
      end;
    end;
  end;
  result := i;
end;

constructor TwgTree.Create(aOwner: TComponent);
begin
{$IFDEF DEBUG}
  writeln('TwgTree.Create');
{$ENDIF}
  inherited create(aOwner);
  FRootNode := nil;
  FSelection := nil;
  FDefaultColumnWidth := 15;
  FFirstColumn := nil;
  FFont := guistyle.LabelFont1;
  FHScrollbar := TwgScrollbar.Create(self);
  FHScrollbar.Orientation := orHorizontal;
  FHScrollbar.onScroll := {$IFDEF FPC}@{$ENDIF}FHScrollbarMove;
  FHScrollbar.Visible := false;
  FHScrollbar.Position := 0;
  FHScrollbar.SliderSize := 0.2;
  FVScrollbar := TwgScrollbar.Create(self);
  FVScrollbar.Orientation := orVertical;
  FVScrollbar.onScroll := {$IFDEF FPC}@{$ENDIF}FVScrollbarMove;
  FVScrollbar.Visible := false;
  FVScrollbar.Position := 0;
  FVScrollbar.SliderSize := 0.2;
  FBackgroundColor := clListBox;
  FFocusable := true;
  FMoving := false;
  FXOffset := 0;
  FYOffset := 0;
  FColumnHeight := FFont.Height + 2;
end;

function TwgTree.GetColumnWidth(aIndex: word): word;
var
  h: PwgTreeColumnWidth;
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.GetColumnWidth');
{$ENDIF}
  h := FFirstColumn;
  i := 0;
  if h = nil then // not found
  begin
    result := DefaultColumnWidth;
    exit;
  end;
  while i < aIndex do
  begin
    if h = nil then // not found - returns the default
    begin
      result := DefaultColumnWidth;
      exit;
    end;
    h := h^.next;
    inc(i);
  end;
  if h <> nil then
    result := h^.width
  else // not found -> returns the default
    result := DefaultColumnWidth;
end;

procedure TwgTree.SetColumnWidth(aindex, awidth: word);
var
  h: PwgTreeColumnWidth;
  n: PwgTreeColumnWidth;
  i: word;
begin
{$IFDEF DEBUG}
  writeln('TwgTree.SetColumnWidth');
{$ENDIF}
  h := FFirstColumn;
  if h = nil then
  begin
    new(h);
    h^.width := FDefaultColumnWidth;
    h^.next := nil;
    FFirstColumn := h;
  end;
  i := 0;
  while i < aindex do
  begin
    if h^.next = nil then
    begin
      new(n);
      h^.next := n;
      n^.width := DefaultColumnWidth;
      n^.next := nil;
    end;
    h := h^.next;
    inc(i);
  end;
  if h^.width <> awidth then
  begin
    h^.width := aWidth;
    RePaint;
  end;
end;

procedure TwgTree.SetDefaultColumnWidth(aValue: word);
begin
{$IFDEF DEBUG}
  writeln('TwgTree.SetDefaultColumnWidth');
{$ENDIF}
  if (aValue <> FDefaultColumnWidth) and (aValue > 3) then
  begin
    FDefaultColumnWidth := aValue;
    RePaint;
  end;
end;

function TwgTree.GetRootNode: TwgTreeNode;
begin
{$IFDEF DEBUG}
  writeln('GetRootNode');
{$ENDIF}
  if FRootNode = nil then
    FRootNode := TwgTreeNode.Create;
  FRootNode.TextColor := clText1;
  FRootnode.SelTextColor := clSelectionText;
  FRootnode.SelColor := clSelection;
  result := FRootNode;
end;

{ TwgTreeNode }

procedure TwgTreeNode.Clear;
// removes all sub-nodes - recourse
begin
  while FirstSubNode <> nil do
  begin
    if FirstSubNode.Count > 0 then // recourse
      FirstSubNode.Clear;
    Remove(FirstSubNode);
  end;
end;

function TwgTreeNode.GetMaxVisibleDepth: integer;
var
  h: TwgTreeNode;
  a, t: integer;
begin
{$IFDEF DEBUG}
  writeln('GetMaxVisibleDepth');
{$ENDIF}
  result := 1;
  h := FirstSubNode;
  if h.Collapsed then
    exit;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

function TwgTreeNode.GetMaxDepth: integer;
var
  h: TwgTreeNode;
  a, t: integer;
begin
{$IFDEF DEBUG}
  writeln('GetMaxDepth');
{$ENDIF}
  h := FirstSubNode;
  result := 1;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

function TwgTreeNode.FindSubNode(aValue: string16; ARecourse : Boolean): TwgTreeNode;
var
  h: TwgTreeNode;
begin
  result := nil;
  if ARecourse then
  begin
  h := FirstSubNode;
  while h <> nil do
  begin
    if h.Text = aValue then
    begin
      result := h;
      exit;
    end;
    if h.count > 0 then
    begin
      result := h.FirstSubNode.FindSubNode(aValue, ARecourse);
      if result <> nil then
        exit;
    end;
    h := h.next;
  end;
  end
  else
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if h.Text = AValue then
      begin
        result := h;
        break;
      end;
      h := h.next;
    end;
  end;
end;

function TwgTreeNode.CountRecurse: integer;
var
  h: TwgTreeNode;
  i: integer;
begin
  h := FFirstSubNode;
  i := 0;
  while h <> nil do
  begin
    i := i + h.CountRecurse; // increases i by the count of the subnodes of the subnode
    h := h.next;
    inc(i); // and the subnode...
  end;
  result := i;
end;

function TwgTreeNode.Count: integer;
var
  h: TwgTreeNode;
  i: integer;
begin
  h := FirstSubNode;
  i := 0;
  while h <> nil do
  begin
    h := h.next;
    inc(i);
  end;
  result := i;
end;

function TwgTreeNode.AppendText8(aValue: string): TwgTreeNode;
begin
{$IFDEF DEBUG}
  writeln('AppendText8');
{$ENDIF}
  result := AppendText(Str8To16(aValue));
end;

function TwgTreeNode.AppendText(aValue: string16): TwgTreeNode;
var
  h: TwgTreeNode;
begin
{$IFDEF DEBUG}
  writeln('AppendText');
{$ENDIF}
  h := TwgTreeNode.Create;
  h.Text := aValue;
  self.Append(h);
  result := h;
end;

procedure TwgTreeNode.Collapse;
begin
  Collapsed := true;
end;

procedure TwgTreeNode.Expand;
begin
  Collapsed := false;
end;

procedure TwgTreeNode.SetCollapsed(aValue: boolean);
begin
  if aValue <> FCollapsed then
  begin
    FCollapsed := aValue;
    DoRePaint;
  end;
end;

destructor TwgTreeNode.Destroy;
begin
  if FParent <> nil then
    FParent.UnregisterSubNode(self); // unregisteres self
  inherited Destroy;
end;

constructor TwgTreeNode.Create;
begin
{$IFDEF DEBUG}
  writeln('TwgTreeNode.Create');
{$ENDIF}
  FFirstSubNode := nil;
  FLastSubNode := nil;
  FText := '';
  FImageIndex := -1;
  FParent := nil;
  FNext := nil;
  FPrev := nil;
  FSelColor := clUnset;
  FSelTextColor := clUnset;
  FTextColor := clUnset;
  FInactSelColor := clUnset;
  FInactSelTextColor := clUnset;
end;

procedure TwgTreeNode.DoRePaint;
begin
  { TODO }
  // call repaint from the Twgtree-widget the root-node is registered to
end;

procedure TwgTreeNode.UnRegisterSubNode(aNode: TwgTreeNode);
var
  h: TwgTreeNode;
begin
  h := FFirstSubNode;
  while h <> nil do
  begin
    if h = aNode then
    begin
      if h = FFirstSubNode then
        FFirstSubNode := FFirstSubNode.Next;
      if h = FLastSubNode then
        FLastSubNode := FLastSubNode.Prev;
      if h.prev <> nil then
        h.prev.next := h.next;
      if h.next <> nil then
        h.next.prev := h.prev;
      exit;
    end;
    h := h.next;
  end;
end;

procedure TwgTreeNode.Append(aValue: TwgTreeNode);
begin
{$IFDEF DEBUG}
  writeln('TwgTreeNode.Append');
{$ENDIF}
  aValue.Parent := self;

  aValue.next := nil;

  if FFirstSubNode = nil then
    FFirstSubNode := aValue;

  aValue.prev := FLastSubNode;

  if FLastSubNode <> nil then
    FLastSubNode.Next := aValue;

  FLastSubNode := aValue;
end;

function TwgTreeNode.GetText8: string;
begin
  result := Str16To8(FText);
end;

procedure TwgTreeNode.SetText8(aValue: string);
begin
  if Str8To16(aValue) <> FText then
  begin
    FText := Str8To16(aValue);
    DoRePaint;
  end;
end;

procedure TwgTreeNode.SetParent(aValue: TwgTreeNode);
begin
  if aValue <> FParent then
  begin
    if FParent <> nil then
      FParent.UnRegisterSubNode(self); // unregisteres
    FParent := aValue;
    if FParent <> nil then
    begin
      DoRePaint;
    end;
  end;
end;

procedure TwgTreeNode.SetText(aValue: string16);
begin
  if aValue <> FText then
  begin
    FText := aValue;
    DoRePaint;
  end;
end;

procedure TwgTreeNode.Remove(aNode: TwgTreeNode);
begin
  if FirstSubNode = aNode then
  begin
    FFirstSubNode := aNode.next;
    if FFirstSubNode <> nil then
      FFirstSubNode.Prev := nil;
  end
  else
    if aNode.prev <> nil then
      aNode.Prev.next := aNode.next;
  if LastSubNode = aNode then
  begin
    FLastSubNode := aNode.prev;
    if FLastSubNode <> nil then
      FLastSubNode.next := nil;
  end
  else
    if aNode.next <> nil then
      aNode.next.prev := aNode.prev;
  aNode.prev := nil;
  aNode.next := nil;
  aNode.parent := nil;
end;

procedure TwgTreeNode.SetSelTextColor(aValue: TgfxColor);
begin
  if FTextColor <> aValue then
  begin
    FSelTextColor := aValue;
    DoRePaint;
  end;
end;

procedure TwgTreeNode.SetSelColor(aValue: TgfxColor);
begin
  if FSelColor <> aValue then
  begin
    FSelColor := aValue;
    DoRePaint;
  end;
end;

procedure TwgTreeNode.SetTextColor(aValue: TgfxColor);
begin
  if FTextColor <> aValue then
  begin
    FTextColor := aValue;
    DoRePaint;
  end;
end;

function TwgTreenode.ParentTextColor: TgfxColor;
begin
  if TextColor <> clUnset then
    result := TextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentTextColor
    else
      result := clText1;
  end;
end;

function TwgTreeNode.ParentSelTextColor: TgfxColor;
begin
  if SelTextColor <> clUnset then
    result := SelTextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelTextColor
    else
      result := clSelectionText;
  end;
end;

function TwgTreeNode.ParentInactSelTextColor : TgfxColor;
begin
  if InactSelTextColor <> clUnset then
    result := InactSelTextColor
  else
  begin
    if Parent <> nil then
      Result := Parent.ParentInactSelTextColor
    else
      Result := clInactiveSelText;
  end;
end;

function TwgTreeNode.ParentInactSelColor : TgfxColor;
begin
  if InactSelColor <> clUnset then
    result := InactSelColor
  else
  begin
    if Parent <> nil then
      result := parent.ParentInactSelColor
    else
      result := clInactiveSel;
  end;
end;

function TwgTreeNode.ParentSelColor: TgfxColor;
begin
  if SelColor <> clUnset then
    result := SelColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelColor
    else
      result := clSelection;
  end;
end;

procedure TwgTreeNode.SetInactSelTextColor(aValue: TgfxColor);
begin
  if AValue <> FInactSelTextColor then
  begin
    FInactSelTextColor := AValue;
    DoRePaint;
  end;
end;

procedure TwgTreeNode.SetInactSelColor(aValue: TgfxColor);
begin
  if AValue <> FInactSelColor then
  begin
    FInactSelColor := AValue;
    DoRePaint;
  end;
end;

end.

