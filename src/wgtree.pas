unit wgtree;

{ 
    feature-requests or bugs? - mail to: erik@grohnwaldt.de
    History
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
    {$mode objfpc}
    {$H+}
{$ENDIF}

{//$DEFINE DEBUG}

interface

uses gfxwidget, gfxbase, schar16, classes, sysutils, wgscrollbar;

type    
    PWgTreeColumnWidth = ^TwgTreeColumnWidth;
    TwgTreeColumnWidth = record
	next : PwgTreeColumnWidth;
	width : word;
    end;

    TwgTreeNode = class
	private
	    FFirstSubNode : TwgTreeNode;	   // the subnodes - for list implementation
	    FLastSubNode : TwgTreeNode;
	    
	    FText : string16;
	    FParent : TwgTreeNode;
	    FNext : TwgTreeNode;
	    FPrev : TwgTreeNode;
	    FCollapsed : boolean;
	    
	    FSelColor : TgfxColor;
	    FTextColor : TgfxColor;
	    FSelTextColor : TgfxColor;
	    
	    procedure SetText(aValue : string16);
	    procedure SetParent(aValue : TwgTreeNode);
	    procedure SetText8(aValue : string);
	    procedure SetCollapsed(aValue : boolean);
	    procedure DoRePaint;
	    
	    function GetText8 : string;
	    procedure SetSelTextColor(aValue : TgfxColor);
	    procedure SetSelColor(aValue : TgfxColor);
	    procedure SetTextColor(aValue : TgfxColor);
	public
	    constructor Create;
	    destructor Destroy; override;
	    
	    procedure UnregisterSubNode(aNode : TwgTreeNode);
	    
	    procedure Append(aValue : TwgTreeNode);
	    function FindSubNode(aValue : string16) : TwgTreeNode;
	    function AppendText(aValue : string16) : TwgTreeNode;
	    function AppendText8(aValue : string) : TwgTreeNode;
	    function GetMaxDepth : integer;
	    function GetMaxVisibleDepth : integer;
	    
	    procedure Collapse;
	    procedure Expand;
	    function Count : integer;
	    function CountRecurse : integer;
	    procedure Remove(aNode : TwgTreeNode);
	    
	    function ParentTextColor : TgfxColor;
	    function ParentSelTextColor : TgfxColor;
	    function ParentSelColor : TgfxColor;
	    
	    procedure Clear;
	    // removes all subnodes recourse
	    
	    property Collapsed : boolean read FCollapsed write SetCollapsed;	    
	    property Next : TwgTreeNode read FNext write FNext;
	    property Prev : TwgTreeNode read FPrev write FPrev;
	    property Text : string16 read FText write SetText;
	    property Text8 : string read GetText8 write SetText8;
	    property Parent : TwgTreeNode read FParent write SetParent;
	    property FirstSubNode : TwgTreeNode read FFirstSubNode;
	    property LastSubNode : TwgTreeNode read FLastSubNode;
	
	    // color-settings
	    property TextColor : TgfxColor read FTextColor write SetTextColor;
	    property SelColor : TgfxColor read FSelColor write SetSelColor;
	    property SelTextColor : TgfxColor read FSelTextColor write SetSelTextColor;	    
    end;

    TwgTree = class(TWidget)
	private
	    FRootNode : TwgTreeNode;
	    FSelection : TwgTreeNode;	// currently selected node
	    FDefaultColumnWidth : word;
	    FFirstColumn : PwgTreeColumnWidth;	// the list for column widths
	    FFont : TgfxFont;
	    FShowColumns : boolean;	
	    FHScrollbar : TwgScrollbar;
	    FVScrollbar : TwgScrollbar;
	    
	    FXOffset : integer;	// for repaint and scrollbar-calculation
	    FYOffset : integer;
	    
	    FColumnHeight : integer;	// height of the column header
	    FMoving : boolean; 
	    FMovingPos : integer;
	    FMovingCol : integer;
	    
	    procedure FHScrollbarMove(aSender : TObject; Position : integer);
	    procedure FVScrollbarMove(aSender : TObject; Position : integer);
	    
	    function GetRootNode : TwgTreeNode;
	    function VisibleWidth : integer;
	    function VisibleHeight : integer;
	    procedure SetSelection(aValue : TwgTreeNode);
	    procedure SetDefaultColumnWidth(aValue : word);
	    procedure SetShowColumns(aValue : boolean);
	    function GetNodeHeightSum : integer;
	    function MaxNodeWidth : integer;
	    procedure UpdateScrollbars;
	    function NodeIsVisible(node : TwgTreeNode) : boolean;
	protected
	    function StepToRoot(aNode : TwgTreeNode) : integer; 
	    function NextVisualNode(aNode : TwgTreeNode) : TwgTreeNode;
	    function PrevVisualNode(aNode : TwgTreeNode) : TwgTreeNode;
	    function SpaceToVisibleNext(aNode : TwgTreeNode) : integer; // the nodes between the given node and the direct next node
	    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;	    
	    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
	    procedure HandleDoubleClick(x,y : integer; button : word; shiftstate : word); override;
	    procedure DoChange;	virtual;
	public
	    OnChange : TNotifyEvent;
	    constructor Create(aOwner : TComponent); override;
	    procedure SetColumnWidth(aindex, awidth : word);
	    function GetColumnWidth(aIndex : word) : word; // the width of a column - aIndex of the rootnode = 0
	    procedure RePaint; override;
	    procedure DoShow; override;
	    procedure HandleResize(dWidth, dHeight : integer); override;
	    procedure HandleKeyPress(var KeyCode : word; var ShiftState : word; var consumed : boolean); override;
	    property ShowColumns : boolean read FShowColumns write SetShowColumns;
	    property RootNode : TwgTreeNode read GetRootNode;
	    property Selection : TwgTreeNode read FSelection write SetSelection;
	    property DefaultColumnWidth : word read FDefaultColumnWidth write SetDefaultColumnWidth;
	    property Font : TgfxFont read FFont;	    
    end;

implementation

uses gfxstyle;

{ TwgTree }

function TwgTree.NodeIsVisible(node : TwgTreeNode) : boolean;
begin
    result := true;
    if node = nil then 
    begin
	result := false;
	exit;
    end;
    node := node.parent;
    while node <> nil do
    begin
	if node.collapsed  and (node.parent <> nil) then result := false;
	node := node.parent;
    end;
end;

procedure TwgTree.DoChange;
begin
    if Assigned(OnChange) then OnChange(self);
end;

procedure TwgTree.HandleMouseDown(x,y : integer; button : word; shiftstate : word);
var
    XPos : integer;
    i : integer;
begin
    inherited HandleMouseDown(x,y,button,shiftstate);
    if button <> 1 then exit;	// only left click allowed
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
	    SetColumnWidth(i,GetColumnWidth(i));
	end;
    end;
end;

procedure TwgTree.HandleDoubleClick(x,y : integer; button : word; shiftstate : word);
begin
    HandleMouseUp(x,y,button,shiftstate);
    inherited HandleDoubleClick(x,y,button,shiftstate);
    if Selection.Collapsed then 
	Selection.Expand
    else
	Selection.Collapse;
    RePaint;
end;

procedure TwgTree.HandleMouseUp(x,y : integer; button : word; shiftstate : word);
var
    col : integer;
    i : integer;
    w : integer;
    i1 : integer;
    last : TwgTreeNode;
    node : TwgTreeNode;
    cancel : boolean;
    OldSel : TwgTreeNode;
begin
    inherited HandleMouseUp(x,y,button,shiftstate);
    if button <> 1 then exit;
    OldSel := Selection;
    if FMoving then	// column resize
    begin
	FMoving := false;
	x := x + FXOffset;
	SetColumnWidth(FMovingCol,GetColumnWidth(FMovingCol) + x - FMovingPos);
	FMoving := false;
	{$IFDEF DEBUG}
	writeln('New Column Size: ',GetColumnWidth(FMovingCol) + x - FMovingPos,' for Column: ',FMovingCol);
	writeln(GetColumnWidth(FMovingCol));
	{$ENDIF}
    end
    else
    begin
	if ShowColumns then col := FColumnHeight else col := 0;
	{$IFDEF DEBUG}
	writeln('X=',x,'; Y=',y);
	{$ENDIF}
	y := y - col - 1 + FYOffset;
	i := 0;
	x := x + FXOffset;
	cancel := false;
	last := RootNode;
	while not (((i-1) * FFont.Height-2 <= y) and ((i) * FFont.Height+2 >= y)) do
	begin
	    node := NextVisualNode(last);
	    if node = nil then exit;	    
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
    	    for i := 1 to i1 do w := w + GetColumnWidth(i);	// left position of the node
	    if (x >= w - GetColumnWidth(i1) div 2 - 3) and (x <=  w - GetColumnWidth(i1) div 2 + 6) then	// collapse or expand? 
	    begin 										// yes
		if node.count > 0 then
		begin
		    if node.collapsed then node.expand else node.collapse;
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

procedure TwgTree.HandleKeyPress(var KeyCode : word; var shiftstate : word; var consumed : boolean);
var
    h : TwgTreeNode;    
    oldSelection : TwgTreeNode;
begin
    OldSelection := Selection;
    case KeyCode of
	KEY_RIGHT : begin
	    Selection.Collapsed := false;
	    RePaint;
	end;
	KEY_LEFT : begin
	  Selection.Collapsed := true;
	  RePaint;
	end;
	KEY_UP : begin
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
	    end else begin
		Selection := RootNode.FirstSubNode;
	    end;
	  end;
	end;
	KEY_DOWN : begin
	    if Selection = nil then Selection := RootNode.FirstSubNode
	    else
	    begin
	      if NodeIsVisible(selection) then
	      begin
		h := NextVisualNode(Selection);
		if (h <> nil) then Selection := h;
	      end
	      else
	        Selection := RootNode.FirstSubNode;
	    end;
	end;
	else Consumed := false;
    end;
    if Selection <> OldSelection then DoChange;
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

function TwgTree.MaxNodeWidth : integer;
var
    h : TwgTreeNode;
    w : integer;
    i1 : integer;
    i : integer;
    r : integer;
begin
    h := RootNode.FirstSubNode;    
    r := 0;
    while h <> nil do
    begin
	w := 0;
	i1 := StepToRoot(h);
	for i := 1 to i1 do
	    w := w + GetColumnWidth(i);
	if r < w + FFont.TextWidth16(h.text) then
	    r := w + FFont.TextWidth16(h.text);
	if (not h.collapsed) and (h.count > 0) then
	    h := h.FirstSubNode
	else
	begin
	    if h.next <> nil then h := h.next
	    else
	    begin
		while h.next = nil do
		begin
		    h := h.parent;
		    if h = nil then
		    begin
			result := r;
			exit;
		    end;
		end;
		h := h.next;
	    end;
	end;
    end;
end;

procedure TwgTree.HandleResize(dWidth, dHeight : integer);
begin
    inherited HandleResize(dwidth,dheight);
    DoShow;
    RePaint;
end;

procedure TwgTree.FHScrollbarMove(aSender : TObject; Position : integer);
begin
    FXOffset := Position;
    RePaint;
end;

procedure TwgTree.FVScrollbarMove(aSender : TObject; Position : integer);
begin
    FYOffset := Position;
    RePaint;
end;

procedure TwgTree.UpdateScrollbars;
begin
    FVScrollbar.Visible := VisibleHeight < GetNodeHeightSum * FFont.Height;
    FVScrollbar.Min := 0;
    FVScrollbar.Max := (GetNodeHeightSum-1) * FFont.Height;
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

procedure TwgTree.DoShow;
begin
    inherited DoShow;
    UpdateScrollBars;
    if FHScrollbar.Visible then
	FVScrollbar.SetDimensions(Width - 19, 1, 18, Height - 2 - 18)
    else
	FVScrollbar.SetDimensions(Width - 19,1,18,Height - 2);
    FHScrollbar.SetDimensions(1, Height - 19, Width - 2, 18);
end;

function TwgTree.SpaceToVisibleNext(aNode : TwgTreeNode) : integer;
var
    h : TwgTreeNode;
    i : integer;
begin
    result := 0;
    i := 0;
    if aNode.next = nil then exit;
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

procedure TwgTree.RePaint;
var
    r : TgfxRect;
    h : TwgTreeNode;
    i : integer;
    i1 : integer;
    w : integer;
    YPos : integer;
    col : integer;
label label_next;
begin
    if FWinHandle = 0 then exit;
    UpdateScrollbars;
    inherited RePaint;
    Canvas.ClearClipRect;
    Canvas.Clear(BackgroundColor);
    if FFocused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWGFrame);    
    Canvas.DrawRectangle(0,0,Width, Height); // border
    
    if ShowColumns then // draw the column header?
    begin
	r.SetRect(1,1,VisibleWidth, FColumnHeight);
	Canvas.SetClipRect(r);
	Canvas.SetColor(clHilite1);
	Canvas.DrawLine(1,1,VisibleWidth,1);
	Canvas.DrawLine(1,1,1,FColumnHeight-1);
	Canvas.SetColor(clHilite2);
	Canvas.DrawLine(1,2,VisibleWidth-1,2);
	Canvas.DrawLine(2,2,FColumnHeight-1,2);
	Canvas.SetColor(clShadow2);
	Canvas.DrawLine(1,FColumnHeight,VisibleWidth, FColumnHeight);
	Canvas.DrawLine(VisibleWidth,1,VisibleWidth, FColumnHeight);	
	Canvas.SetColor(clShadow1);
	Canvas.DrawLine(2,FColumnHeight-1, VisibleWidth - 1, FColumnHeight - 1);	
	Canvas.DrawLine(VisibleWidth-1,2, VisibleWidth - 1, FColumnHeight-1);
	Canvas.SetColor(clGridHeader);
	Canvas.FillRectangle(3,3,VisibleWidth-4,FColumnHeight-4);
	Canvas.SetColor(clWidgetFrame);
	w := 0;
	r.SetRect(3,2,VisibleWidth-4, FColumnHeight-2);
	Canvas.SetClipRect(r);	
	for i := 1 to rootnode.getMaxDepth-1 do
	begin
	    w := w + GetColumnWidth(i);
	    Canvas.DrawLine(w-FXOffset,2,w-FXOffset,FColumnHeight - 1);
	end;
	Canvas.SetColor(clShadow1);
	w := 0;
	for i := 1 to rootnode.getMaxDepth-1 do
	begin
	    w := w + GetColumnWidth(i);
	    Canvas.DrawLine(w-1-FXOffset,3,w-1-FXOffset,FColumnHeight - 2);
	end;	
	Canvas.SetColor(clHilite2);
	w := 0;
	for i := 1 to rootnode.getMaxDepth-1 do
	begin
	    w := w + GetColumnWidth(i);
	    Canvas.DrawLine(w+1-FXOffset,3,w+1-FXOffset,FColumnHeight - 2);
	end;
    end;
    if ShowColumns then
    begin
	r.SetRect(1,1+FColumnHeight,VisibleWidth, VisibleHeight);	// drawing rectangle for nodes and lines
	col := FColumnHeight;
    end
    else
    begin
	r.SetRect(1,1,VisibleWidth, VisibleHeight);
	col := 0;
    end;
    Canvas.ClearClipRect;
    Canvas.SetClipRect(r);

    // draw the nodes with lines
    h := RootNode.FirstSubNode;
    Canvas.SetTextColor(RootNode.ParentTextColor);
    YPos := 0;
    while h <> nil do
    begin
      Canvas.SetTextColor(h.ParentTextColor);
      // lines with + or -
      i1 := StepToRoot(h);
      w := 0;
      for i := 1 to i1 do w := w + GetColumnWidth(i);	// left position of the node
	YPos := YPos + FFont.Height;
      if h = Selection then	// draw the selection rectangle and text
      begin
        Canvas.SetColor(h.ParentSelColor);
        Canvas.FillRectangle(w-FXOffset,YPos-FYOffset+col-FFont.Height + FFont.Ascent div 2 - 2,FFont.TextWidth16(h.text)+2,FFont.Height);
	Canvas.SetTextColor(h.ParentSelTextColor);
        Canvas.DrawString16(w-FXOffset+1,YPos-FYOffset+col,h.text);
	Canvas.SetTextColor(h.ParentTextColor);	
      end
      else
        Canvas.DrawString16(w-FXOffset+1,YPos-FYOffset+col,h.text);
      Canvas.SetColor(clText1);
      if h.Count > 0 then	// subnodes? 
      begin
        Canvas.DrawRectangle(w - FXOffset - GetColumnWidth(i1) div 2 - 3, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 4,9,9);
	Canvas.Drawline(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 4);
//	Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) + 2, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 - 3,YPos - FYOffset + col - FFont.Height + FFont.Ascent);
        if h.Collapsed then	// draw a "+"
	begin
	    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 + 3, YPos - FYOffset + col - FFont.Height + FFont.Ascent);	
	    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 2, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent + 2);
	end
	else	
	begin			// draw a "-"
	    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 + 3, YPos - FYOffset + col - FFont.Height + FFont.Ascent);
	end;
      end
      else
      begin
    	if (h.next <> nil) or (h.prev <> nil) then
	begin
	    Canvas.SetColor(clText1);
	    Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2+1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - 3, YPos - FYOffset + col - FFont.Height + FFont.Ascent);
	end;
      end;
      if h.prev <> nil then
      begin
        // line up to the previous node
	if h.prev.count > 0 then
	    if h.count > 0 then
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - SpaceToVisibleNext(h.prev) * FFont.Height - FFont.Height + FFont.Ascent + 4)
	    else
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - SpaceToVisibleNext(h.prev) * FFont.Height - FFont.Height + FFont.Ascent + 4)	    
	else
	    if h.count > 0 then
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - SpaceToVisibleNext(h.prev) * FFont.Height - FFont.Height + FFont.Ascent)
	    else
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - SpaceToVisibleNext(h.prev) * FFont.Height - FFont.Height + FFont.Ascent);	
      end
      else
      begin
        if h.count > 0 then
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1,  YPos - FYOffset + col - FFont.Height + FFont.Descent)
	else
		Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, YPos - FYOffset + col - FFont.Height + FFont.Ascent, w - FXOffset - GetColumnWidth(i1) div 2 + 1,  YPos - FYOffset + col - FFont.Height+FFont.Descent);
      end;
      if h.count > 0 then
      begin
        if not h.collapsed then h := h.FirstSubNode
	else goto label_next;
      end
      else
      begin
        label_next:
        if h.next <> nil then h := h.next	// next node
	else
	begin
	    while h.next = nil do // or recurse next node per parent
	    begin
		h := h.parent;
		if (h = nil) or (h = rootnode)  then exit; 
	    end;
	    h := h.next;
	end;
      end;
    end;
end;

function TwgTree.PrevVisualNode(aNode : TwgTreeNode) : TwgTreeNode;
var
    no : TwgTreeNode;
begin
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
	if aNode.Parent <> nil then result := aNode.Parent
	else result := no;
    end;
end;

function TwgTree.NextVisualNode(aNode : TwgTreeNode) : TwgTreeNode;
label nextnode;
begin
    {$IFDEF DEBUG}
    writeln('NextVisualNode');
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
		if aNode = nil then exit;
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


function TwgTree.StepToRoot(aNode : TwgTreeNode) : integer;
var
    i : integer;
begin
    {$IFDEF DEBUG}
    writeln('StepToRoot');
    {$ENDIF}
    i := -1;
    while aNode <> nil do
    begin
	aNode := aNode.parent;
	inc(i);
    end;
    result := i;
end;

procedure TwgTree.SetShowColumns(aValue : boolean);
begin
    {$IFDEF DEBUG}
    writeln('SetShowColumns');
    {$ENDIF}
    if FShowColumns <> aValue then
    begin
	FShowColumns := aValue;
	RePaint;
    end;
end;

function TwgTree.VisibleHeight : integer;
begin
    {$IFDEF DEBUG}
    writeln('VisibleHeight');
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

function TwgTree.VisibleWidth : integer;
begin
    if GetNodeHeightSum * (FFont.Height) > Height - 2 then VisibleWidth := Width - 2 - FVScrollbar.Width
    else
	result := Width - 2;
end;

function TwgTree.GetNodeHeightSum : integer;
var
    h : TwgTreeNode;
    i : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetNodeHeightSum');
    {$ENDIF}
    h := RootNode;
    i := -1;
    while h <> nil do
    begin
	inc(i);
	if (not h.Collapsed) and (h.Count > 0)  then
	begin
	    h := h.FirstSubNode;
	end
	else
	begin
	    if h.next <> nil then h := h.next
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

constructor TwgTree.Create(aOwner : TComponent);
begin
    {$IFDEF DEBUG}
    writeln('Create');
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
    FVScrollbar := TwgScrollbar.Create(self);
    FVScrollbar.Orientation := orVertical;
    FVScrollbar.onScroll := {$IFDEF FPC}@{$ENDIF}FVScrollbarMove;
    FVScrollbar.Visible := false;
    FBackgroundColor := clListBox;
    FFocusable := true;
    FMoving := false;
    FXOffset := 0;
    FYOffset := 0;
    FColumnHeight := FFont.Height + 2;
end;

function TwgTree.GetColumnWidth(aIndex : word) : word;
var
    h : PwgTreeColumnWidth;
    i : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetColumnWidth');
    {$ENDIF}
    h := FFirstColumn;
    i := 0;
    if h = nil then	// not found
    begin
	result := DefaultColumnWidth;
	exit;
    end;
    while i < aIndex do
    begin
	if h = nil then	// not found - returns the default
	begin
	    result := DefaultColumnWidth;
	    exit;
	end;
	h := h^.next;	
	inc(i);
    end;
    if h <> nil then
	result := h^.width
    else  // not found -> returns the default
	result := DefaultColumnWidth;
end;

procedure TwgTree.SetColumnWidth(aindex, awidth : word);
var
    h : PwgTreeColumnWidth;
    n : PwgTreeColumnWidth;
    i : word;
begin
    {$IFDEF DEBUG}
    writeln('SetColumnWidth');
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

procedure TwgTree.SetDefaultColumnWidth(aValue : word);
begin
    {$IFDEF DEBUG}
    writeln('SetDefaultColumnWidth');
    {$ENDIF}
    if (aValue <> FDefaultColumnWidth) and (aValue > 3) then
    begin
	FDefaultColumnWidth := aValue;
	RePaint;
    end;
end;

procedure TwgTree.SetSelection(aValue : TwgTreeNode);
begin
    {$IFDEF DEBUG}
    writeln('SetSelection');
    {$ENDIF}
    if aValue <> FSelection then
    begin
	FSelection := aValue;
	RePaint;
    end;
end;

function TwgTree.GetRootNode : TwgTreeNode;
begin
    {$IFDEF DEBUG}
    writeln('GetRootNode');
    {$ENDIF}
    if FRootNode = nil then FRootNode := TwgTreeNode.Create;
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
	if FirstSubNode.Count > 0 then	// recourse
	    FirstSubNode.Clear;
	Remove(FirstSubNode);
    end;
end;

function TwgTreeNode.GetMaxVisibleDepth : integer;
var
    h : TwgTreeNode;
    a,t : integer;
begin
    {$IFDEF DEBUG}
    writeln('GetMaxVisibleDepth');
    {$ENDIF}
    result := 1;
    h := FirstSubNode;
    if h.Collapsed then exit;
    a := 0;
    while h <> nil do
    begin
	t := h.GetMaxDepth;
	if t > a then a := t;
	h := h.next;
    end;
    result := result + a;
end;

function TwgTreeNode.GetMaxDepth : integer;
var
    h : TwgTreeNode;
    a,t : integer;
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
	if t > a then a := t;
	h := h.next;
    end;
    result := result + a;
end;

function TwgTreeNode.FindSubNode(aValue : string16) : TwgTreeNode;
var
    h : TwgTreeNode;
begin
    result := nil;
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
	    result := h.FirstSubNode.FindSubNode(aValue);
	    if result <> nil then exit;
	end;
	h := h.next;
    end;
end;

function TwgTreeNode.CountRecurse : integer;
var
    h : TwgTreeNode;
    i : integer;
begin
    h := FFirstSubNode;
    i := 0;
    while h <> nil do
    begin
	i := i + h.CountRecurse;	// increases i by the count of the subnodes of the subnode
	h := h.next;
	inc(i);			// and the subnode...
    end;
    result := i;
end;

function TwgTreeNode.Count : integer;
var
    h : TwgTreeNode;
    i : integer;
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

function TwgTreeNode.AppendText8(aValue : string) : TwgTreeNode;
begin
    {$IFDEF DEBUG}
    writeln('AppendText8');
    {$ENDIF}
    result := AppendText(Str8To16(aValue));
end;

function TwgTreeNode.AppendText(aValue : string16) : TwgTreeNode;
var
    h : TwgTreeNode;
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

procedure TwgTreeNode.SetCollapsed(aValue : boolean);
begin
    if aValue <> FCollapsed then
    begin
	FCollapsed := aValue;
	DoRePaint;
    end;
end;

destructor TwgTreeNode.Destroy;
begin
    if FParent <> nil then FParent.UnregisterSubNode(self); // unregisteres self
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
    FParent := nil;
    FNext := nil;
    FPrev := nil;
    FSelColor := clUnset;
    FSelTextColor := clUnset;
    FTextColor := clUnset;
end;

procedure TwgTreeNode.DoRePaint;
begin
    { TODO }
    // call repaint from the Twgtree-widget the root-node is registered to
end;

procedure TwgTreeNode.UnRegisterSubNode(aNode : TwgTreeNode);
var
    h : TwgTreeNode;
begin
    h := FFirstSubNode;
    while h <> nil do
    begin
	if h = aNode then
	begin
	    if h = FFirstSubNode then FFirstSubNode := FFirstSubNode.Next;
	    if h = FLastSubNode then FLastSubNode := FLastSubNode.Prev;
	    if h.prev <> nil then h.prev.next := h.next;
	    if h.next <> nil then h.next.prev := h.prev;
	    exit;
	end;
	h := h.next;
    end;
end;

procedure TwgTreeNode.Append(aValue : TwgTreeNode);
begin
    {$IFDEF DEBUG}
    writeln('TwgTreeNode.Append');
    {$ENDIF} 
    aValue.Parent := self;
    
    aValue.next := nil;
    
    if FFirstSubNode = nil then FFirstSubNode := aValue;
    
    aValue.prev := FLastSubNode;
    
    if FLastSubNode <> nil then
	FLastSubNode.Next := aValue;
	
    FLastSubNode := aValue;
end;

function TwgTreeNode.GetText8 : string;
begin
    result := Str16To8(FText);
end;

procedure TwgTreeNode.SetText8(aValue : string);
begin
    if Str8To16(aValue) <> FText then
    begin
	FText := Str8To16(aValue);
	DoRePaint;
    end;
end;

procedure TwgTreeNode.SetParent(aValue : TwgTreeNode);
begin
    if aValue <> FParent then
    begin
	if FParent <> nil then FParent.UnRegisterSubNode(self); // unregisteres
	FParent := aValue;
	if FParent <> nil then
	begin
	    DoRePaint;	    
	end;
    end;
end;

procedure TwgTreeNode.SetText(aValue : string16);
begin
    if aValue <> FText then
    begin
	FText := aValue;
	DoRePaint;
    end;
end;

procedure TwgTreeNode.Remove(aNode : TwgTreeNode);
begin
    if FirstSubNode = aNode then
    begin
	FFirstSubNode := aNode.next;
	if FFirstSubNode <> nil then FFirstSubNode.Prev := nil;
    end
    else
	if aNode.prev <> nil then aNode.Prev.next := aNode.next;
    if LastSubNode = aNode then
    begin
	FLastSubNode := aNode.prev;
	if FLastSubNode <> nil then FLastSubNode.next := nil;
    end
    else
	if aNode.next <> nil then aNode.next.prev := aNode.prev;
    aNode.prev := nil;
    aNode.next := nil;
    aNode.parent := nil;
end;

procedure TwgTreeNode.SetSelTextColor(aValue : TgfxColor);
begin
    if FTextColor <> aValue then
    begin
	FSelTextColor := aValue;
	DoRePaint;
    end;
end;

procedure TwgTreeNode.SetSelColor(aValue : TgfxColor);
begin
    if FSelColor <> aValue then
    begin
	FSelColor := aValue;
	DoRePaint;
    end;
end;

procedure TwgTreeNode.SetTextColor(aValue : TgfxColor);
begin
    if FTextColor <> aValue then
    begin
	FTextColor := aValue;
	DoRePaint;
    end;
end;

function TwgTreenode.ParentTextColor : TgfxColor;
begin
    if TextColor <> clUnset then result := TextColor
    else
    begin
	if parent <> nil then result := parent.ParentTextColor
	else result := clText1;
    end;
end;

function TwgTreeNode.ParentSelTextColor : TgfxColor;
begin
    if SelTextColor <> clUnset then result := SelTextColor
    else
    begin
	if parent <> nil then result := parent.ParentSelTextColor
	else result := clSelectionText;
    end;
end;

function TwgTreeNode.ParentSelColor : TgfxColor;
begin
    if SelColor<>clUnset then result := SelColor
    else
    begin
	if parent <> nil then result := parent.ParentSelColor
	else result := clSelection;
    end;
end;

end.
