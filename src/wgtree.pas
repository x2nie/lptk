{
    feature-requests or bugs? - mail to: erik@grohnwaldt.de
    History
    08.05.2003  0.3	AppendText are now functions. it returns the new node
    06.05.2003	0.2	Bugfix in FindSubNode
    04.05.2003	0.1	Initial Release    
}

unit wgtree;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ENDIF}
// {$DEFINE DEBUG}

interface

uses classes, gfxwidget, schar16, gfxbase, wgscrollbar;

type
    TwgTreeNode = class
	private
	    FSubNodeFirst : TwgTreeNode;	// list-implementation for sub-nodes
	    FSubNodeLast : TwgTreeNode;
	    FNext : TwgTreeNode;
	    FPrev : TwgTreeNode;
	    
	    FText : string16;
	    FCollapsed : boolean;	    
	    FParent : TwgTreeNode;
	    procedure SetText8(aValue : string);
	    function GetText8 : string;
	public
	    property Next : TwgTreeNode read FNext write FNext;	// list implementation
	    property Prev : TwgTreeNode read FPrev write FPrev;
	    	
	    constructor Create;
	    function Count : integer;
	    procedure Append(aNode : TwgTreeNode);	    // appends a sub-node
	    function AppendText(aText : string16) : TwgTreeNode;	    // creates a sub-node with the given text
	    procedure Remove(aNode : TwgTreeNode);	    // removes the node with all sub-nodes - it doesn't release the memory, you have do call destroy
	    function FindSubNode(aText : string16) : TwgTreeNode;	// search for a sub-node - nil: not found
	    
	    property Text8 : string read GetText8 write SetText8;
	    property Text : string16 read FText write FText;
	    property Parent : TwgTreeNode read FParent write FParent;
	    
	    function Collapsed : boolean;
	    procedure Expand;
	    procedure Collapse;
	    function GetFirstSubNode : TwgTreeNode;
	    function GetLastSubNode : TwgTreeNode;
    end;

    TTreeSelectionChangeEvent = procedure(Sender : TObject; OldNode : TwgTreeNode; NewNode : TwgTreeNode);

    TwgTree = class(TWidget)
	private
	    FRoot : TwgTreeNode;
	    
	    FFirstNode : integer;	// needed for repaint
	    FLastNode : integer;
	    
	    FSpace : integer;		// pixel between node and subnodes
	    FFont : TgfxFont;
	    
	    FVScrollBar : TwgScrollBar;
	    FHScrollBar : TwgScrollBar;
	    
	    FMargin : integer;
	    FSelection : TwgTreeNode;
	    
	    FXOffset, FYOffset : integer;
	    procedure SetSpace(const aValue : integer);
	    function GetNodeHeightSum : integer;
	    function GetXPos(aNode : TwgTreeNode) : integer;
	    function GetYPos(aNode : TwgTreeNode) : integer;
	    
	    function GetMaxXSize : integer;
	    procedure SetSelection(aValue : TwgTreeNode);
	    procedure VScrollbarMove(Sender : TObject; Position : integer);
	    procedure HScrollbarMove(Sender : TObject; Position : integer);

	    function FindNodeByY( y : integer) : TwgTreeNode;
	    function GetRootNode : TwgTreeNode;
	protected
	    function VisibleHeight : integer;
	    function VisibleWidth : integer;
	    procedure UpdateScrollbars;
	    procedure HandleKeyPress(var KeyCode : word; var Shiftstate : word; var consumed : boolean); override;
	    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
	    procedure HandleResize(dWidth, dHeight : integer); override;
	    function GetPrevVisibleNode(aNode : TwgTreeNode) : TwgTreeNode;
	    function GetNextVisibleNode(aNode : TwgTreenode) : TwgTreeNode;
	    procedure DoSelectionChange(OldNode, NewNode : TwgTreeNode);
	public
	    procedure RePaint; override;
	    procedure DoShow; override;
	    constructor create(aOwner : TComponent); override;
	    property RootNode : TwgTreeNode read GetRootNode;
	    property Space : integer read FSpace write SetSpace;
	    property Selection : TwgTreeNode read FSelection write SetSelection;	    
	public
	    OnSelectionChange : TTreeSelectionChangeEvent;
    end;

implementation

uses gfxstyle;

{ TwgTree }

procedure TwgTree.HandleResize(dwidth, dheight : integer);
begin
    inherited HandleResize(dwidth,dheight);
    UpdateScrollbars;
    DoShow;
    RePaint;
end;

function TwgTree.FindNodeByY( y : integer) : TwgTreeNode;
var
    next : TwgTreeNode;
    yp : integer;
begin
    next := RootNode;
    result := nil;
    while next <> nil do
    begin
	if next.Collapsed then	// es muss nur die node selbst gezeichnet werden
	begin
	    yp := GetYPos(next) * (FFont.Ascent + FFont.Descent);
	    if (y > yp) and (y < yp + FFont.Ascent + FFont.Descent) then
	    begin
		result := next;
		exit;
	    end;
	    if next.next = nil then
	    begin
	      while next.next = nil do
	      begin
		if next.parent = nil then break;
		next := next.parent;		
	      end;
	      if next.next <> nil then next := next.next;
	    end
	    else
	      next := next.next;
	end
	else
	begin
	    yp := GetYPos(next) * (FFont.Ascent + FFont.Descent);
	    if (y > yp) and (y < yp + FFont.Ascent + FFont.Descent) then
	    begin
		result := next;
		exit;
	    end;	
	    if next.count > 0 then	// nur dann gibt es untergeordnete nodes
	    begin
		next := next.GetFirstSubNode;
	    end
	    else
	    begin	// es gibt keine unternodes
		if next.next <> nil then // aber nachfolgende nodes
		begin
		    next := next.next;
		end
		else
		begin
		    while next.next = nil do
		    begin
			if next.parent = nil then break;
			next := next.parent;	// geht zurueck, solange, bis es nachfolge-nodes gibt
		    end;
		    if next.next <> nil then next := next.next;
		end;
	    end;
	end;
	if next.Parent = nil then next := nil; // root-node, abort
    end;    
end;

procedure TwgTree.HandleMouseUp(x,y : integer; button : word; shiftstate : word);
var
    xp, yp, xpos : integer;
    fn : TwgTreeNode;
    os : TwgTreeNode;
begin
    inherited HandleMouseUp(x,y, button, shiftstate);
    if Button = 1 then
    begin
      os := Selection;
      xp := x + FXOffset;
      yp := y + FYOffset;
      fn := FindNodeByY(yp);
      if fn <> nil then	// selection change
      begin
	xpos := GetXPos(fn) * FSpace + FMargin+2;
	Selection := fn;
	if (xp > xpos - FSpace) and (xp < xpos) then
	begin
	  if fn <> RootNode then
	  begin
	    if (fn.count > 0) and (fn.Collapsed) then fn.Expand
	    else if (fn.count > 0) and (not fn.Collapsed) then fn.Collapse;	    
	  end;
	end;    
      end;
      UpdateScrollbars;
      if Windowed then 
      begin
	DoShow;
	RePaint;
      end;
      if Selection <> os then
      begin
  	DoSelectionChange(os, Selection);
      end;
    end;
end;

procedure TwgTree.DoSelectionChange(OldNode, NewNode : TwgTreeNode);
begin
    if Assigned(OnSelectionChange) then OnSelectionChange(self, OldNode, NewNode);
end;

function TwgTree.GetPrevVisibleNode(aNode : TwgTreeNode) : TwgTreeNode;
var
    no : TwgTreeNode;
begin
    no := aNode;
    if aNode.Prev <> nil then
    begin
	result := aNode.Prev;
	aNode := aNode.Prev;
	while (not aNode.Collapsed) and (aNode.Count > 0) do
	begin
	    result := aNode.GetLastSubNode;
	    aNode := aNode.GetLastSubNode;
	end;
    end
    else
    begin
	if aNode.Parent <> nil then
	begin
	    result := aNode.Parent;	    
	end
	else result := No;
    end;
end;

function TwgTree.GetNextVisibleNode(aNode : TwgTreeNode) : TwgTreeNode;
var
    no : TwgTreeNode;
begin
    no := aNode;
    result := nil;
    if (aNode.Count > 0) and (not aNode.Collapsed) then
    begin
	result := aNode.GetFirstSubNode;
    end
    else
    begin
	if aNode.Next <> nil then result := aNode.Next
	else
	begin
	    while aNode.next = nil do
	    begin
		if aNode.Parent = nil then
		begin
		    result := no;
		    exit;
		end;
		aNode := aNode.Parent;
	    end;
	    result := aNode.next;
	end;
    end;
end;

procedure TwgTree.HandleKeyPress(var KeyCode : word; var Shiftstate : word; var consumed : boolean);
var
    OldSel : TwgTreeNode;    
    CheckPosition : boolean;    
begin
    OldSel := Selection;
    consumed := true;
    CheckPosition := false;
    case keycode of
	KEY_ENTER, $0020: begin  // change collapsed state
	    if Selection = RootNode then exit;
	    if Selection = nil then Selection := RootNode;
	    if Selection.Collapsed then 
	    begin
		Selection.Expand;
		if Selection.Count > 0 then Selection := GetNextVisibleNode(Selection);
	    end
	    else
		Selection.Collapse;
	    CheckPosition := true;
	end;
	KEY_RIGHT: begin // expand
	    if Selection = RootNode then exit;
	    if Selection = nil then Selection := RootNode;
	    Selection.Expand;
	    if Selection.Count > 0 then
		Selection := GetNextVisibleNode(Selection);	    
	    CheckPosition := true;
	end;
	KEY_LEFT: begin  // collapse
	    if Selection = RootNode then exit;
	    if Selection = nil then Selection := RootNode;
	    Selection.Collapse;
	    CheckPosition := true;
	end;
	KEY_UP: begin  // goto prev. node
	    if Selection <> nil then
	    begin
		Selection := GetPrevVisibleNode(Selection);
	    end
	    else
		Selection := RootNode;
	    CheckPosition := true;
	end;
	KEY_DOWN: begin // goto next node
	    if Selection <> nil then
	    begin
		Selection := GetNextVisibleNode(Selection);
	    end
	    else Selection := RootNode;
	    CheckPosition := true;	    
	end;
	else Consumed := false;
    end;
    if CheckPosition then
    begin
    	    while ((GetYPos(Selection)+1) * (FFont.Ascent + FFont.Descent)) - FYOffset > Visibleheight do	// out of display
	    begin
		FYOffset := FYOffset + FFont.Ascent + FFont.Descent;
	    end;
	    
	    while ((GetYPOs(Selection)) * (FFont.Ascent + FFont.Descent)) - FYOffset < 0 do
	    begin
		FYOffset := FYOffset - (FFont.Ascent + FFont.Descent);
	    end;
	    if FVScrollbar.Position <> FYOffset then
	    begin
 		FVScrollbar.Position := FYOffset;
		if FVScrollbar.Windowed then FVScrollBar.RePaint;	    
	    end;
	    UpdateScrollBars;
    end;
    if Consumed then inherited HandleKeyPress(KeyCode, ShiftState, Consumed);
    if Windowed then RePaint;
    if OldSel<>Selection then 
    begin
	if Windowed then RePaint;
	DoSelectionChange(OldSel, Selection);
    end;
end;

function TwgTree.GetRootNode : TwgTreeNode;
begin
    if FRoot = nil then FRoot := TwgTreeNode.Create;
    result := FRoot;
end;

function TwgTree.GetMaxXSize : integer;
var
    next : TwgTreeNode;
    i : integer;
begin
    next := RootNode;
    result := 0;
    while next <> nil do
    begin
	if next.Collapsed then	// es muss nur die node selbst gezeichnet werden
	begin
	    i := FSpace * GetXPos(next) + FFont.TextWidth16(next.text) + FMargin + 2;
	    if i > result then result := i;
	    if next.next = nil then
	    begin
	      while next.next = nil do
	      begin
		if next.parent = nil then break;
		next := next.parent;		
	      end;
	      if next.next <> nil then next := next.next;
	    end
	    else
	      next := next.next;
	end
	else
	begin
	    i := FSpace * GetXPos(next) + FFont.TextWidth16(next.text) + FMargin + 2;
	    if i > result then result := i;
	    if next.count > 0 then	// nur dann gibt es untergeordnete nodes
	    begin
		next := next.GetFirstSubNode;
	    end
	    else
	    begin	// es gibt keine unternodes
		if next.next <> nil then // aber nachfolgende nodes
		begin
		    next := next.next;
		end
		else
		begin
		    while next.next = nil do
		    begin
			if next.parent = nil then break;
			next := next.parent;	// geht zurueck, solange, bis es nachfolge-nodes gibt
		    end;
		    if next.next <> nil then next := next.next;
		end;
	    end;
	end;
	if next.Parent = nil then next := nil; // root-node, abort
    end;    
end;


procedure TwgTree.VScrollbarMove(Sender : TObject; Position : integer);
begin
    FYOffset := Position;
    if Windowed then RePaint;
end;

procedure TwgTree.HScrollbarMove(Sender : TObject; Position : integer);
begin
    FXOffset := Position;
    if Windowed then RePaint;
end;

procedure TwgTree.UpdateScrollBars;
begin
    FVScrollbar.Visible := VisibleHeight < GetNodeHeightSum * (FFont.Ascent + FFont.Descent);
    FVScrollbar.Min := 0;
    FVScrollbar.Max := (GetNodeHeightSum-1) * (FFont.Ascent + FFont.Descent);
    FHscrollbar.Min := 0;
    FHScrollbar.Max := GetMaxXSize - VisibleWidth;
    FHScrollbar.Visible := GetMaxXSize > Width - FMargin * 2;
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

function TwgTree.VisibleHeight : integer;
begin
    if GetMaxXSize > Width - FMargin * 2 then
	VisibleHeight := (Height - 2*FMargin) - FHScrollbar.Height
    else
    begin
	VisibleHeight := (Height - 2*FMargin);
    end;
end;

function TwgTree.VisibleWidth : integer;
begin
    if GetNodeHeightSum * (FFont.Ascent + FFont.Descent) > Height - FMargin * 2 then VisibleWidth := (Width - 2*FMargin) - FVScrollbar.Width
    else
	VisibleWidth := Width - 2*FMargin;
end;

procedure TwgTree.DoShow;
begin
    inherited DoShow;
    UpdateScrollbars;
    if FHScrollbar.Visible then
    begin
	FVScrollbar.SetDimensions((Width - 18)-FMargin,FMargin, 18, (Height - 2*FMargin)-18);
    end
    else
	FVScrollbar.SetDimensions((Width - 18) - FMargin, FMargin, 18, Height - 2*FMargin);
    FHScrollBar.SetDimensions(FMargin, (Height - FMargin) - 18, (Width - 2*FMargin), 18);
end;

procedure TwgTree.SetSelection(aValue : TwgTreeNode);
begin
    if aValue <> FSelection then
    begin
	FSelection := aValue;	// setzen
	if Windowed then RePaint; // und neu zeichnen, wenn der tree schon dargestellt wird 
    end;
end;

function TwgTree.GetYPos(aNode : TwgTreeNode) : integer;
var
    next : TwgTreeNode;
    i : integer;
begin
    next := RootNode;
    i := 0;
    result := -1;
    while next <> nil do
    begin
	if next = aNode then 
	begin
	    result := i;	// gefunden 
	    exit;
	end;
	if next.Collapsed then	// es muss nur die node selbst gezeichnet werden
	begin
	    inc(i);
	    if next.next = nil then
	    begin
	      while next.next = nil do
	      begin
		if next.parent = nil then break;
		next := next.parent;		
	      end;
	      if next.next <> nil then next := next.next;
	    end
	    else
	      next := next.next;
	end
	else
	begin
	    inc(i);
	    if next.count > 0 then	// nur dann gibt es untergeordnete nodes
	    begin
		next := next.GetFirstSubNode;
	    end
	    else
	    begin	// es gibt keine unternodes
		if next.next <> nil then // aber nachfolgende nodes
		begin
		    next := next.next;
		end
		else
		begin
		    while next.next = nil do
		    begin
			if next.parent = nil then break;
			next := next.parent;	// geht zurueck, solange, bis es nachfolge-nodes gibt
		    end;
		    if next.next <> nil then next := next.next;
		end;
	    end;
	end;
	if next.Parent = nil then next := nil; // root-node, abort
    end;    
end;

function TwgTree.GetXPos(aNode : TwgTreeNode) : integer;
var
    h : TwgTreeNode;
    i : integer;
begin
    h := aNode;
    i := -1;
    while h <> nil do
    begin
	h := h.parent;
	inc(i);
    end;
    result := i;
end;

function TwgTree.GetNodeHeightSum : integer;
var
    i : integer;
    next : TwgTreeNode;
begin
    // die maximale y-ausdehnung aller nodes in ihrem aktuellen zustand
    next := RootNode;
    i := 0;
    while next <> nil do
    begin
	if next.Collapsed then	// es muss nur die node selbst gezeichnet werden
	begin
	    inc(i);
	    if next.next = nil then
	    begin
	      while next.next = nil do
	      begin
		if next.parent = nil then break;
		next := next.parent;		
	      end;
	      if next.next <> nil then next := next.next;
	    end
	    else
	      next := next.next;
	end
	else
	begin
	    inc(i);
	    if next.count > 0 then	// nur dann gibt es untergeordnete nodes
	    begin
		next := next.GetFirstSubNode;
	    end
	    else
	    begin	// es gibt keine unternodes
		if next.next <> nil then // aber nachfolgende nodes
		begin
		    next := next.next;
		end
		else
		begin
		    while next.next = nil do
		    begin
			if next.parent = nil then break;
			next := next.parent;	// geht zurueck, solange, bis es nachfolge-nodes gibt
		    end;
		    if next.next <> nil then next := next.next;
		end;
	    end;
	end;
	if next.Parent = nil then next := nil; // root-node, abort
    end;
    result := i;
end;

procedure TwgTree.SetSpace(const aValue : integer);
begin
    if aValue <> FSpace then
    begin
	if FSpace < FFont.TextWidth16(Str8to16('+')) then exit; // das kreuz kann nicht angezeigt werden
	FSpace := aValue;
	if Windowed then RePaint;
    end;
end;

procedure TwgTree.RePaint;
var
    next : TwgTreeNode;
    TextSize : integer;
    r : TGfxRect;
begin
    {$IFDEF DEBUG}
    writeln('TwgTree.RePaint');
    {$ENDIF}
  if Windowed then
  begin
    inherited RePaint;
    UpdateScrollBars;
    Canvas.ClearClipRect;
    r.SetRect(FMargin,FMargin, VisibleWidth, VisibleHeight);
    Canvas.SetClipRect(r);
    Canvas.Clear(FBackgroundColor);
    Canvas.SetFont(FFont);
    if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0,0,width,height);  // den rahmen zeichnen
    next := RootNode;
    TextSize := FFont.Ascent + FFont.Descent;
    Canvas.SetColor(clInactiveWgFrame);
    while next <> nil do
    begin
	if next.Collapsed then	// es muss nur die node selbst gezeichnet werden
	begin
	    Canvas.SetColor(clInactiveWgFrame);
	    if next.parent <> nil then
	    begin
		if next.prev <> nil then
		begin
		    Canvas.DrawLine(FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace * 1/3) - FXOffset, TextSize div 2 + FMargin + 2 + (GetYPos(next.prev))*TextSize - FYOffset, FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace*1/3) - FXOffset, FMargin + 2 + (GetYPos(next))*TextSize + TextSize div 2 - FYOffset);
		end;
		Canvas.DrawLine(FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace * 1/3) - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize - FYOffset, FMargin+2+(GetXPos(next)-1)*FSpace+FSpace div 2 + FSpace div 4 - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize - FYOffset);
	    end;
	    if FSelection = next then
	    begin
		if Focused then 
		begin
		    Canvas.SetTextColor(clSelectionText);
		    Canvas.SetColor(clSelection);
		end
		else 
		begin
		    Canvas.SetTextColor(clInactiveSelText);	    
		    Canvas.SetColor(clInactiveSel);
		end;
		Canvas.FillRectangle(FMargin + 2 + GetXPos(next) * FSpace - FXOffset, FMargin + 2 + GetYPos(next) * TextSize - FYOffset, FFont.TextWidth16(next.text),TextSize);
	    end
	    else
	    begin
		Canvas.SetTextColor(clText1);
	    end;	    
	    Canvas.DrawString16(FMargin + 2 + GetXPos(next) * FSpace - FXOffset, FFont.Ascent + FMargin + 2 + GetYPos(next) * TextSize - FYOffset, next.Text);
	    if next.Count > 0 then 
		    Canvas.DrawLine(FMargin+2+(GetXPos(next)-1)*FSpace+FSpace div 2 - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize - 2 - FYOffset, FMargin+2+(GetXPos(next)-1)*FSpace+FSpace div 2 - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize +2 - FYOffset);
	    if next.next = nil then
	    begin
	      while next.next = nil do
	      begin
		if next.parent = nil then break;
		next := next.parent;		
	      end;
	      if next.next <> nil then next := next.next;
	    end
	    else
	      next := next.next;
	end
	else
	begin
	    Canvas.SetColor(clInactiveWgFrame);	
	    
	    if next.parent <> nil then // draw tree-lines
	    begin
		if next.prev <> nil then
		begin
		    Canvas.DrawLine(FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace * 1/3) - FXOffset, TextSize div 2 + FMargin + 2 + (GetYPos(next.prev))*TextSize - FYOffset, FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace*1/3) - FXOffset, FMargin + 2 + (GetYPos(next))*TextSize + TextSize div 2 - FYOffset);
		end;
		Canvas.DrawLine(FMargin+2+(GetXPos(next)-1)*FSpace+trunc(FSpace * 1/3) - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize - FYOffset, FMargin+2+(GetXPos(next)-1)*FSpace+FSpace div 2 + FSpace div 4 - FXOffset, FMargin + 2 + TextSize div 2 + (GetYPos(next))*TextSize - FYOffset);
	    end;
	    if FSelection = next then // draw selection
	    begin
		if Focused then 
		begin
		    Canvas.SetTextColor(clSelectionText);
		    Canvas.SetColor(clSelection);
		end
		else 
		begin
		    Canvas.SetTextColor(clInactiveSelText);	    
		    Canvas.SetColor(clInactiveSel);
		end;
		Canvas.FillRectangle(FMargin + 2 + GetXPos(next) * FSpace - FXOffset, FMargin + 2 + GetYPos(next) * TextSize - FYOffset, FFont.TextWidth16(next.text),TextSize);
	    end
	    else
	    begin
		Canvas.SetTextColor(clText1);
	    end;
	    
	    Canvas.DrawString16(FMargin + 2 + GetXPos(next) * FSpace - FXOffset, FFont.Ascent + FMargin + 2 + GetYPos(next) * TextSize - FYOffset, next.Text); //draw node-text
	    if next.count > 0 then	// nur dann gibt es untergeordnete nodes
	    begin
		next := next.GetFirstSubNode;
	    end
	    else
	    begin	// es gibt keine unternodes
		if next.next <> nil then // aber nachfolgende nodes
		begin
		    next := next.next;
		end
		else
		begin
		    while next.next = nil do
		    begin
			if next.parent = nil then break;
			next := next.parent;	// geht zurueck, solange, bis es nachfolge-nodes gibt
		    end;
		    if next.next <> nil then next := next.next;
		end;
	    end;
	end;
	if next.Parent = nil then next := nil; // root-node, abort
    end;
  end;
end;

constructor TwgTree.Create(aOwner : TComponent);
begin
    {$IFDEF DEBUG}
    writeln('TwgTree.Create');
    {$ENDIF}
    inherited Create(aOwner);
    FFocusable := true;
    FHeight := 50;
    FWidth := 50;
    onSelectionChange := nil;
    FBackgroundColor := clListBox;
    FFirstNode := 0;
    FLastNode := 0;
    FFont := guistyle.ListFont;
    FVScrollBar := TwgScrollBar.Create(self);
    FVScrollbar.Orientation := orVertical;
    FVScrollbar.Position := 0; 
    FVScrollbar.OnScroll := {$IFDEF FPC}@{$ENDIF}VScrollbarMove;
    FHScrollBar := TwgScrollBar.Create(self);
    FHScrollBar.Orientation := orHorizontal;
    FHScrollbar.Position := 0;
    FHScrollbar.OnScroll := {$IFDEF FPC}@{$ENDIF}HScrollbarMove;    
    FSpace := FFont.TextWidth16(Str8to16('+'));
    FMargin := 1;
end;

{ TwgTreeNode }

function TwgTreeNode.GetFirstSubNode : TwgTreeNode;
begin
    result := FSubNodeFirst;
end;

function TwgTreeNode.AppendText(aText : string16) : TwgTreeNode;
var
    h : TwgTreeNode;
begin
    h := TwgTreeNode.Create;
    h.text := aText;
    h.parent := self;
    h.next := nil;
    if FSubNodeFirst = nil then FSubNodeFirst := h;
    if FSubNodeLast = nil then 
    begin
	h.Prev := nil;	
    end
    else 
    begin
	h.prev := FSubNodeLast;
	FSubNodeLast.Next := h;
    end;
    FSubNodeLast := h;
    result := h;
end;

procedure TwgTreeNode.Expand;
begin
    if FCollapsed then
    begin
	FCollapsed := false;
	// TODO - send repaint
    end;
end;

function TwgTreeNode.Collapsed : boolean;
begin
    result := FCollapsed;
end;

function TwgTreeNode.Count : integer;
var
    h : TwgTreeNode;
    i : integer;
begin
    h := FSubNodeFirst;
    i := 0;
    while h <> nil do
    begin
	inc(i);
	h := h.next;
    end;
    result := i;
end;

function TwgTreeNode.FindSubNode(aText : string16) : TwgTreeNode;
var
    h : TwgTreeNode;
begin
    result := nil;
    h := FSubNodeFirst;
    while h <> nil do
    begin
	if h.Text = aText then
	begin
	    result := h;
	    exit;
	end;
	if h.Count > 0 then h := GetFirstSubNode
	else
	begin
	    if h.next <> nil then h := h.next
	    else
	    begin
		while h.next = nil do
		begin
		    h := h.parent;
		    if h = nil then exit;
		end;
	    end;
	end;
    end;
end;

constructor TwgTreeNode.Create;
begin
    {$IFDEF DEBUG}
    writeln('TwgTreeNode.Create');
    {$ENDIF}
    FNext := nil;
    FPrev := nil;
    FCollapsed := true;
    FText := '';
    FSubNodeFirst := nil;
    FSubNodeLast := nil;
    FParent := nil;
end;

procedure TwgTreeNode.Remove(aNode : TwgTreeNode);
begin
    {$IFDEF DEBUG}
    writeln('TwgTreeNode.Remove');
    {$ENDIF}
    // TODO - drive into subnodes to search for the aNode
    if Parent = nil then exit; // root-node
    if FSubNodeFirst = aNode then 
    begin
	FSubNodeFirst := aNode.next;
	FSubNodeFirst.Prev := nil;
    end
    else
	if aNode.Prev <> nil then aNode.Prev.Next := aNode.Next;	
    if FSubNodeLast = aNode then 
    begin
	FSubNodeLast := aNode.Prev;
	FSubNodeLast.Next := nil;
    end
    else
	if aNode.Next <> nil then aNode.Next.Prev := aNode.Prev;
    aNode.Prev := nil;
    aNode.Next := nil;    
end;

procedure TwgTreeNode.Append(aNode : TwgTreeNode);
begin
    {$IFDEF DEBUG}
    writeln('TwgTreeNode.Append');
    {$ENDIF}
    if aNode <> nil then
    begin
	aNode.Parent := self;
	aNode.Next := nil;
	if FSubNodeFirst = nil then
	    FSubNodeFirst := aNode;
	FSubNodeLast.Next := aNode;
	aNode.prev := FSubNodeLast;
	FSubNodeLast := aNode;
    end;
end;

procedure TwgTreenode.Collapse;
begin
    FCollapsed := true;
end;

function TwgTreenode.GetLastSubNode : TwgTreeNode;
begin
    result := FSubNodeLast;
end;

procedure TwgTreenode.SetText8(aValue : string);
begin
    FText := Str8To16(aValue);
end;

function TwgTreeNode.GetText8 : string;
begin
    result := Str16To8(FText);
end;

end.