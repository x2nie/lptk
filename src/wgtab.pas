{ 	feature-requests or bugs? - mail to: erik@grohnwaldt.de
}

// $Log$
// Revision 1.5  2004/04/19 04:26:39  nvitya
// compatibility fix
//
// Revision 1.4  2004/02/25 06:41:53  aegluke
// Bugfix in SetFixedTabWidth with empty Tab-Caption
//

{
	History
	19.06.2003	0.4	property Text8 for TwgTabSheet
	16.05.2003	0.3	new property: FixedTabWidth
	15.05.2003		nvitya: components focusing at page activation
	12.05.2003	0.2	new property NavBar : TWinPosition;
				- wpTop and wpBottom
				- visual enhancements
	11.05.2003	0.1 	First Release
}

unit
    wgtab;


{$IFDEF FPC}
    {$mode objfpc}{$H+}
{$ENDIF}

// {$DEFINE DEBUG}

interface

uses gfxbase, gfxstyle, classes, gfxwidget, schar16, wgbutton;

type
    TWinPosition = (wpTop, wpBottom);

    TwgTabSheet = class(TWidget)
	private
	    FText : string16;
	    FIndex : word;
	    procedure SetText(aValue : string16);
	    procedure SetText8(aValue : string);
	    function GetText8 : string;
	protected
	public
	    constructor Create(aOwner : TComponent); override;
	    destructor Destroy; override;

	    property Text : string16 read FText write SetText;
	    property Text8 : string read GetText8 write SetText8;
	    property Index : word read FIndex write FIndex;
    end;

    PTabSheetList = ^TwgTabSheetList;

    TwgTabSheetList = record
	    next : PTabSheetList;
	    prev : PTabSheetList;
	    TabSheet : TwgTabSheet;
    end;

    TTabSheetChange = procedure(Sender : TObject; NewActiveSheet : TwgTabSheet);

    TwgTabControl = class(TWidget)
	  private
	    FActiveSheet : TwgTabSheet;
	    FNavBar : TWinPosition;

	    FFirstTabSheet : PTabSheetList;
	    FFont : TgfxFont;
	    FMargin : integer;
	    FFirstTabButton : PTabSheetList;

	    FLeftButton : TwgButton;
	    FRightButton : TwgButton;
	    FFixedTabWidth : integer;

	    procedure LeftButtonClick(Sender : TObject);
	    procedure RightButtonClick(Sender : TObject);
	    procedure SetActiveSheet(aValue : TwgTabSheet);
	    procedure SetNavBar(aValue : TWinPosition);
	    function TabSheetIndex(TabSheet : TwgTabSheet) : integer;
	    function MaxButtonWidthSum : integer;
	    function MaxButtonHeight : integer;
	    function MaxButtonWidth : integer;
	    function ButtonWidth(aText : string16) : integer;
	    function ButtonHeight : integer;

	    procedure OrderSheets; // currently using bubblesort
	    procedure SetFixedTabWidth(aValue : integer);
	    function GetTabText(aText : string16) : string16;
	  protected
	    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
	    procedure HandleKeyPress(var KeyCode : word; var ShiftState : word; var consumed : boolean); override;
	    procedure HandleResize(dWidth, dHeight : integer); override;
	    procedure DoChange(NewActive : TwgTabSheet);
	  public
	    onChange : TTabSheetChange;

	    constructor Create(aOwner : TComponent); override;

	    procedure DoShow; override;
	    procedure UnregisterTabSheet(TabSheet : TwgTabSheet);
	    procedure RegisterTabSheet(TabSheet : TwgTabSheet);

	    property ActiveTabSheet : TwgTabSheet read FActiveSheet write SetActiveSheet;
	    property Navbar : TWinPosition read FNavBar write SetNavBar;
	    property FixedTabWidth : integer read FFixedTabWidth write SetFixedTabWidth;
	    procedure RePaint; override;
	    procedure RePaintTitles;
	    function TabSheetCount : integer;
	    function MaxTabIndex : word;
	    function AppendTabSheet(Title : string16) : TwgTabSheet;

    end;

implementation

{ TwgTabSheet }

procedure TwgTabSheet.SetText8(aValue : string);
begin
    Text := Str8To16(aValue);
end;

function TwgTabSheet.GetText8 : string;
begin
    result := Str16to8(Text);
end;

procedure TwgTabSheet.SetText(aValue : string16);
begin
    {$IFDEF DEBUG}writeln('TabSheet.SetText');{$ENDIF}
    if Owner is TwgTabControl then
    begin
	    FText := aValue;
	    TwgTabControl(Owner).RePaintTitles;
    end;
end;

constructor TwgTabSheet.Create(aOwner : TComponent);
begin
    {$IFDEF DEBUG}writeln('TabSheet.Create');{$ENDIF}
    inherited Create(aOwner);
    FFocusable := true;
    if Owner is TwgTabControl then
    begin
	TwgTabControl(Owner).RegisterTabSheet(self);
	FIndex := TwgTabControl(Owner).MaxTabIndex + 1;
    end;
end;

destructor TwgTabSheet.Destroy;
begin
    {$IFDEF DEBUG}writeln('TabSheet.Destroy');{$ENDIF}
    if Owner is TwgTabControl then
	TwgTabControl(Owner).UnregisterTabSheet(self);
    inherited Destroy;
end;

{ TwgTabControl }

function TwgTabControl.GetTabText(aText : string16) : string16;
var
    s, s1 : string;
    i : integer;
begin
    {$IFDEF DEBUG}writeln('TwgTabControl.GetTabText');{$ENDIF}
    result := aText;
    s := Str16To8(aText);
    s1 := '';
    i := 1;
    if FFixedTabWidth > 0 then
    begin
	    while FFont.TextWidth16(Str8to16(s1)) < FFixedTabWidth - 10 do
	    begin
	      if length(s1) = length(s) then break;
	      s1 := copy(s,1,i);
	      inc(i);
	    end;
	    if FFont.TextWidth16(Str8To16(s1)) > FFixedTabWidth - 10 then
	      delete(s1,length(s1),1);
      if length(s1) > 0 then
	      if s1[length(s1)] = ' ' then delete(s1,length(s1),1);
      result := Str8to16(s1);
    end;
end;

procedure TwgTabControl.SetFixedTabWidth(aValue : integer);
begin
    if aValue <> FFixedTabWidth then
    begin
	    if aValue >= 5 then
	    begin
	      FFixedTabWidth := aValue;
	      RePaint;
	    end;
    end;
end;

function TwgTabControl.ButtonWidth(aText : string16) : integer;
begin
    if FFixedTabWidth > 0 then
    begin
	    result := FFixedTabWidth;
    end
    else
	result := FFont.TextWidth16(aText) + 10;
end;

function TwgTabControl.MaxButtonWidth : integer;
var
    h : PTabSheetList;
begin
    result := 0;
    h := FFirstTabSheet;
    if h = nil then exit;
    while h^.prev <> nil do h := h^.prev;
    while h <> nil do
    begin
	    if ButtonWidth(h^.TabSheet.Text) > result then result := ButtonWidth(h^.TabSheet.Text);
	    h := h^.next;
    end;
end;

function TwgTabControl.MaxButtonHeight : integer;
begin
    result := TabSheetCount * ButtonHeight;
end;

procedure TwgTabControl.HandleResize(dwidth, dheight : integer);
begin
    RePaint;
end;

procedure TwgTabControl.HandleKeyPress(var KeyCode : word; var Shiftstate : word; var consumed : boolean);
var
    h : PTabSheetList;
begin
    h := FFirstTabSheet;
    consumed := true;
    if h <> nil then
	case KeyCode of
	    KEY_LEFT: begin
		while h^.prev <> nil do h := h^.prev;
		while h <> nil do
		begin
		    if h^.TabSheet = FActiveSheet then
		    begin
			break;
		    end;
		    h := h^.next;
		end;
		if h^.prev <> nil then
		    ActiveTabSheet := h^.prev^.TabSheet;
	    end;
	    KEY_RIGHT:begin
		while h^.prev <> nil do h := h^.prev;
		while h <> nil do
		begin
		    if h^.TabSheet = FActiveSheet then break;
		    h := h^.next;
		end;
		if h^.next <> nil then
		    ActiveTabSheet := h^.next^.tabsheet;
	    end;
	    else consumed := false;
	end;
    inherited HandleKeyPress(KeyCode, ShiftState, Consumed);
end;

procedure TwgTabControl.LeftButtonClick(Sender : TObject);
begin
    {$IFDEF DEBUG}writeln('TabControl.LeftButtonClick');{$ENDIF}
    if FFirstTabButton <> nil then
    begin
	    if FFirstTabButton^.Prev <> nil then
	    begin
	        FFirstTabButton := FFirstTabButton^.Prev;
	        RePaint;
	    end;
    end;
end;

procedure TwgTabControl.RightButtonClick(Sender : TObject);
begin
    {$IFDEF DEBUG}writeln('TabControl.RightButtonClick');{$ENDIF}
    if FFirstTabButton <> nil then
    begin
	    if FFirstTabButton^.Next <> nil then
	    begin
	        FFirstTabButton := FFirstTabButton^.Next;
	        RePaint;
      end;
    end;
end;

procedure TwgTabControl.OrderSheets;
var
    h : PTabSheetList;
    changed : boolean;
    next : PTabSheetList;
    prev : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TwgTabControl.OrderSheets');{$ENDIF}
    h := FFirstTabSheet;
    if h = nil then exit;
    changed := true;
    while changed do
    begin
      h := FFirstTabSheet;
      changed := false;
      while h^.prev <> nil do h := h^.prev;
      FFirstTabSheet := h;
      while h <> nil do
      begin
	if h^.next <> nil then
	begin
	    if h^.Tabsheet.index > h^.next^.Tabsheet.Index then // change?
	    begin
		changed := true;
		next := h^.next;
		prev := h^.prev;

		if prev <> nil then prev^.next := next;
		next^.prev := prev;
		h^.next := next^.next;

		if next^.next <> nil then next^.next^.prev := h;
		next^.next := h;

		h^.prev := next;
	    end;
	end;
	h := h^.next;
      end;
    end;
end;

procedure TwgTabControl.DoChange(NewActive : TwgTabSheet);
begin
    if Assigned(OnChange) then OnChange(self,NewActive);
end;

procedure TwgTabControl.HandleMouseUp(x,y : integer; button : word; shiftstate : word);
var
    h : PTabSheetList;
    lp : integer;
    bw : integer; // button width
begin
    h := FFirstTabSheet;
    if h = nil then exit;
    lp := FMargin;
    while h^.prev <> nil do h := h^.prev; // first tabsheet
    if MaxButtonWidthSum > Width-FMargin * 2 then
    begin
	h := FFirstTabButton;
    end;
    case FNavBar of
	wpTop : begin
    	   if (y > FMargin) and (y < buttonheight) then
	   begin
		while h <> nil do
		begin
		    bw := ButtonWidth(h^.Tabsheet.Text);	// initialize button width
		    if (x > lp) and (x < lp + bw) then
		    begin
			if h^.TabSheet <> ActiveTabSheet then
			begin
			    ActiveTabSheet := h^.TabSheet;
			    DoChange(ActiveTabSheet);
			end;
			exit;
		    end;
		    lp := lp + bw;
		    h := h^.next;
		end;
	    end;
	end;
	wpBottom : begin
	    if (y > Height - FMargin - buttonheight) and (y < height - FMargin) then
	    begin
		while h <> nil do
		begin
		    bw := ButtonWidth(h^.TabSheet.Text);	// initialize button width
		    if (x > lp) and (x < lp + bw) then
		    begin
			if h^.TabSheet <> ActiveTabSheet then
			begin
			    ActiveTabSheet := h^.TabSheet;
			    DoChange(ActiveTabSheet);
			end;
			exit;
		    end;
		    lp := lp + bw;
		    h := h^.next;
		end;
	    end;
	end;
    end;
    inherited HandleMouseUp(x,y,button,shiftstate);
end;

procedure TwgTabControl.SetNavBar(aValue : TWinPosition);
begin
    {$IFDEF DEBUG}writeln('TabControl.SetNavBar');{$ENDIF}
    if aValue <> FNavBar then
    begin
	FNavBar := aValue;
    end;
end;

procedure TwgTabControl.DoShow;
begin
    {$IFDEF DEBUG}writeln('TabControl.DoShow');{$ENDIF}
    inherited DoShow;
    // TODO
end;

function TwgTabControl.TabSheetIndex(TabSheet : TwgTabSheet) : integer;
var
    h : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.TabSheetIndex');{$ENDIF}
    h := FFirstTabSheet;
    result := -1;
    if h = nil then exit;
    while h^.prev <> nil do h := h^.prev;
    while h <> nil do
    begin
	if h^.tabsheet = TabSheet then
	begin
	    result := h^.TabSheet.Index;
	    exit;
	end;
	h := h^.next;
    end;
end;

function TwgTabControl.MaxTabIndex : word;
var
    h : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.MaxTabIndex');{$ENDIF}
    result := 0;
    h := FFirstTabSheet;
    if h = nil then exit;
    while h^.prev <> nil do h := h^.prev;
    while h <> nil do
    begin
	if h^.TabSheet.Index > result then result := h^.tabsheet.index;
	h := h^.next;
    end;
end;

procedure TwgTabControl.RegisterTabSheet(TabSheet : TwgTabSheet);
var
    h : PTabSheetList;
    nl : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.RegisterTabSheet');{$ENDIF}
    if TabSheetIndex(TabSheet) > -1 then exit; // allready registered
    new(nl);
    nl^.next := nil;
    nl^.prev := nil;
    nl^.TabSheet := TabSheet;
    if FFirstTabSheet = nil then
    begin
	FFirstTabSheet := nl;
	FActiveSheet := TabSheet;
    end
    else
    begin
	h := FFirstTabSheet;
	while h^.next <> nil do h := h^.next;
	h^.next := nl;
	nl^.prev := h;
    end;
end;

procedure TwgTabControl.UnregisterTabSheet(TabSheet : TwgTabSheet);
var
    h : PTabSheetList;
    pl : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.UnregisterTabSheet');{$ENDIF}
    if TabSheetIndex(TabSheet) < 0 then exit; // doesn't exists
    h := FFirstTabSheet;
    while h^.TabSheet <> TabSheet do
    begin
	h := h^.next;
    end;
    pl := h^.prev;
    if pl = nil then
    begin
	FFirstTabSheet := h^.next;
	if (h^.next <> nil) and (TabSheet = ActiveTabSheet) then FActiveSheet := h^.next^.TabSheet;
    end
    else
    begin
	pl^.next := h^.next;
	if (TabSheet = ActiveTabSheet)  then FActiveSheet := pl^.TabSheet;
    end;
    if FFirstTabButton = h then	// resets all variables
    begin
	h := FFirstTabSheet;
    end;
    if h^.next <> nil then h^.next^.prev := pl;
    dispose(h);
end;

function TwgTabControl.AppendTabSheet(Title : string16) : TwgTabSheet;
var
    h : PTabSheetList;
    nt : TwgTabSheet;
    nl : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.AppendTabSheet');{$ENDIF}
    h := FFirstTabSheet;
    nt := TwgTabSheet.Create(self);
    nt.Text := Title;
    new(nl);
    nl^.next := nil;
    nl^.prev := nil;
    nl^.TabSheet := nt;
    if h = nil then FFirstTabSheet := nl
    else
    begin
	while h^.next <> nil do
	    h := h^.next;
	h^.next := nl;
	nl^.prev := h;
    end;
    result := nt;
end;

function TwgTabControl.TabSheetCount : integer;
var
    h : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.TabSheetCount');{$ENDIF}
    h := FFirstTabSheet;
    result := 0;
    if h = nil then exit;
    while h^.prev <> nil do h := h^.prev;
    while h^.next <> nil do
    begin
	h := h^.next;
	inc(result);
    end;
end;

procedure TwgTabControl.RePaint;
begin
    {$IFDEF DEBUG}writeln('TabControl.RePaint');{$ENDIF}
    if FWinHandle <= 0 then exit;
    inherited RePaint;
    OrderSheets;
    Canvas.ClearClipRect;
    Canvas.Clear(FBackgroundColor);
    if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0,0,Width,Height);
    RePaintTitles;
end;

procedure TwgTabControl.RePaintTitles;
var
    h : PTabSheetList;
    lp : integer;
    bh : integer; // button height
    r : tgfxrect;
begin
    {$IFDEF DEBUG}writeln('TwgTabControl.RePaintTitles');{$ENDIF}
    if FWinHandle <= 0 then exit;
    h := FFirstTabSheet;
    if h = nil then exit;
    bh := ButtonHeight;
    while h^.prev <> nil do h := h^.prev;
    Canvas.SetTextColor(clText1);
    case FNavBar of
	    wpBottom : begin
	      lp := 0;
	      if MaxButtonWidthSum > Width - FMargin * 2 then
	      begin
		      if FFirstTabButton = nil then FFirstTabButton := h
		      else h := FFirstTabButton;
		      r.SetRect(FMargin, Height - FMargin - ButtonHeight, Width - FMargin * 2 - ButtonHeight * 2, ButtonHeight);
		      FLeftButton.SetDimensions(Width - FMargin * 2 - FRightButton.Width * 2 + 1, Height - FMargin - ButtonHeight, FRightButton.Height, FRightButton.Height);
		      FRightButton.SetDimensions(Width - FMargin * 2 - FrightButton.Width + 1, Height - FMargin - ButtonHeight, FRightButton.Height, FRightButton.Height);
		      FLeftButton.Visible := true;
		      FRightButton.Visible := true;
	      end
	      else
	      begin
		      r.SetRect(FMargin, Height - FMargin - ButtonHeight, Width - FMargin * 2, ButtonHeight);
		      FLeftButton.Visible := false;
		      FRightButton.Visible := false;
	      end;
	      Canvas.SetColor(clHilite1);
	      Canvas.DrawLine(FMargin, FMargin, Width - FMargin * 2, FMargin);
	      Canvas.DrawLine(FMargin, FMargin, FMargin, Height - ButtonHeight - FMargin + 1);
	      Canvas.SetColor(clHilite2);
	      Canvas.DrawLine(FMargin, FMargin + 1, Width - FMargin * 2 - 1, FMargin + 1);
	      Canvas.DrawLine(FMargin + 1, FMargin + 2, FMargin + 1, Height - Buttonheight - FMargin);
	      Canvas.SetColor(clShadow2);
	      Canvas.DrawLine(Width - FMargin * 2, FMargin+1, Width - FMargin * 2, Height - Buttonheight - FMargin + 1);
	      Canvas.DrawLine(FMargin, Height - FMargin - ButtonHeight + 2, Width - 2 * FMargin, Height - FMargin - ButtonHeight + 2);
	      Canvas.SetColor(clShadow1);
	      Canvas.DrawLine(Width - FMargin * 2 - 1, FMargin+2, Width - FMargin * 2 - 1, Height - Buttonheight - FMargin + 1);
	      Canvas.DrawLine(FMargin+1, Height - FMargin - ButtonHeight + 1, Width - 2 * FMargin -2 , Height - FMargin - ButtonHeight + 1);
	      Canvas.SetClipRect(r);
	      while h <> nil do
	      begin
		      if h^.TabSheet <> ActiveTabSheet then
		      begin
		        h^.TabSheet.Visible := false;
		        Canvas.SetColor(clShadow1);
		        Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text), Height - ButtonHeight - FMargin + 3, lp + FMargin + ButtonWidth(h^.TabSheet.Text), Height - FMargin);
		      end
		      else
		      begin
		        h^.TabSheet.SetDimensions(FMargin + 2, FMargin + 2, Width - FMargin * 2 - 4, Height - ButtonHeight - FMargin * 2 - 1);
		        h^.TabSheet.Visible := true;
		        Canvas.SetColor(clHilite1);
		        Canvas.DrawLine(lp + FMargin, Height - ButtonHeight - FMargin + 2, lp + FMargin, Height - FMargin);
		        Canvas.SetColor(clHilite2);
		        Canvas.DrawLine(lp + FMargin + 1, Height - ButtonHeight - FMargin + 1, lp + FMargin + 1, Height - FMargin);
		        Canvas.SetColor(clShadow1);
            Canvas.DrawLine(lp + FMargin + 1, Height - FMargin - 2, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2, Height - FMargin - 2);
		        Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2, Height - ButtonHeight - Fmargin + 2, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2, Height - FMargin - 1);
		        Canvas.SetColor(clShadow2);
		        Canvas.DrawLine(lp + FMargin, Height - FMargin - 1, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 1, Height - FMargin - 1);
		        Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 1, Height - ButtonHeight - Fmargin + 3, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 1, Height - FMargin);
		        Canvas.SetColor(clWindowBackground);
		        Canvas.DrawLine(lp + FMargin + 2, Height - ButtonHeight - FMargin + 1,  lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 3, Height - ButtonHeight - FMargin + 1);
		        Canvas.DrawLine(lp + FMargin + 2, Height - ButtonHeight - FMargin + 2,  lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 3, Height - ButtonHeight - FMargin + 2);
		      end;
		      Canvas.DrawString16(lp+ButtonWidth(h^.TabSheet.Text) div 2 - FFont.TextWidth16(GetTabText(h^.TabSheet.text)) div 2,Height - FMargin - ButtonHeight + 2, GetTabText(h^.TabSheet.text));
		      lp := lp + ButtonWidth(h^.TabSheet.Text);
		      h := h^.next;
	      end;
	  end;
	  wpTop : begin // draw in top of the control
	    if MaxButtonWidthSum > Width - FMargin * 2 then
	    begin
		    if FFirstTabButton = nil then FFirstTabButton := h
		    else h := FFirstTabButton;
		    r.SetRect(FMargin,FMargin,Width - FMargin * 2 - FRightButton.Width * 2 - 1, FRightButton.Height);
		    FLeftButton.SetDimensions(Width - FMargin * 2 - FRightButton.Width * 2, FMargin, FRightButton.Height, FRightButton.Height);
		    FRightButton.SetDimensions(Width - FMargin * 2 - FrightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
		    FLeftButton.Visible := true;
		    FRightButton.Visible := true;
	    end
	    else
	    begin
		    r.SetRect(FMargin,FMargin,Width - FMargin * 2, ButtonHeight);
		    FLeftButton.Visible := false;
		    FRightButton.Visible := false;
	    end;
	    Canvas.SetColor(clHilite1);
	    Canvas.DrawLine(FMargin,ButtonHeight, FMargin, Height - FMargin * 2);
	    Canvas.SetColor(clHilite2);
	    Canvas.DrawLine(FMargin+1,ButtonHeight+1, FMargin+1, Height - FMargin * 2 - 1);
	    Canvas.SetColor(clShadow2);
	    Canvas.DrawLine(FMargin, Height - FMargin * 2, Width - FMargin * 2, Height - FMargin * 2);
	    Canvas.DrawLine(Width - FMargin - 1, FMargin + ButtonHeight - 1, Width - FMargin - 1, Height - FMargin);
	    Canvas.SetColor(clShadow1);
	    Canvas.DrawLine(FMargin + 1, Height - FMargin * 2 - 1, Width - FMargin * 2 - 1, Height - FMargin * 2 - 1);
	    Canvas.DrawLine(Width - FMargin - 2, FMargin + ButtonHeight - 1, Width - FMargin - 2, Height - FMargin - 2);
	    Canvas.SetClipRect(r);
	    lp := 0;
	    while h <> nil do
	    begin
		if h^.TabSheet <> ActiveTabSheet then
		begin
		    Canvas.SetColor(clHilite1);
		    Canvas.DrawLine(FMargin + lp, FMargin + ButtonHeight - 2, FMargin + lp + ButtonWidth(h^.TabSheet.Text), FMargin + ButtonHeight - 2);
		    Canvas.SetColor(clHilite2);
		    Canvas.DrawLine(FMargin + lp, FMargin + ButtonHeight - 1, FMargin + lp + ButtonWidth(h^.TabSheet.Text) + 1, FMargin + ButtonHeight - 1);
		    Canvas.SetColor(clShadow1);
		    Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text), FMargin, lp + FMargin + ButtonWidth(h^.TabSheet.Text), FMargin + ButtonHeight - 3);
		    h^.TabSheet.Visible := false;
		end
		else
		begin
		    h^.tabSheet.Visible := true;
		    h^.TabSheet.SetDimensions(FMargin+2, FMargin + ButtonHeight, Width - FMargin * 2 - 4, Height - FMargin * 2 - ButtonHeight - 2);
		    Canvas.SetColor(clHilite1);
		    Canvas.DrawLine(lp + FMargin, FMargin, lp + FMargin + ButtonWidth(h^.TabSheet.Text)-1, FMargin);
		    Canvas.DrawLine(lp + FMargin, FMargin, lp + FMargin, FMargin + ButtonHeight - 2);
		    Canvas.SetColor(clHilite2);
		    Canvas.DrawLine(lp + FMargin + 1, FMargin + 1, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2, FMargin + 1);
		    Canvas.DrawLine(lp + FMargin + 1, FMargin + 1, lp + FMargin + 1, FMargin + ButtonHeight - 1);
		    Canvas.SetColor(clShadow1);
		    Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2,FMargin + 1, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 2, FMargin + ButtonHeight-1);
		    Canvas.SetColor(clShadow2);
		    Canvas.DrawLine(lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 1,FMargin + 1, lp + FMargin + ButtonWidth(h^.TabSheet.Text) - 1, FMargin + ButtonHeight - 2);
		end;
		Canvas.DrawString16(lp + (ButtonWidth(h^.TabSheet.Text) div 2) - FFont.TextWidth16(GetTabText(h^.TabSheet.text)) div 2, FMargin, GetTabText(h^.TabSheet.Text));
		lp := lp + ButtonWidth(h^.TabSheet.Text);
		h := h^.next;
	    end;
	    Canvas.SetColor(clHilite1);
	    Canvas.Drawline(lp + 1,FMargin + bh - 2, width, FMargin + bh - 2);
	    Canvas.SetColor(clHilite2);
	    Canvas.Drawline(lp + 1,FMargin + bh - 1, width, FMargin + bh - 1);
	end;
    end;
end;

procedure TwgTabControl.SetActiveSheet(aValue : TwgTabSheet);
begin
    {$IFDEF DEBUG}writeln('TabControl.SetActiveSheet');{$ENDIF}
    if aValue <> FActiveSheet then
    begin
	FActiveSheet := aValue;
	ActiveWidget := FActiveSheet;
	RePaint;
    end;
end;

constructor TwgTabControl.Create(aOwner : TComponent);
begin
    {$IFDEF DEBUG}writeln('TabControl.Create');{$ENDIF}
    inherited Create(aOwner);
    FFirstTabSheet := nil;
    Focusable := true;
    FBackgroundColor := clWindowBackground;
    FNavBar := wpTop;
    FMargin := 1;
    FFixedTabWidth := 0;
    FFont := guistyle.LabelFont1;
    FFirstTabButton := nil;
    FLeftButton := TwgButton.Create(self);
    FLeftButton.Text := Str8To16('<');
    FLeftButton.Width := FLeftButton.Height;
    FLeftButton.Visible := false;
    FLeftButton.OnClick := {$ifdef fpc}@{$endif}LeftButtonClick;
    FRightButton := TwgButton.Create(self);
    FRightButton.text := Str8To16('>');
    FRightButton.Width := FRightButton.Height;
    FRightButton.OnClick := {$ifdef fpc}@{$endif}RightButtonClick;
    FRightButton.Visible := false;
end;

function TwgTabControl.MaxButtonWidthSum : integer;
var
    h : PTabSheetList;
begin
    {$IFDEF DEBUG}writeln('TabControl.MaxButtonWidthSum');{$ENDIF}
    h := FFirstTabSheet;
    result := 0;
    if h = nil then exit;
    while h^.prev <> nil do h := h^.prev;
    while h <> nil do
    begin
	result := result + ButtonWidth(h^.TabSheet.Text); // little space
	h := h^.next;
    end;
end;

function TwgTabControl.ButtonHeight : integer;
begin
    result := FRightButton.Height;
end;

end.
