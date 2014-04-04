{ wglistbox.pas: Listbox widget by nvitya@freemail.hu
History:
    13.01.2004	Buffer-support added (Erik Grohnwaldt)
    10.09.2012  Translated to PasGF by Preben Bjorn Biermann Madsen
}

unit lp_listbox;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_utils, lp_Widget, lp_ScrollBar;

type

  { TlpListBox }

  TlpListBox = class(TpgfWidget)
  private
    FPopupFrame: boolean;
    function GetFontName: string;
    procedure SetFocusItem(const AValue: integer);
    procedure SetFontName(const AValue: string);
    procedure SetPopupFrame(const AValue: boolean);
    procedure SetBackgroundColor(color : TpgfColor);
  protected
    FFont : TpgfFont;
    FBackgroundColor: TpgfColor;
    FScrollBar : TwgScrollBar;
    FFocusItem : integer;
    FMouseDragging : boolean;
    FFirstItem : integer;
    FMargin    : integer;

    procedure SetFirstItem(item : integer);
    procedure UpdateScrollBar;
    procedure FollowFocus;
    function ListHeight : TpgfCoord;
    function ScrollBarWidth : TpgfCoord;
    function PageLength : integer;
    procedure ScrollBarMove(Sender: TObject; position : integer);
    procedure DoChange;
    procedure DoSelect;
  public
    HotTrack : boolean;
    OnChange : TNotifyEvent;
    OnSelect : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure HandleShow; override;
    procedure HandlePaint; override;
    procedure Update;
    function ItemCount : integer; virtual;
    function RowHeight : integer; virtual;
    procedure DrawItem(num : integer; rect : TpgfRect; flags : integer); virtual;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleLMouseDown(x,y : integer; shiftstate : word); override;
    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleRMouseDown(x,y : integer; shiftstate : word); override;
    procedure HandleRMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleWindowScroll(direction, amount : integer); //* override;
    procedure HandleResize(dwidth, dheight : integer); override;
    property PopupFrame : boolean read FPopupFrame write SetPopupFrame;
    property Font : TpgfFont read FFont;
    property FocusItem : integer read FFocusItem write SetFocusItem;
  published
    property FontName : string read GetFontName write SetFontName;
    property BackgroundColor : TpgfColor read FBackgroundColor write SetBackgroundColor;
  end;

  TwgTextListBox = class(TlpListBox)
  protected
    FItems : TStringList;
    FInternalItems : TStringList;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetItemRef(itemref : TStringList);
    function ItemCount : integer; override;
    procedure DrawItem(num : integer; rect : TpgfRect; flags : integer); override;
    function Text : WideString;
    function Text8 : string;
  published
      property Items : TStringList read FItems;
  end;

implementation


{ TwgScrollBar }

procedure TlpListBox.SetFocusItem(const AValue: integer);
begin
  if FFocusItem=AValue then exit;
  FFocusItem:=AValue;
  FollowFocus;
  UpdateScrollbar;
  if FWinHandle > 0 then RePaint;
end;

function TlpListBox.GetFontName: string;
begin
  result := FFont.FontDesc;
end;

procedure TlpListBox.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := PgfGetFont(AValue);
  RePaint;
end;

procedure TlpListBox.SetPopupFrame(const AValue: boolean);
begin
  if FPopupFrame=AValue then exit;
  FPopupFrame:=AValue;
  Repaint;
end;

procedure TlpListBox.SetBackgroundColor(color : TpgfColor);
begin
  FBackgroundColor := color;
  RePaint;
end;

procedure TlpListBox.HandleShow;
begin
  FScrollBar.SetPosition(width-18,0,18,height);
  inherited HandleShow;
  UpdateScrollBar;
end;

procedure TlpListBox.SetFirstItem(item: integer);
begin
  FFirstItem := item;
  UpdateScrollBar;
end;

procedure TlpListBox.UpdateScrollBar;
var
  pn : integer;
begin
  pn := PageLength;
  FScrollBar.Visible := PageLength < ItemCount;

  if FScrollBar.Visible then
  begin
    FScrollBar.Min := 1;
    if ItemCount <> 0 then FScrollBar.SliderSize := pn / ItemCount else FScrollBar.SliderSize := 1;
    FScrollBar.Max := ItemCount-pn+1;
    FScrollBar.Position := FFirstItem;
//* ??  if FScrollBar.WinHandle > 0 then 
    FScrollBar.RePaint;
  end;
end;

procedure TlpListBox.ScrollBarMove(Sender: TObject; position: integer);
begin
  FFirstItem := position;
  Repaint;
end;

procedure TlpListBox.DoChange;
begin
  if assigned(OnChange) then OnChange(self);
end;

procedure TlpListBox.DoSelect;
begin
  if assigned(OnSelect) then OnSelect(self);
end;

procedure TlpListBox.FollowFocus;
var
  n : integer;
  h : TpgfCoord;
begin
  if FFocusItem < FFirstItem then
  begin
    FFirstItem := FFocusItem;
    UpdateScrollBar;
  end
  else
  begin
    h := 0;
    for n := FFocusItem downto FFirstItem do
    begin
      h := h + RowHeight;
      if h > ListHeight then
      begin
        FFirstItem := n+1;
        UpdateScrollBar;
        break;
      end;
    end;
  end;
end;

function TlpListBox.ListHeight: TpgfCoord;
begin
  result := height - (2*FMargin);
end;

function TlpListBox.ScrollBarWidth: TpgfCoord;
begin
  if FScrollBar.Visible then result := FScrollBar.Width else result := 0;
end;

function TlpListBox.PageLength: integer;
begin
  result := trunc(ListHeight / RowHeight)
end;

constructor TlpListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := PgfGetFont('#List');
  FBackgroundColor := clListBox;
  FScrollBar := TwgScrollBar.Create(self);
  FScrollBar.OnScroll := {$ifdef FIXME}@{$endif}ScrollBarMove;
  FFocusable := true;
  FFocusItem := 1;
  FFirstItem := 1;
  FWidth := 80;
  FHeight := 80;
  FMargin := 2;
  FMouseDragging := false;
  FPopupFrame := false;
  HotTrack := false;
  OnChange := nil;
  OnSelect := nil;
end;

destructor TlpListBox.Destroy;
begin
  FScrollBar.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TlpListBox.HandlePaint;
var
  n : integer;
  r : TpgfRect;
begin
  //inherited RePaint;
  if not HasHandle then Exit;

  Canvas.BeginDraw;  
  Canvas.ClearClipRect;
  
  if popupframe then
  begin
    canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(0,0,width,height);
    r.SetRect(1,1, width - 2, height - 2);
  end
  else
  begin
    Canvas.DrawControlFrame(0,0,width,height);
    r.SetRect(2,2, width - 4, height - 4);
  end;

  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRect(r);
  Canvas.SetFont(FFont);
  r.SetRect(FMargin,FMargin, width-ScrollBarWidth-FMargin-2, height-(2*FMargin));
  Canvas.SetClipRect(r);
  r.Height := RowHeight;

  for n:=FFirstItem to ItemCount do
  begin
    if n = FFocusItem then
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
    DrawItem(n,r,0);
    r.Top := r.Top + r.Height;
    if r.Top >= self.Height then break;
  end;

  // clearing after the last row
  if r.Top <= Height then
  begin
    canvas.SetColor(FBackgroundColor);
    r.SetBottom(Height - fmargin);
    canvas.FillRect(r);
  end;
  Canvas.EndDraw;
end;

procedure TlpListBox.Update;
begin
  FFirstItem := 1;
  FFocusItem := 1;
  UpdateScrollBar;
  Repaint;
end;

function TlpListBox.ItemCount: integer;
begin
  result := 17;
end;

function TlpListBox.RowHeight: integer;
begin
  result := FFont.Height+2;
end;

procedure TlpListBox.DrawItem(num: integer; rect: TpgfRect; flags: integer);
var
  s : WideString;
begin
  s := u8('Item'+IntToStr(num));
  Canvas.DrawString(rect.left+4, rect.top+1,s);
end;

procedure TlpListBox.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  consumed := true;
  case keycode of
    KEY_UP:
           begin // up
             if FFocusItem > 1 then
             begin
               dec(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;
    KEY_DOWN:
           begin // down
             if FFocusItem < ItemCount then
             begin
               inc(FFocusItem);
               FollowFocus;
               RePaint;
               DoChange;
             end;
           end;
    KEY_PGUP:
           begin // pgup
             dec(FFocusItem,PageLength);
             if FFocusItem < 1 then FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_PGDN:
           begin // pgdown
             inc(FFocusItem,PageLength);
             if FFocusItem > ItemCount then FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_HOME:
           begin // home
             FFocusItem := 1;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_END:
           begin // end
             FFocusItem := ItemCount;
             FollowFocus;
             RePaint;
             DoChange;
           end;
    KEY_ENTER:
           begin // enter
             DoSelect;
             consumed := false; // to allow the forms to detect it
           end;
  else
    consumed := false;
  end;

end;

procedure TlpListBox.HandleLMouseDown(x, y: integer; shiftstate: word);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if ItemCount < 1 then Exit;

  FFocusItem := FFirstItem + trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then FFocusItem := ItemCount;
  FollowFocus;
  FMouseDragging := true;
  repaint;
  DoChange;
//  DoSelect;
end;

procedure TlpListBox.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  if ItemCount < 1 then Exit;
  FMouseDragging := False;
  FFocusItem := FFirstItem + trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then FFocusItem := ItemCount;
  FollowFocus;
  repaint;
  DoChange;
  DoSelect;
end;
procedure TlpListBox.HandleRMouseDown(x, y: integer; shiftstate: word);
begin
  inherited HandleRMouseDown(x, y, shiftstate);

  if ItemCount < 1 then Exit;
  FFocusItem := FFirstItem + trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then FFocusItem := ItemCount;
  FollowFocus;
  FMouseDragging := true;
  repaint;
  DoChange;
//  DoSelect;
end;

procedure TlpListBox.HandleRMouseUp(x, y: integer; shiftstate: word);
begin
  inherited HandleRMouseUp(x, y, shiftstate);

  if ItemCount < 1 then Exit;
  FMouseDragging := False;
  FFocusItem := FFirstItem + trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then FFocusItem := ItemCount;
  FollowFocus;
  repaint;
  DoChange;
  DoSelect;
end;

procedure TlpListBox.HandleMouseMove(x,y : integer; btnstate, shiftstate : word);
var
  oldf : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if ItemCount < 1 then Exit;
  if ((not FMouseDragging) or (btnstate and 1 = 0)) and (not HotTrack) then Exit;
  oldf := FFocusItem;
  FFocusItem := FFirstItem + trunc((y - FMargin) / RowHeight);
  if FFocusItem > ItemCount then FFocusItem := ItemCount;
  if oldf <> FFocusItem then
  begin
    FollowFocus;
    repaint;
  end;
end;

procedure TlpListBox.HandleWindowScroll(direction, amount: integer);
var
  pfi : integer;
begin
  pfi := FFirstItem;
  if direction = 0 then
  begin
    FFirstItem := FFirstItem - amount;
  end
  else if direction = 1 then
  begin
    FFirstItem := FFirstItem + amount;
  end;
  if FFirstItem + PageLength > ItemCount then FFirstItem := ItemCount - PageLength + 1;
  if FFirstItem < 1 then FFirstItem := 1;
  if pfi <> FFirstItem then
  begin
    UpdateScrollBar;
    Repaint;
  end;
end;

procedure TlpListBox.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);

  FScrollBar.Height := Height;
  FScrollBar.left := Width - FScrollBar.width;
  FScrollBar.UpdateWindowPosition;
  UpdateScrollBar;
end;

{ TwgTextListBox }

constructor TwgTextListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalItems := nil;
  SetItemRef(nil);
end;

destructor TwgTextListBox.Destroy;
begin
  if FInternalItems <> nil then FInternalItems.Free;
  inherited Destroy;
end;

procedure TwgTextListBox.SetItemRef(itemref: TStringList);
begin
  if itemref = nil then
  begin
    if FinternalItems <> nil then FInternalItems.Free;
    FInternalItems := TStringList.Create;
    FItems := FInternalItems;
  end
  else FItems := itemref;
end;

function TwgTextListBox.ItemCount: integer;
begin
  result := FItems.Count;
end;

procedure TwgTextListBox.DrawItem(num: integer; rect: TpgfRect; flags: integer);
begin
  canvas.DrawString(rect.left+2,rect.top+1,FItems.Strings[num-1]);
end;

function TwgTextListBox.Text: WideString;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count)
    then result := FItems.Strings[FocusItem-1]
    else result := '';
end;

function TwgTextListBox.Text8: string;
begin
  result := wstoutf8(Text);
end;

end.

