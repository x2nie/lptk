unit gfxmenu;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

uses Classes, SysUtils, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  popupwindow;
  
type
  THotKeyDef = string[16];

  TMenuItem = class(TComponent)
  public
    Text : string16;
    HotKeyDef8 : THotKeyDef;

    Handler : TNotifyEvent;

    Separator : boolean;

    Visible : boolean;
    Enabled : boolean;

    constructor Create(AOwner : TComponent); override;
    
    procedure Click;
    
    function Selectable : boolean;
    
    function GetAccelChar : string16;
  end;

  TPopupMenu = class(TPopupWindow)
  private
    FMargin : TGfxCoord;
    FTextMargin : TGfxCoord;
  protected
    FMenuFont   : TGfxFont;
    FMenuAccelFont  : TGfxFont;
    FMenuDisabledFont : TGfxFont;
    
    FSymbolWidth : integer;

    FItems : TList;
    
    FFocusItem : integer;

    function VisibleCount : integer;
    function VisibleItem(ind : integer) : TMenuItem;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure PrepareToShow;

    procedure ShowAt(wh : TWinHandle; x,y : integer); virtual;

    function ItemHeight(mi : TMenuItem) : integer;
    procedure DrawItem(mi : TMenuItem; rect : TGfxRect);
    procedure DrawRow(line : integer; focus : boolean);

    procedure Repaint; override;
    
    function CalcMouseRow(y : integer) : integer;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    
    procedure DoSelect;
    
    function SearchItemByAccel(s : string16) : integer;
    
  public

    function AddMenuItem8(const menuname8 : string; const hotkeydef8 : string; HandlerProc : TNotifyEvent) : TMenuItem;

  public

    BeforeShow : TNotifyEvent;

  end;


implementation

{ TMenuItem }

constructor TMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := '';
  HotKeyDef8 := '';
  Separator := false;
  Visible := true;
  Enabled := true;
  Handler := nil;
end;

procedure TMenuItem.Click;
begin
  //Writeln('MenuItem click: ',u16u8(Text));
  if Assigned(Handler) then Handler(self);
end;

function TMenuItem.Selectable: boolean;
begin
  result := Enabled and Visible and NOT Separator;
end;

function TMenuItem.GetAccelChar: string16;
var
  p : integer;
begin
  p := pos16(u8('&'),Text);
  if p > 0 then
  begin
    result := copy16(Text,p+1,1);
  end
  else result := '';
end;

{ TPopupMenu }

function TPopupMenu.VisibleCount: integer;
begin
  result := FItems.Count;
end;

function TPopupMenu.VisibleItem(ind: integer): TMenuItem;
begin
  if (ind < 1) or (ind > FItems.Count)
    then result := nil
    else result := TMenuItem(FItems.Items[ind-1]);
end;

constructor TPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMargin := 2;
  FTextMargin := 3;

  FItems := TList.Create;

  FMenuFont := guistyle.MenuFont;
  FMenuAccelFont := guistyle.MenuAccelFont;
  FMenuDisabledFont := guistyle.MenuDisabledFont;
  
  FSymbolWidth := FMenuFont.Height+2;

  BeforeShow := nil;
  FFocusItem := 1;
end;

destructor TPopupMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TPopupMenu.PrepareToShow;
var
  n,h,tw,hkw,x : integer;
  mi : TMenuItem;
begin
  if Assigned(BeforeShow) then BeforeShow(self);

  // Collecting visible items
  FItems.Count := 0;

  for n:=0 to ComponentCount-1 do
  begin
    if Components[n] is TMenuItem then
    begin
      mi := TMenuItem(Components[n]);

      if mi.Visible then FItems.Add(mi);
    end;
  end;

  // Measuring sizes

  h := 0; tw := 0; hkw := 0;
  FSymbolWidth := 0;

  for n:=1 to VisibleCount do
  begin
    mi := VisibleItem(n);
    x := ItemHeight(mi);
    inc(h,x);
    x := FMenuFont.TextWidth16(mi.Text);
    if tw < x then tw := x;
    x := FMenuFont.TextWidth16(u8(mi.HotKeyDef8));
    if hkw < x then hkw := x;
  end;
  
  if hkw > 0 then hkw := hkw + 5;
  
  FHeight := FMargin*2 +1 + h;  // +1=the shadow
  FWidth := (FMargin+FTextMargin)*2 +1 + FSymbolWidth + tw + hkw;

end;

function TPopupMenu.AddMenuItem8(const menuname8: string;
  const hotkeydef8: string; HandlerProc: TNotifyEvent): TMenuItem;
begin
  result := TMenuItem.Create(self);
  if menuname8 <> '-' then
  begin
    result.Text := u8(menuname8);
    result.hotkeydef8 := hotkeydef8;
    result.Handler := HandlerProc;
  end
  else
  begin
    result.Separator := true;
  end;
end;

function TPopupMenu.ItemHeight(mi: TMenuItem): integer;
begin
  if mi.Separator then result := 5
  else result := FMenuFont.Height + 2;
end;

procedure TPopupMenu.Repaint;
var
  n : integer;
  mi : TMenuItem;
begin
  inherited Repaint;

  Canvas.Clear(FBackgroundColor);
  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0,0,width-1,height-1);
  Canvas.DrawLine(2,Height-1,width-1,Height-1);
  Canvas.DrawLine(Width-1,2,Width-1,Height-1);

  for n:=1 to VisibleCount do
  begin
    DrawRow(n,n = FFocusItem);
  end;
end;

function TPopupMenu.CalcMouseRow(y: integer): integer;
var
  h : integer;
  n : integer;
begin
  result := 1;
  h := 2;
  n := 1;
  while (h <= y) and (n <= VisibleCount) do
  begin
    result := n;
    inc(h, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TPopupMenu.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  newf : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  
  newf := CalcMouseRow(y);
  
  if NOT VisibleItem(newf).Selectable then Exit;

  if newf = FFocusItem then Exit;
  
  DrawRow(FFocusItem,false);
  FFocusItem := newf;
  DrawRow(FFocusItem,true);
end;

procedure TPopupMenu.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
var
  newf : integer;
begin
  inherited HandleMouseUp(x, y, button, shiftstate);

  newf := CalcMouseRow(y);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf <> FFocusItem then
  begin
    DrawRow(FFocusItem,false);
    FFocusItem := newf;
    DrawRow(FFocusItem,true);
  end;
  
  if button <> 1 then Exit;
  
  DoSelect;
  
end;

procedure TPopupMenu.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  oldf : integer;
  i : integer;
  s : string16;
  
  procedure FollowFocus;
  begin
    if oldf <> FFocusItem then
    begin
      DrawRow(oldf,false);
      DrawRow(FFocusItem,true);
    end;
  end;
  
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  oldf := FFocusItem;

  consumed := true;
  case keycode of
    KEY_UP:
           begin // up
             i := FFocusItem-1;
             while (i >= 1) and not VisibleItem(i).Selectable do dec(i);
             
             if i >= 1 then FFocusItem := i;
           end;
    KEY_DOWN:
           begin // down
             if FFocusItem < VisibleCount then
             begin
               i := FFocusItem+1;
               while (i <= VisibleCount) and not VisibleItem(i).Selectable do inc(i);

               if i <= VisibleCount then FFocusItem := i;
             end;
           end;
    KEY_ENTER:
           begin
             DoSelect;
           end;
    KEY_ESC:
           begin
             Close;
           end;
  else
    consumed := false;
  end;
  
  FollowFocus;
  
  if ((keycode and $8000) <> $8000) then
  begin
    // normal char
    s := chr(keycode and $00FF) + chr((keycode and $FF00) shr 8);
    i := SearchItemByAccel(s);
    if i > 0 then
    begin
      FFocusItem := i;
      FollowFocus;
      
      Consumed := true;
      
      DoSelect;
    end;
  end;

end;

procedure TPopupMenu.DoSelect;
begin
  // Close this popup
  Close;

  VisibleItem(FFocusItem).Click;
end;

function TPopupMenu.SearchItemByAccel(s: string16): integer;
var
  n : integer;
begin
  result := -1;
  for n:=1 to VisibleCount do
  begin
    with VisibleItem(n) do
    begin
      if Enabled and (UpCase16(s) = UpCase16(GetAccelChar)) then
      begin
        result := n;
        Exit;
      end;
    end;
  end;
end;

procedure TPopupMenu.DrawItem(mi : TMenuItem; rect: TGfxRect);
var
  s16 : string;
  x : integer;
  p : integer;
  achar, ch : string16;
begin
  if mi.Separator then
  begin
    Canvas.SetColor(clMenuText);
    Canvas.DrawLine(rect.Left, rect.Top+2, rect.Right, rect.Top+2);
  end
  else
  begin
    if not mi.Enabled
      then Canvas.SetFont(FMenuDisabledFont)
      else Canvas.SetFont(FMenuFont);
    
    x := rect.Left + FSymbolWidth + FTextMargin;
    achar := u8('&');
    
    s16 := mi.Text;
    
    repeat
       p := Pos16(achar, s16);
       if p > 0 then
       begin
         Canvas.DrawString16(x, rect.Top, copy16(s16,1,p-1));
         inc(x, FMenuFont.TextWidth16(copy16(s16,1,p-1)) );
         if copy16(s16,p+1,1) = achar then
         begin
           Canvas.DrawString16(x,rect.Top,achar);
           inc(x, FMenuFont.TextWidth16(achar) );
         end
         else
         begin
           if mi.Enabled then Canvas.SetFont(FMenuAccelFont);
           //Canvas.SetTextColor(clMenuAccel);
           Canvas.DrawString16(x,rect.Top,copy16(s16,p+1,1));
           inc(x, Canvas.Font.TextWidth16(copy16(s16,p+1,1)) );
           if mi.Enabled then Canvas.SetFont(FMenuFont);
         end;
         s16 := copy16(s16,p+2,length16(s16));
       end;
    until p < 1;
    
    if Length16(s16) > 0 then Canvas.DrawString16(x, rect.Top, s16);

    if mi.HotKeyDef8 <> '' then
    begin
      s16 := u8(mi.HotKeyDef8);
      Canvas.DrawString16(rect.Right-FMenuFont.TextWidth16(s16)-FTextMargin, rect.Top, s16);
    end;
  end;
end;

procedure TPopupMenu.DrawRow(line: integer; focus: boolean);
var
  h : integer;
  n : integer;
  r : TGfxRect;
  mi : TMenuItem;
begin
  r.SetRect(FMargin,FMargin, FWidth-2*FMargin-1, FHeight-2*FMargin-1);

  for n:=1 to VisibleCount do
  begin
    mi := VisibleItem(n);

    r.height := ItemHeight(mi);
    
    if line = n then
    begin
      if focus and (not mi.Separator) then
      begin
        canvas.SetColor(clSelection);
        canvas.SetTextColor(clSelectionText);
      end
      else
      begin
        if mi.Enabled then
        begin
          canvas.SetColor(BackgroundColor);
          canvas.SetTextColor(clMenuText);
        end
        else
        begin
          canvas.SetColor(BackgroundColor);
          canvas.SetTextColor(clMenuDisabled);
        end;
      end;
      canvas.FillRect(r);

      DrawItem(mi,r);
      
      EXIT;
    end;

    inc(r.Top, ItemHeight(mi) );
  end;
  
end;

procedure TPopupMenu.ShowAt(wh: TWinHandle; x, y: integer);
begin
  PrepareToShow;
  inherited;
end;

end.

