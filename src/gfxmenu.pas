unit gfxmenu;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

uses Classes, SysUtils, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  popupwindow, gfxwidget, messagequeue;

type
  THotKeyDef = string[16];

  TPopupMenu = class;

  TMenuItem = class(TComponent)
  public
    Text : string16;
    HotKeyDef8 : THotKeyDef;

    Handler : TNotifyEvent;

    Separator : boolean;

    Visible : boolean;
    Enabled : boolean;

    SubMenu : TPopupMenu;

    constructor Create(AOwner : TComponent); override;

    procedure Click;

    function Selectable : boolean;

    function GetAccelChar : string16;

    procedure DrawText(canvas : TGfxCanvas; x,y : TGfxCoord);

    property OnClick : TNotifyEvent read Handler write Handler;
  end;

  TMenuBar = class;

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

    //procedure MsgClose(var msg : TMessageRec); message MSG_CLOSE;

  public
    OpenerPopup : TPopupMenu;
    OpenerMenuBar : TMenuBar;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure PrepareToShow; // measuring width, height

    procedure ShowAt(wh : TWinHandle; x,y : integer); virtual;

    function ItemHeight(mi : TMenuItem) : integer;
    procedure DrawItem(mi : TMenuItem; rect : TGfxRect);
    procedure DrawRow(line : integer; focus : boolean);

    procedure Repaint; override;

    function CalcMouseRow(y : integer) : integer;
    function GetItemPosY(index : integer) : integer;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure DoSelect;
    procedure Close; override;

    function MenuFocused : boolean;

    procedure CloseSubmenus;

    function SearchItemByAccel(s : string16) : integer;

  public

    function AddMenuItem8(const menuname8 : string; const hotkeydef8 : string; HandlerProc : TNotifyEvent) : TMenuItem;

  public

    BeforeShow : TNotifyEvent;

  end;

  TMenuBar = class(TWidget)
  protected
    FItems : TList;
    FFocusItem : integer;

    procedure PrepareToShow; // measuring width, height

    function VisibleCount : integer;
    function VisibleItem(ind : integer) : TMenuItem;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DoShow; override;

    function ItemWidth(mi : TMenuItem) : integer;

    procedure Repaint; override;
    procedure DrawColumn(col : integer; focus : boolean);

    function CalcMouseCol(x : integer) : integer;
    function GetItemPosX(index : integer) : integer;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure DoSelect;
    procedure CloseSubmenus;

    function MenuFocused : boolean;

    function SearchItemByAccel(s : string16) : integer;

    procedure DeActivateMenu;
    procedure ActivateMenu;

  public

    function AddMenuItem8(const menuname8 : string; HandlerProc : TNotifyEvent) : TMenuItem;

  public

    BeforeShow : TNotifyEvent;
  end;


implementation

var
  FocusedPopupMenu : TPopupMenu;

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
  SubMenu := nil;
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

procedure TMenuItem.DrawText(canvas: TGfxCanvas; x,y : TGfxCoord);
var
  s16 : string;
  p : integer;
  achar : string16;
begin
  if not Enabled
    then Canvas.SetFont(guistyle.MenuDisabledFont)
    else Canvas.SetFont(guistyle.MenuFont);

  achar := u8('&');

  s16 := Text;

  repeat
     p := Pos16(achar, s16);
     if p > 0 then
     begin
       Canvas.DrawString16(x, y, copy16(s16,1,p-1));
       inc(x, guistyle.MenuFont.TextWidth16(copy16(s16,1,p-1)) );
       if copy16(s16,p+1,1) = achar then
       begin
         Canvas.DrawString16(x,y,achar);
         inc(x, guistyle.MenuFont.TextWidth16(achar) );
       end
       else
       begin
         if Enabled then Canvas.SetFont(guistyle.MenuAccelFont);
         //Canvas.SetTextColor(clMenuAccel);
         Canvas.DrawString16(x,y,copy16(s16,p+1,1));
         inc(x, Canvas.Font.TextWidth16(copy16(s16,p+1,1)) );
         if Enabled then Canvas.SetFont(guistyle.MenuFont);
       end;
       s16 := copy16(s16,p+2,length16(s16));
     end;
  until p < 1;

  if Length16(s16) > 0 then Canvas.DrawString16(x, y, s16);
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
  OpenerPopup := nil;
  OpenerMenubar := nil;
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

    if mi.SubMenu <> nil then x := FMenuFont.Height
                         else x := FMenuFont.TextWidth16(u8(mi.HotKeyDef8));
    if hkw < x then hkw := x;
  end;

  if hkw > 0 then hkw := hkw + 5;

  FHeight := FMargin*2 +1 + h;  // +1=the shadow
  FWidth := (FMargin+FTextMargin)*2 +1 + FSymbolWidth + tw + hkw;

  FocusedPopupMenu := self;
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

  if not MenuFocused then Exit;

  newf := CalcMouseRow(y);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf = FFocusItem then Exit;

  DrawRow(FFocusItem,false);
  FFocusItem := newf;
  DrawRow(FFocusItem,true);
end;

procedure TPopupMenu.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
var
  newf : integer;
  mi : TMenuItem;
begin
  inherited HandleMouseDown(x, y, button, shiftstate);

  newf := CalcMouseRow(y);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf <> FFocusItem then
  begin
    DrawRow(FFocusItem,false);
    FFocusItem := newf;
    DrawRow(FFocusItem,true);
  end;

  if button <> 1 then Exit;

  mi := VisibleItem(FFocusItem);
  if (mi <> nil) and (not MenuFocused) and (mi.SubMenu <> nil) and mi.SubMenu.Windowed
    then mi.SubMenu.Close
    else DoSelect;

end;

procedure TPopupMenu.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  oldf : integer;
  i : integer;
  s : string16;
  op : TPopupMenu;
  trycnt : integer;

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
             trycnt := 2;
             i := FFocusItem-1;
             repeat
               while (i >= 1) and not VisibleItem(i).Selectable do dec(i);

               if i >= 1 then break;

               i := VisibleCount;
               dec(trycnt);
             until trycnt > 0;

             if i >= 1 then FFocusItem := i;
           end;
    KEY_DOWN:
           begin // down

             trycnt := 2;
             i := FFocusItem+1;
             repeat
               while (i <= VisibleCount) and not VisibleItem(i).Selectable do inc(i);

               if i <= VisibleCount then break;

               i := 1;
               dec(trycnt);
             until trycnt > 0;

             if i <= VisibleCount then FFocusItem := i;

           end;
    KEY_ENTER:
           begin
             DoSelect;
           end;

    KEY_LEFT:
           begin
             if OpenerMenubar <> nil then OpenerMenubar.HandleKeyPress(keycode, shiftstate, consumed);
           end;

    KEY_RIGHT:
           begin
             if OpenerMenubar <> nil then OpenerMenubar.HandleKeyPress(keycode, shiftstate, consumed);
             // VisibleItem(FFocusItem).SubMenu <> nil then DoSelect;
           end;

    KEY_BACKSPACE:
           begin
             //if self.OpenerPopup <> nil then
             Close;
           end;

    KEY_ESC:
           begin
             Close;
             op := OpenerPopup;
             while op <> nil do
             begin
               op.Close;
               op := op.OpenerPopup;
             end;
           end;
  else
    consumed := false;
  end;

  FollowFocus;

  if (not consumed) and ((keycode and $8000) <> $8000) then
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
var
  mi : TMenuItem;
  op : TPopupMenu;
begin
  mi := VisibleItem(FFocusItem);

  if mi.SubMenu <> nil then
  begin
    CloseSubMenus;

    // showing the submenu
    mi.SubMenu.ShowAt(self.WinHandle,Width,GetItemPosY(FFocusItem));
    mi.SubMenu.OpenerPopup := self;
    mi.SubMenu.OpenerMenuBar := self.OpenerMenuBar;

    FocusedPopupMenu := mi.SubMenu;
    self.Repaint;
  end
  else
  begin
    // Close this popup
    Close;

    op := OpenerPopup;
    while op <> nil do
    begin
      if op.Windowed then op.Close;
      op := op.OpenerPopup;
    end;

    VisibleItem(FFocusItem).Click;
  end;

  if OpenerMenuBar <> nil then OpenerMenuBar.DeActivateMenu;
end;

procedure TPopupMenu.Close;
var
  n : integer;
  mi : TMenuItem;
begin
  for n:=0 to FItems.Count-1 do
  begin
    mi := TMenuItem(FItems[n]);
    if mi.SubMenu <> nil then
    begin
      if mi.SubMenu.Windowed then mi.SubMenu.Close;
    end;
  end;
  inherited Close;

  FocusedPopupMenu := OpenerPopup;
  if (FocusedPopupMenu <> nil) and FocusedPopupMenu.Windowed then FocusedPopupMenu.Repaint;

  if (OpenerMenuBar <> nil) and OpenerMenuBar.Windowed then
  begin
    if (OpenerPopup = nil) or not OpenerPopup.Windowed then
    begin
      OpenerMenuBar.DeActivateMenu;
      //OpenerMenuBar.Repaint;
    end;
    //else
    //OpenerMenuBar.Repaint;
  end;  
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
begin
  if mi.Separator then
  begin
    Canvas.SetColor(clMenuText);
    Canvas.DrawLine(rect.Left, rect.Top+2, rect.Right, rect.Top+2);
  end
  else
  begin
    x := rect.Left + FSymbolWidth + FTextMargin;

    mi.DrawText(Canvas,x,rect.top);

    if mi.HotKeyDef8 <> '' then
    begin
      s16 := u8(mi.HotKeyDef8);
      Canvas.DrawString16(rect.Right-FMenuFont.TextWidth16(s16)-FTextMargin, rect.Top, s16);
    end;

    if mi.SubMenu <> nil then
    begin
      canvas.SetColor(canvas.TextColor);

      x := (rect.height div 2) - 3;

      canvas.FillTriangle(rect.right-x-2,rect.top+2,
                          rect.right-2,rect.top+2+x,
                          rect.right-x-2,rect.top+2+2*x);
    end;
  end;
end;

procedure TPopupMenu.DrawRow(line: integer; focus: boolean);
var
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
        if MenuFocused then
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

function TPopupMenu.GetItemPosY(index: integer): integer;
var
  n : integer;
begin
  result := 2;
  if index < 1 then Exit;
  n := 1;
  while (n <= VisibleCount) and (n < index) do
  begin
    Inc(result, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

function TPopupMenu.MenuFocused: boolean;
begin
  result := (FocusedPopupMenu = self);
end;

procedure TPopupMenu.CloseSubmenus;
var
  n : integer;
begin
  // Close all previous popups
  for n := 1 to VisibleCount do
  with VisibleItem(n) do
  begin
    if (SubMenu <> nil) and (SubMenu.Windowed) then SubMenu.Close;
  end;
end;

{
procedure TPopupMenu.MsgClose(var msg: TMessageRec);
begin
  Writeln('popup closed.');
end;
}

{ TMenuBar }

function TMenuBar.AddMenuItem8(const menuname8: string; HandlerProc: TNotifyEvent): TMenuItem;
begin
  result := TMenuItem.Create(self);
  result.Text := u8(menuname8);
  result.hotkeydef8 := '';
  result.Handler := HandlerProc;
  result.Separator := false;
end;

function TMenuBar.CalcMouseCol(x: integer): integer;
var
  w : integer;
  n : integer;
begin
  result := 1;
  w := 0;
  n := 1;
  while (w <= x) and (n <= VisibleCount) do
  begin
    result := n;
    inc(w, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TMenuBar.CloseSubmenus;
var
  n : integer;
begin
  // Close all previous popups
  for n := 1 to VisibleCount do
  with VisibleItem(n) do
  begin
    if (SubMenu <> nil) and (SubMenu.Windowed) then SubMenu.Close;
  end;
end;

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList.Create;
  BeforeShow := nil;
  FFocusItem := 1;
  FFocusable := false
end;

destructor TMenuBar.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMenuBar.DoSelect;
var
  mi : TMenuItem;
begin
  mi := VisibleItem(FFocusItem);

  CloseSubMenus;  // deactivates menubar!
  
  if mi.SubMenu <> nil then
  begin
    ActivateMenu;
    // showing the submenu
    mi.SubMenu.ShowAt(self.WinHandle,GetItemPosX(FFocusItem)+2, guistyle.MenuFont.Height+4);
    mi.SubMenu.OpenerPopup := nil;
    mi.SubMenu.OpenerMenuBar := self;
    
    mi.SubMenu.SetDontCloseWidget(self);

    FocusedPopupMenu := mi.SubMenu;

    self.Repaint;
  end
  else
  begin
    VisibleItem(FFocusItem).Click;
    DeActivateMenu;
  end;
end;

procedure TMenuBar.DoShow;
begin
  PrepareToShow;
  inherited;
end;

procedure TMenuBar.DrawColumn(col: integer; focus: boolean);
var
  n : integer;
  r : TGfxRect;
  mi : TMenuItem;
begin
  r.SetRect(2,1, 1, guistyle.MenuFont.Height+2);

  for n:=1 to VisibleCount do
  begin
    mi := VisibleItem(n);

    r.width := ItemWidth(mi);

    if col = n then
    begin
{
      if focus and not MenuFocused then
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
}

      if focus and Focused then
      begin
        if MenuFocused then
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

      mi.DrawText(canvas,r.left+4,r.top+1);

      EXIT;
    end;

    inc(r.Left, ItemWidth(mi) );
  end;

end;

function TMenuBar.GetItemPosX(index: integer): integer;
var
  n : integer;
begin
  result := 0;
  if index < 1 then Exit;
  n := 1;
  while (n <= VisibleCount) and (n < index) do
  begin
    Inc(result, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TMenuBar.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  oldf : integer;
  i,trycnt : integer;
  s : string16;

  procedure FollowFocus;
  begin
    if oldf <> FFocusItem then
    begin
      DrawColumn(oldf,false);
      DrawColumn(FFocusItem,true);
    end;
  end;

begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  oldf := FFocusItem;

  consumed := true;
  case keycode of
    KEY_LEFT:
           begin // left
             CloseSubmenus;

             trycnt := 2;
             i := FFocusItem-1;
             repeat
               while (i >= 1) and not VisibleItem(i).Selectable do dec(i);

               if i >= 1 then break;

               i := VisibleCount;
               dec(trycnt);
             until trycnt > 0;

             if i >= 1 then FFocusItem := i;

             if VisibleItem(FFocusItem).SubMenu <> nil then DoSelect;
           end;

    KEY_RIGHT:
           begin // down
             CloseSubmenus;

             trycnt := 2;
             i := FFocusItem+1;
             repeat
               while (i <= VisibleCount) and not VisibleItem(i).Selectable do inc(i);

               if i <= VisibleCount then break;

               i := 1;
               dec(trycnt);
             until trycnt > 0;

             if i <= VisibleCount then FFocusItem := i;

             if VisibleItem(FFocusItem).SubMenu <> nil then DoSelect;
           end;

    KEY_ENTER:
           begin
             DoSelect;
           end;

    KEY_BACKSPACE:
           begin
             if (VisibleItem(FFocusItem).SubMenu <> nil) and VisibleItem(FFocusItem).SubMenu.Windowed
               then VisibleItem(FFocusItem).SubMenu.Close;
           end;
{
    KEY_ESC:
           begin
           end;
}
  else
    consumed := false;
  end;

  FollowFocus;

  if (not consumed) and ((keycode and $8000) <> $8000) then
  begin
    // normal char
    s := chr(keycode and $00FF) + chr((keycode and $FF00) shr 8);
    i := SearchItemByAccel(s);
    if i > 0 then
    begin
      FFocusItem := i;
      FollowFocus;

      Consumed := true;
      
      CloseSubmenus;
      DoSelect;
    end;
  end;

end;

procedure TMenuBar.HandleMouseMove(x, y: integer; btnstate,shiftstate: word);
var
  newf : integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  
  exit;

  if not MenuFocused then Exit;

  newf := CalcMouseCol(x);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf = FFocusItem then Exit;

  DrawColumn(FFocusItem,false);
  FFocusItem := newf;
  DrawColumn(FFocusItem,true);
end;

procedure TMenuBar.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
var
  newf : integer;
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  
  exit;

  newf := CalcMouseCol(x);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf <> FFocusItem then
  begin
    DrawColumn(FFocusItem,false);
    FFocusItem := newf;
    DrawColumn(FFocusItem,true);
  end;

  if button <> 1 then Exit;

  DoSelect;
end;

procedure TMenuBar.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
var
  newf : integer;
begin
  inherited HandleMouseDown(x, y, button, shiftstate);

  if not Focused then ActivateMenu;

  newf := CalcMouseCol(x);

  if NOT VisibleItem(newf).Selectable then Exit;

  if newf <> FFocusItem then
  begin
    DrawColumn(FFocusItem,false);
    FFocusItem := newf;
    DrawColumn(FFocusItem,true);
  end;

  if button <> 1 then Exit;

  DoSelect;
end;


function TMenuBar.ItemWidth(mi: TMenuItem): integer;
begin
  result := guistyle.MenuFont.TextWidth16(mi.Text) + 2*6;
end;

function TMenuBar.MenuFocused: boolean;
var
  n : integer;
  mi : TMenuItem;
begin
  result := true;
  for n := 1 to VisibleCount do
  begin
    mi := VisibleItem(n);
    if (mi.SubMenu <> nil) and (mi.SubMenu.Windowed) then
    begin
      //Writeln('Visible menu: ',u16noesc(mi.Text));
      result := false;
      break;
    end;
  end;
end;

procedure TMenuBar.PrepareToShow;
var
  n : integer;
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
  
end;

procedure TMenuBar.Repaint;
var
  n : integer;
begin
  inherited Repaint;

  Canvas.Clear(FBackgroundColor);

  for n:=1 to VisibleCount do
  begin
    DrawColumn(n,n = FFocusItem);
  end;
end;

function TMenuBar.SearchItemByAccel(s: string16): integer;
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

function TMenuBar.VisibleCount: integer;
begin
  result := FItems.Count;
end;

function TMenuBar.VisibleItem(ind: integer): TMenuItem;
begin
  if (ind < 1) or (ind > FItems.Count)
    then result := nil
    else result := TMenuItem(FItems.Items[ind-1]);
end;

procedure TMenuBar.DeActivateMenu;
begin
  Parent.ActiveWidget := nil;
end;

procedure TMenuBar.ActivateMenu;
begin
  Parent.ActiveWidget := self;
end;

end.

