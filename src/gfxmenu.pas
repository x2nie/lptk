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
  end;

  TPopupMenu = class(TPopupWindow)
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

  FItems := TList.Create;

  FMenuFont := guistyle.LabelFont1;
  FMenuAccelFont := guistyle.LabelFont2;
  FMenuDisabledFont := guistyle.LabelFont1;
  
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
  
  FHeight := 2+2+1 + h;
  FWidth := 2+2+1 + FSymbolWidth + tw + hkw;

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
  r : TGfxRect;
begin
  inherited Repaint;

  Canvas.Clear(FBackgroundColor);
  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0,0,width-1,height-1);
  Canvas.DrawLine(2,Height-1,width-1,Height-1);
  Canvas.DrawLine(Width-1,2,Width-1,Height-1);

  r.SetRect(2,2, FWidth-1-2*2, FHeight-1-2*2);

  for n:=1 to VisibleCount do
  begin
    mi := VisibleItem(n);
    
    r.height := ItemHeight(mi);
    
    if (n = 1) and (not mi.Separator) then
    begin
      canvas.SetColor(clSelection);
      canvas.SetTextColor(clSelectionText);
    end
    else
    begin
      canvas.SetColor(BackgroundColor);
      canvas.SetTextColor(clText1);
    end;
    canvas.FillRect(r);
    
    DrawItem(mi,r);

    inc(r.Top, ItemHeight(mi) );
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
  
  if VisibleItem(newf).Separator then Exit;
  
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

  if VisibleItem(newf).Separator then Exit;
  
  if newf <> FFocusItem then
  begin
    DrawRow(FFocusItem,false);
    FFocusItem := newf;
    DrawRow(FFocusItem,true);
  end;
  
  if button <> 1 then Exit;
  
  // Close this popup
  Close;
  
  VisibleItem(FFocusItem).Click;
end;

procedure TPopupMenu.DrawItem(mi : TMenuItem; rect: TGfxRect);
var
  s16 : string;
  x : integer;
begin
  if mi.Separator then
  begin
    Canvas.SetColor(clText1);
    Canvas.DrawLine(rect.Left, rect.Top+2, rect.Right, rect.Top+2);
  end
  else
  begin
    x := rect.Left + FSymbolWidth;
    Canvas.DrawString16(x, rect.Top, mi.Text);
    
    if mi.HotKeyDef8 <> '' then
    begin
      s16 := u8(mi.HotKeyDef8);
      Canvas.DrawString16(rect.Right-FMenuFont.TextWidth16(s16)-2, rect.Top, s16);
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
  r.SetRect(2,2, FWidth-1-2*2, FHeight-1-2*2);

  for n:=1 to VisibleCount do
  begin
    mi := VisibleItem(n);

    r.height := ItemHeight(mi);
    
    if line = n then
    begin
      if (focus) and (not mi.Separator) then
      begin
        canvas.SetColor(clSelection);
        canvas.SetTextColor(clSelectionText);
      end
      else
      begin
        canvas.SetColor(BackgroundColor);
        canvas.SetTextColor(clText1);
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

