// not a real part of the LPTK yet

unit wgmenu;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, messagequeue, gfxbase, GfxStyle, GfxWidget, popupwindow;

type

  TPopupMenu = class(TPopupWindow)
  protected
    FFont : TGfxFont;

  public
    constructor Create(AOwner : TComponent); override;

    procedure RePaint; override;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

  end;

  TwgMenuBar = class(TWidget)
  protected
    FFont : TGfxFont;

    FFocusItem : integer;

  public

    PopupWin : TPopupMenu;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    // New functions

    function ItemCount : integer; virtual;
    function ItemWidth(num : integer) : integer; virtual;
    procedure DrawItem(num : integer; rect : TGfxRect; flags : integer); virtual;

    // Properties

    property Font : TGfxFont read FFont;
  end;

implementation

uses X, Xlib, Xutil;

{ TwgMenuBar }

constructor TwgMenuBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFont := guistyle.LabelFont1;
  FHeight := FFont.Height;
  FFocusable := true;
  FFocusItem := 2;
  PopupWin := nil;
end;

destructor TwgMenuBar.Destroy;
begin
  inherited Destroy;
end;

procedure TwgMenuBar.RePaint;
var
  n : integer;
  r : TGfxRect;
begin
  inherited RePaint;
  Canvas.Clear;
  Canvas.SetFont(Font);

  if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
  Canvas.DrawRectangle(0,0,width-1,height-1);

  r.SetRect(2,2, width-4, height-4);

  for n := 1 to ItemCount do
  begin
    r.width := ItemWidth(n);
    canvas.SetClipRect(r);
    if n = FFocusItem then
    begin
      canvas.SetColor(clSelection);
      canvas.SetTextColor(clSelectionText);
    end
    else
    begin
      canvas.SetColor(clWindowBackground);
      canvas.SetTextColor(clText3);
    end;
    canvas.FillRectangle(r.left,r.top,r.width,r.height);
    DrawItem(n,r,0);
    r.left := r.left + r.width;
  end;
  canvas.ClearClipRect;
end;

procedure TwgMenuBar.HandleKeyPress(var keycode : word; var shiftstate : word; var consumed : boolean);
var
  dx,dy : integer;
  cw : TWinHandle;

  wh  : TWinHandle;
  attr : TXSetWindowAttributes;
  mask : longword;
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  case keycode of
    $FF52: begin // up
             if FFocusItem > 1 then dec(FFocusItem);
             RePaint;
             consumed := true;
           end;
    $FF54: begin // down
             if FFocusItem < ItemCount then inc(FFocusItem);
             RePaint;
             consumed := true;
           end;
  end;
end;

function TwgMenuBar.ItemCount : integer;
begin
  result := 3;
end;

function TwgMenuBar.ItemWidth(num : integer) : integer;
begin
  result := 50;
end;

procedure TwgMenuBar.DrawItem(num : integer; rect : TGfxRect; flags : integer);
var
  s : string16;
begin
  s := Str8To16('Menu'+IntToStr(num));
  canvas.DrawString16(rect.left+4, rect.top+1,s);
  //Canvas.DrawRectangle(rect.left+1,rect.top+1,rect.width-1,rect.height-1);
end;

{ TPopupMenu }

constructor TPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := guistyle.LabelFont1;
end;

procedure TPopupMenu.RePaint;
var
  s : string;
begin
  //inherited RePaint;

  Canvas.Clear;
  Canvas.SetFont(FFont);

  s := Str8To16('PopupMenu');
  canvas.DrawString16(2,2, s);

end;

procedure TPopupMenu.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
var
  pm2 : TPopupMenu;
begin
  Writeln('PopupMenu mousedown: (',x,',',y,')');
  pm2 := TPopupMenu.Create(self);
  pm2.Width := 100;
  pm2.height := 50;
  pm2.BackGroundColor := $80C080;
  pm2.ShowAt(self.WinHandle, x,y);
end;

end.

