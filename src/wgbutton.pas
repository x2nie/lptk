{ wgbutton.pas: Button widget
  File maintainer: nvitya@freemail.hu

History:
}

unit wgbutton;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget, gfximagelist;

type
  TwgButton = class(TWidget)
  private
    FImageName: string;
    FClicked : Boolean;
    FImage : TgfxImage;
    FShowImage : Boolean;
    FClickOnPush : Boolean;
    FDown : Boolean;
    FImageMargin: integer;
    FImageSpacing: integer;
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    procedure SetImageName(const AValue: string);
    procedure SetText(const AValue : String16);
    procedure SetDown(AValue : Boolean);
    procedure SetImageMargin(const Value: integer);
    procedure SetImageSpacing(const Value: integer);
    function GetAllowDown: Boolean;
    procedure SetAllowDown(const Value: Boolean);
    procedure SetAllowAllUp(const Value: boolean);
  protected
    FText : String16;
    FFont : TGfxFont;
    function GetText8 : String;
    procedure SetText8(const AValue : String);
    procedure SetShowImage(AValue : Boolean);
  public
    OnClick : TNotifyEvent;

  public
    FModalResult : integer;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;

    procedure DoPush;
    procedure DoRelease;

    procedure HandleMouseExit; override;
    procedure HandleMouseEnter; override;

    procedure Click;

    property ShowImage : Boolean read FShowImage write SetShowImage;
    property Down : Boolean read FDown write SetDown;
    property Text8 : String read GetText8 write SetText8;

    property Font : TGfxFont read FFont;

    property AllowDown : Boolean read GetAllowDown write SetAllowDown;
    
  published

    property Text : String16 read FText write SetText;

    property FontName : string read GetFontName write SetFontName;

    property ImageName : string read FImageName write SetImageName;

    property ImageMargin : integer read FImageMargin write SetImageMargin;
    property ImageSpacing : integer read FImageSpacing write SetImageSpacing;

    property GroupIndex : integer read FGroupIndex write FGroupIndex;
    property AllowAllUp : boolean read FAllowAllUp write SetAllowAllUp;

    property ModalResult : integer read FModalResult write FModalResult;

  end;

function CreateButton(AOwner : TComponent; x, y, w : TGfxCoord; txt : String; onclk : TNotifyEvent) : TwgButton;

implementation

uses gfxstyle, gfxform;

function CreateButton(AOwner : TComponent; x, y, w : TGfxCoord; txt : String; onclk : TNotifyEvent) : TwgButton;
begin
  Result := TwgButton.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := Str8To16(txt);
  Result.Width := w;
  Result.OnClick := onclk;
end;

{ TwgButton }

procedure TwgButton.SetDown(AValue : Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AllowDown and Windowed then RePaint;
  end;
end;

procedure TwgButton.SetShowImage(AValue : Boolean);
begin
  if AValue <> FShowImage then
  begin
    FShowImage := AValue;
    if (FImage <> nil) and ShowImage then RePaint;
  end;
end;


function TwgButton.GetText8 : String;
begin
    result := Str16To8(Text);
end;

procedure TwgButton.SetText8(const AValue : String);
begin
    SetText(Str8To16(AValue));
end;

procedure TwgButton.SetText(const AValue : String16);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

procedure TwgButton.SetImageName(const AValue: string);
begin
  FImageName:=AValue;
  FImage := GfxLibGetImage(FImageName);
  if Windowed then Repaint;
  //FShowImage := true;
end;

function TwgButton.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgButton.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

constructor TwgButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('Button');
  FFont := GfxGetFont('#Label1');
  FHeight := FFont.Height + 8;
  FWidth := 96;
  FFocusable := True;
  FBackgroundColor := clButtonFace;
  OnClick := nil;
  FDown := FALSE;
  FClicked := FALSE;
  FDown := False;
  FClickOnPush := False;
  FGroupIndex := 0;
  FImage := nil;
  FImageName := '';
  FShowImage := true;
  FImageMargin := 3;
  FImageSpacing := -1;
  FModalResult := 0;
end;

destructor TwgButton.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgButton.RePaint;
var
  AText : String16;
  x,y,iy,w : integer;
  r : TGfxRect;
  pofs : integer;
begin
  if not Windowed then Exit;
  inherited RePaint;

  Canvas.Clear(FBackgroundColor);
  Canvas.ClearClipRect;

  if not FDown then Canvas.SetColor(clHilite1)
           else Canvas.SetColor(clShadow2);

  Canvas.DrawLine(0,height-2, 0,0);
  Canvas.DrawLine(0,0, width-1,0);

  if not FDown then Canvas.SetColor(clHilite2)
           else Canvas.SetColor(clShadow1);

  Canvas.DrawLine(1,height-3, 1,1);
  Canvas.DrawLine(1,1, width-2,1);

  if not FDown then Canvas.SetColor(clShadow2)
           else Canvas.SetColor(clHilite1);

  Canvas.DrawLine(width-1,1, width-1,height-1);
  Canvas.DrawLine(0,height-1, width-1,height-1);

  if not FDown then Canvas.SetColor(clShadow1)
           else Canvas.SetColor(clHilite2);

  Canvas.DrawLine(width-2,2, width-2,height-2);
  Canvas.DrawLine(1,height-2, width-2,height-2);

  if FFocused then
  begin
    Canvas.SetColor(clSelection);
    Canvas.FillRectangle(3,3,width-5,height-5);
    Canvas.SetTextColor(clSelectionText);
    Canvas.SetColor(clSelectionText);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  r.left := 2;
  r.top  := 2;
  r.width := Width-4;
  r.height := Height-4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  AText := FText;
  y := Height div 2 - FFont.Height div 2;
  if y < 3 then y := 3;

  if FDown then pofs := 1 else pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := Height div 2 - FImage.Height div 2;
    if ImageMargin = -1 then // centered
    begin
      w := FFont.TextWidth16(AText) + FImage.Width;
      if FImageSpacing > 0 then inc(w,FImageSpacing);
      x := (width div 2) - (w div 2);
      if x < 3 then x := 3;
    end
    else
    begin
      x := FImageMargin+3;
    end;

    Canvas.DrawImage(x+pofs, iy+pofs, FImage);
    inc(x, FImage.Width);
    if FImageSpacing > 0 then inc(x,FImageSpacing);

    if (FImageSpacing = -1) and (FImageMargin >= 0) then
    begin
      w := (Width-3-x) div 2 - FFont.TextWidth16(AText) div 2;
      if w < 1 then w := 1; // minimal spaceing
      x := x + w;
    end;
  end
  else
  begin
    x := (width div 2) - (FFont.TextWidth16(AText) div 2);
  end;
  if x < 3 then x := 3;
  Canvas.DrawString16(x+pofs, y+pofs, AText);
end;

procedure TwgButton.DoPush;
var
  n : integer;
  c : TComponent;
begin
  FClickOnPush := (not FDown) and AllowDown;

  // search the other buttons in the group
  for n:=0 to FParent.ComponentCount-1 do
  begin
    c := FParent.Components[n];
    if (c <> self) and (c is TwgButton) then
    begin
      with TwgButton(c) do
      begin
        if GroupIndex = self.GroupIndex then Down := false;
      end;
    end;
  end;

  FDown := True;
  FClicked := TRUE;
  if FWinHandle > 0 then RePaint;

  if FClickOnPush then Click;
end;

procedure TwgButton.DoRelease;
begin
  if AllowDown then
  begin
    if FDown and (not FClickOnPush) and FAllowAllUp then
    begin
      FDown := False;
      if FWinHandle > 0 then RePaint;
      Click;
    end;
  end
  else
  begin
    if FDown and FClicked then Click;
    FDown := FALSE;
    if FWinHandle > 0 then RePaint;
  end;

  FClickOnPush := false;
  FClicked := false;
end;


procedure TwgButton.HandleKeyPress(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    DoPush;
    Consumed := true;
  end;
end;

procedure TwgButton.HandleKeyRelease(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    DoRelease;
    Consumed := true;
  end;
end;

procedure TwgButton.HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word);
begin
  inherited HandleMouseDown(X, Y, Button, ShiftState);
  if Button = 1 then   // this should be some constant value i think...
  begin
    DoPush;
  end;
end;

procedure TwgButton.HandleMouseUp(x, y : integer; button : word; shiftstate : word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  if (button = 1) then	// this should be some constant value i think...
  begin
    DoRelease;
  end;
end;

procedure TwgButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if FDown and (not AllowDown) then
  begin
    FDown := FALSE;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if FClicked and (not AllowDown) then
  begin
     FDown := TRUE;
     if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.Click;
var
  pform : TGfxForm;
begin
  pform := WidgetParentForm(self);
  if pform <> nil then pform.ModalResult := self.ModalResult;
  
  if Assigned(OnClick) then OnClick(self);
end;

procedure TwgButton.SetImageMargin(const Value: integer);
begin
  FImageMargin := Value;
  if Windowed then Repaint;
end;

procedure TwgButton.SetImageSpacing(const Value: integer);
begin
  FImageSpacing := Value;
  if Windowed then Repaint;
end;

function TwgButton.GetAllowDown: Boolean;
begin
  result := GroupIndex > 0;
end;

procedure TwgButton.SetAllowDown(const Value: Boolean);
begin
  GroupIndex := 1;
end;

procedure TwgButton.SetAllowAllUp(const Value: boolean);
begin
  FAllowAllUp := Value;
end;

end.

