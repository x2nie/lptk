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
    FPushed  : Boolean;
    FClicked : Boolean;
    FImage : TgfxImage;
    FShowImage : Boolean;
    FAllowDown : Boolean;
    FConsumed : Boolean;
    FDown : Boolean;
    FImageMargin: integer;
    FImageSpacing: integer;
    procedure SetImageName(const AValue: string);
    procedure SetText(const AValue : String16);
    procedure SetDown(AValue : Boolean);
    procedure SetImageMargin(const Value: integer);
    procedure SetImageSpacing(const Value: integer);
  protected
    FText : String16;
    FFont : TGfxFont;
    function GetText8 : String;
    procedure SetText8(const AValue : String);
    procedure SetShowImage(AValue : Boolean);
  public
    OnClick : TNotifyEvent;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleMouseExit; override;
    procedure HandleMouseEnter; override;

    procedure Click;

    property AllowDown : Boolean read FAllowDown write FAllowDown;
    property Down : Boolean read FDown write SetDown;
    property Text : String16 read FText write SetText;
    property Text8 : String read GetText8 write SetText8;

    property Font : TGfxFont read FFont;
{
    // image-properties
    property ImageList : TgfxImageList read FImageList write SetImageList;
    property ImageIndex : Longword read FImageIndex write SetImageIndex;
}
    property ImageName : string read FImageName write SetImageName;
    property ShowImage : Boolean read FShowImage write SetShowImage;

    property ImageMargin : integer read FImageMargin write SetImageMargin;
    property ImageSpacing : integer read FImageSpacing write SetImageSpacing;

  end;

function CreateButton(AOwner : TComponent; x, y, w : TGfxCoord; txt : String; onclk : TNotifyEvent) : TwgButton;

implementation

uses gfxstyle;

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
          if AllowDown then RePaint;
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
  //FShowImage := true;
end;

constructor TwgButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := '';
  FFont := guistyle.LabelFont1;
  FHeight := FFont.Height + 8;
  FFocusable := True;
  FBackgroundColor := clButtonFace;
  OnClick := nil;
  FPushed := FALSE;
  FClicked := FALSE;
  FDown := False;
  FConsumed := False;
  FAllowDown := False;
  FImage := nil;
  FImageName := '';
  FShowImage := true;
  FImageMargin := 3;
  FImageSpacing := -1;
end;

destructor TwgButton.Destroy;
begin
  FText := '';
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

  if not FPushed then Canvas.SetColor(clHilite1)
           else Canvas.SetColor(clShadow2);

  Canvas.DrawLine(0,height-2, 0,0);
  Canvas.DrawLine(0,0, width-1,0);

  if not FPushed then Canvas.SetColor(clHilite2)
           else Canvas.SetColor(clShadow1);

  Canvas.DrawLine(1,height-3, 1,1);
  Canvas.DrawLine(1,1, width-2,1);

  if not FPushed then Canvas.SetColor(clShadow2)
           else Canvas.SetColor(clHilite1);

  Canvas.DrawLine(width-1,1, width-1,height-1);
  Canvas.DrawLine(0,height-1, width-1,height-1);

  if not FPushed then Canvas.SetColor(clShadow1)
           else Canvas.SetColor(clHilite2);

  Canvas.DrawLine(width-2,2, width-2,height-2);
  Canvas.DrawLine(1,height-2, width-2,height-2);

  if FFocused then
  begin
    Canvas.SetColor(clSelection);
    Canvas.FillRectangle(3,3,width-5,height-5);
    Canvas.SetTextColor(clSelectionText);
  end
  else Canvas.SetTextColor(clText1);

  r.left := 2;
  r.top  := 2;
  r.width := Width-4;
  r.height := Height-4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  AText := FText;
  y := Height div 2 - FFont.Height div 2;
  if y < 3 then y := 3;

  if FPushed then pofs := 1 else pofs := 0;

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
      w := (Width-2-x) div 2 - FFont.TextWidth16(AText) div 2;
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

procedure TwgButton.HandleKeyPress(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    consumed := true;
    FClicked := TRUE;
    FPushed := TRUE;
    
    if AllowDown and (not FDown) then
    begin
         FConsumed := True;
         FDown := True;
    end else FConsumed := false;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleKeyRelease(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if not FClicked then Exit;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    if AllowDown then
    begin
       if FDown and (not FConsumed) then
       begin
            FDown := False;
            FClicked := False;
            FPushed := False;
            RePaint;
            Click;
       end;
    end
    else
    begin
       FClicked := FALSE;
       FPushed := FALSE;
       if FWinHandle > 0 then RePaint;
       Click;
    end;
  end;
end;

procedure TwgButton.HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word);
begin
  inherited HandleMouseDown(X, Y, Button, ShiftState);
  if Button = 1 then   // this should be some constant value i think...
  begin
    FConsumed := False;
    if (not FPushed) and AllowDown then
    begin
         FConsumed := True;
         FPushed := True;
         FDown := True;
         Click;
         RePaint;
    end
    else
    begin
         FPushed := TRUE;
         FClicked := TRUE;
         if FWinHandle > 0 then RePaint;
    end;
  end;
end;

procedure TwgButton.HandleMouseUp(x, y : integer; button : word; shiftstate : word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  if (button = 1) then	// this should be some constant value i think...
  begin
    if AllowDown then
    begin
         if not FConsumed then
         begin
              FDown := False;
              FPushed := False;
              FClicked := False;
              FConsumed := False;
              Click;
              RePaint;
         end;
    end
    else
    begin
        FClicked := FALSE;
        if (FPushed) then
        begin
             FPushed := FALSE;
             Click;
        end;
    end;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if FPushed and (not AllowDown) then
  begin
    FPushed := FALSE;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if FClicked and (not AllowDown) then
  begin
     FPushed := TRUE;
     if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.Click;
begin
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

end.

