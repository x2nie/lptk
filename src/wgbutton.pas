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
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget;

type
  TwgButton = class(TWidget)
  private
    FPushed  : Boolean;
    FClicked : Boolean;
    procedure SetText(const AValue : String16);

  protected
    FText : String16;
    FFont : TGfxFont;
    function GetText8 : String;
    procedure SetText8(const AValue : String);
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

    property Text : String16 read FText write SetText;
    property Text8 : String read GetText8 write SetText8;
    
    property Font : TGfxFont read FFont;
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
end;

destructor TwgButton.Destroy;
begin
  FText := '';
  inherited Destroy;
end;

procedure TwgButton.RePaint;
begin
  inherited RePaint;
  Canvas.Clear(FBackgroundColor);

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
//    Canvas.FillRectangle(2,2,width-3,height-3);
    Canvas.FillRectangle(3,3,width-5,height-5);
    Canvas.SetTextColor(clSelectionText);
  end
  else Canvas.SetTextColor(clText1);

  Canvas.SetFont(Font);

  canvas.DrawString16((width div 2) - (FFont.TextWidth16(FText) div 2), 4, FText);

end;

procedure TwgButton.HandleKeyPress(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    consumed := true;
    FClicked := TRUE;
    FPushed := TRUE;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleKeyRelease(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  inherited;
  if not FClicked then Exit;
  if (keycode = KEY_ENTER) or (keycode = 32) then
  begin
    FClicked := FALSE;
    FPushed := FALSE;
    if FWinHandle > 0 then RePaint;
    Click;
  end;
end;

procedure TwgButton.HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word);
begin
  inherited HandleMouseDown(X, Y, Button, ShiftState);
  if Button = 1 then   // this should be some constant value i think...
  begin
    FPushed := TRUE;
    FClicked := TRUE;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseUp(x, y : integer; button : word; shiftstate : word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
  if (button = 1) then	// this should be some constant value i think...
  begin
    FClicked := FALSE;
    if (FPushed) then
    begin
      FPushed := FALSE;
      Click;
    end;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if FPushed then
  begin
    FPushed := FALSE;
    if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if FClicked then
  begin
     FPushed := TRUE;
     if FWinHandle > 0 then RePaint;
  end;
end;

procedure TwgButton.Click;
begin
  if Assigned(OnClick) then OnClick(self);
end;

end.

