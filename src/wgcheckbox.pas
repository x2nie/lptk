{ wgcheckbox.pas: CheckBox widget
  File maintainer: nvitya@freemail.hu

History:
}
unit wgcheckbox;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget;

type
  TwgCheckBox = class(TWidget)
  private
    procedure SetText(const AValue : String16);
    procedure SetChecked(const Value: boolean);
    function GetFontName: string;
    procedure SetFontName(const AValue: string);
  protected
    FText : String16;
    FFont : TGfxFont;

    FBoxSize : integer;

    FChecked : boolean;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    property Font : TGfxFont read FFont;

  public

    OnChange : TNotifyEvent;

  published

    property Text : String16 read FText write SetText;
    property Checked : boolean read FChecked write SetChecked;

    property FontName : string read GetFontName write SetFontName;

  end;

//  TLabelClass = class of TwgCheckBox;

function CreateCheckBox(AOwner : TComponent; x, y : TGfxCoord; txt : String) : TwgCheckBox;

implementation

uses gfxstyle;

function CreateCheckBox(AOwner : TComponent; x, y : TGfxCoord; txt : String) : TwgCheckBox;
begin
  Result := TwgCheckBox.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := Str8To16(txt);
  Result.Width := Result.Font.TextWidth16(Result.Text) + 24;
end;

{ TwgCheckBox }

procedure TwgCheckBox.SetText(const AValue : String16);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

constructor TwgCheckBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('CheckBox');
  FFont := GfxGetFont('#Label1');
  FHeight := FFont.Height + 4;
  FWidth := 120;

  FFocusable := true;

  FBoxSize := 14;

  FChecked := false;
  OnChange := nil;
end;

destructor TwgCheckBox.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgCheckBox.RePaint;
var
  r : TGfxRect;
  ty,tx : integer;
begin
  //inherited RePaint;
  //writeln('label paint');

  Canvas.SetFont(Font);

  if FFocused then
  begin
    canvas.SetColor(clSelection);
    canvas.SetTextColor(clSelectionText);
  end
  else
  begin
    canvas.SetColor(FBackgroundColor);
    canvas.SetTextColor(clText1);
  end;

  canvas.FillRectangle(0,0,width,height);

  r.SetRect(2,(Height div 2) - (FBoxSize div 2), FBoxSize,FBoxSize);
  if r.top < 0 then r.top := 0;

  canvas.SetColor(clBoxColor);
  canvas.FillRect(r);
  DrawControlFrame(canvas,r.Left,r.Top,r.width,r.height);

  canvas.SetColor(clText1);
//  canvas.DrawRect(r);

  tx := r.right + 8;

  inc(r.left,4);
  inc(r.top,4);
  dec(r.width,8);
  dec(r.height,8);

  canvas.SetLineStyle(2,false);
  if FChecked then
  begin
    canvas.DrawLine(r.left,r.top,r.right,r.bottom);
    canvas.DrawLine(r.Right,r.top,r.left,r.bottom);
  end;

  canvas.SetLineStyle(1,false);

  ty := (Height div 2) - (Font.Height div 2);
  if ty < 0 then ty := 0;

  Canvas.DrawString16(tx,ty,FText);

end;

procedure TwgCheckBox.SetChecked(const Value: boolean);
begin
  if Value = FChecked then Exit;
  FChecked := Value;
  if WinHandle > 0 then RePaint;
end;

procedure TwgCheckBox.HandleMouseUp(x, y: integer; button, shiftstate: word);
begin
  inherited;
  Checked := not Checked;
  if Assigned(OnChange) then OnChange(self);
end;

procedure TwgCheckBox.HandleKeyPress(var keycode, shiftstate: word; var consumed: boolean);
begin
  inherited;

  if Consumed then exit;

  if (KeyCode = $20) or (KeyCode = KEY_ENTER) then // space
  begin
    Checked := not Checked;
    if keycode <> KEY_ENTER then Consumed := true;
  end;

end;

function TwgCheckBox.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgCheckBox.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

end.

