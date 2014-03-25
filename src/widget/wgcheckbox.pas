unit wgcheckbox;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, pgf_defs, pgf_main, pgf_widget;

type
  TwgCheckBox = class(TpgfWidget)
  private
    procedure SetText(const AValue : WideString);
    procedure SetChecked(const Value: boolean);
    function GetFontName: string;
    procedure SetFontName(const AValue: string);
  protected
    FText : WideString;
    FFont : TpgfFont;

    FBoxSize : integer;

    FChecked : boolean;
    
    FBackgroundColor : TpgfColor;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    property Font : TpgfFont read FFont;

  public

    OnChange : TNotifyEvent;

  published

    property Text : WideString read FText write SetText;
    property Checked : boolean read FChecked write SetChecked;

    property FontName : string read GetFontName write SetFontName;

  end;

//  TLabelClass = class of TwgCheckBox;

function CreateCheckBox(AOwner : TComponent; x, y : TpgfCoord; txt : WideString) : TwgCheckBox;

implementation

function CreateCheckBox(AOwner : TComponent; x, y : TpgfCoord; txt : WideString) : TwgCheckBox;
begin
  Result := TwgCheckBox.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := txt;
  Result.Width := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TwgCheckBox }

procedure TwgCheckBox.SetText(const AValue : WideString);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

constructor TwgCheckBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('CheckBox');
  FFont := pgfGetFont('#Label1');
  FHeight := FFont.Height + 4;
  FWidth := 120;

  FBackgroundColor := clWindowBackground;

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

procedure TwgCheckBox.HandlePaint;
var
  r : TpgfRect;
  ty,tx : integer;
begin
  Canvas.BeginDraw;

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
  canvas.DrawControlFrame(r.Left,r.Top,r.width,r.height);

  canvas.SetColor(clText1);
//  canvas.DrawRect(r);

  tx := r.right + 8;

  inc(r.left,4);
  inc(r.top,4);
  dec(r.width,8);
  dec(r.height,8);

  canvas.SetLineStyle(2,lsSolid);
  if FChecked then
  begin
    canvas.DrawLine(r.left,r.top,r.right,r.bottom);
    canvas.DrawLine(r.Right,r.top,r.left,r.bottom);
  end;

  canvas.SetLineStyle(1,lsSolid);

  ty := (Height div 2) - (Font.Height div 2);
  if ty < 0 then ty := 0;

  Canvas.DrawString(tx,ty,FText);

  Canvas.EndDraw;
end;

procedure TwgCheckBox.SetChecked(const Value: boolean);
begin
  if Value = FChecked then Exit;
  FChecked := Value;
  if WinHandle > 0 then RePaint;
end;

procedure TwgCheckBox.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  inherited;
  Checked := not Checked;
  if Assigned(OnChange) then OnChange(self);
end;

procedure TwgCheckBox.HandleKeyChar(var keycode, shiftstate: word; var consumed: boolean);
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
  result := FFont.FontDesc;
end;

procedure TwgCheckBox.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

end.

