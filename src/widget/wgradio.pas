unit wgradio;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, pgf_defs, pgf_main, pgf_widget;

type
  TwgRadioButton = class(TpgfWidget)
  private
    procedure SetText(const AValue : WideString);
    procedure SetChecked(const Value: boolean);
    function GetFontName: string;
    procedure SetFontName(const AValue: string);
  protected
    FText : WideString;
    FFont : TpgfFont;

    FButtonSize : integer;

    FChecked : boolean;
    
    FGroupIndex : integer;

    FBackgroundColor : TpgfColor;

  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
    procedure HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    property Font : TpgfFont read FFont;
    
    property GroupIndex : integer read FGroupIndex write FGroupIndex;

  public

    OnChange : TNotifyEvent;

  published

    property Text : WideString read FText write SetText;
    property Checked : boolean read FChecked write SetChecked;

    property FontName : string read GetFontName write SetFontName;

  end;

function CreateRadioButton(AOwner : TComponent; x, y : TpgfCoord; txt : WideString) : TwgRadioButton;

implementation

function CreateRadioButton(AOwner : TComponent; x, y : TpgfCoord; txt : WideString) : TwgRadioButton;
begin
  Result := TwgRadioButton.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := txt;
  Result.Width := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TwgRadioButton }

procedure TwgRadioButton.SetText(const AValue : WideString);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

constructor TwgRadioButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('CheckBox');
  FFont := pgfGetFont('#Label1');
  FHeight := FFont.Height + 4;
  FWidth := 120;

  FBackgroundColor := clWindowBackground;

  FFocusable := true;

  FButtonSize := 14;

  FChecked := false;
  FGroupIndex := 0;
  
  OnChange := nil;
end;

destructor TwgRadioButton.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgRadioButton.HandlePaint;
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
  
  r.SetRect(2,(Height div 2) - (FButtonSize div 2), FButtonSize,FButtonSize);
  if r.top < 0 then r.top := 0;

  canvas.SetColor(clBoxColor);
  
{
  canvas.SetColor(clShadow1);
  Canvas.DrawLine(x,y, x+w-1,y);
  Canvas.DrawLine(x,y+h-1, x,y);

  canvas.SetColor(clShadow2);
  Canvas.DrawLine(x+1,y+1,  x+w-2,y+1);
  Canvas.DrawLine(x+1,y+h-2, x+1,y+1);

  canvas.SetColor(clHilite2);
  Canvas.DrawLine(x+1,y+h-1, x+w-1,y+h-1);
  Canvas.DrawLine(x+w-1,y+1, x+w-1,y+h-1);

  canvas.SetColor(clHilite1);
  Canvas.DrawLine(x+2,y+h-2, x+w-2,y+h-2);
  Canvas.DrawLine(x+w-2,y+2, x+w-2,y+h-2);
}
  
  // fill the base
  canvas.FillArc(r.Left,r.Top,r.Width,r.Height,0,2*pi);

  canvas.SetColor(clShadow1);
  canvas.DrawArc(r.Left,r.Top,r.Width,r.Height,pi/4,pi);

  canvas.SetColor(clShadow2);
  canvas.DrawArc(r.Left+1,r.Top,r.Width,r.Height,3*pi/4,pi/2);
  canvas.DrawArc(r.Left,r.Top+1,r.Width,r.Height,pi/4,pi/2);


  canvas.SetColor(clHilite2);
  canvas.DrawArc(r.Left,r.Top,r.Width,r.Height,5*pi/4,pi);

  canvas.SetColor(clHilite1);
  canvas.DrawArc(r.Left-1,r.Top,r.Width,r.Height,pi+3*pi/4,pi/2);
  canvas.DrawArc(r.Left,r.Top-1,r.Width,r.Height,pi+pi/4,pi/2);

  canvas.SetColor(clText1);
  
//  canvas.DrawRect(r);

  tx := r.right + 8;

  inc(r.left,4);
  inc(r.top,4);
  dec(r.width,8);
  dec(r.height,8);

  if FChecked then
  begin
    canvas.FillArc(r.Left,r.Top,r.Width,r.Height,0,2*pi);
  end;

  ty := (Height div 2) - (Font.Height div 2);
  if ty < 0 then ty := 0;

  Canvas.DrawString(tx,ty,FText);

  Canvas.EndDraw;
end;

procedure TwgRadioButton.SetChecked(const Value: boolean);
var
  pwg,wg : TpgfWidget;
  n : integer;
begin
  if Value = FChecked then Exit;
  
  FChecked := Value;
  
  if FChecked then
  begin
    // clear other radio buttons in this group
    pwg := self.Parent;
    if pwg <> nil then
    begin
      for n:=0 to pwg.ComponentCount-1 do
      begin
        wg := TpgfWidget(pwg.Components[n]);
        
        if (wg <> nil) and (wg <> self)
           and
           (wg is TwgRadioButton) and (TwgRadioButton(wg).GroupIndex = self.GroupIndex)
        then
        begin
        
          TwgRadioButton(pwg.Components[n]).Checked := false;
        end;
      end;
    end;
  end;
  
  RePaint;
end;

procedure TwgRadioButton.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  inherited;
  Checked := not Checked;
  if Assigned(OnChange) then OnChange(self);
end;

procedure TwgRadioButton.HandleKeyChar(var keycode, shiftstate: word; var consumed: boolean);
begin
  inherited;

  if Consumed then exit;

  if (KeyCode = $20) or (KeyCode = KEY_ENTER) then // space
  begin
    Checked := not Checked;
    if keycode <> KEY_ENTER then Consumed := true;
  end;

end;

function TwgRadioButton.GetFontName: string;
begin
  result := FFont.FontDesc;
end;

procedure TwgRadioButton.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

end.

