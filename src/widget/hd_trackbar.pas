unit hd_trackbar;

{ feature requests or bugs? - mail to: erik@grohnwaldt.de
    25.05.2003	0.1	Initial release
    13.09.2012 migrated to PasGf by Preben Bjørn Madsen
}

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses Classes, hd_defs, hd_main, hd_widget;

type
    TTrackBarChange = procedure(Sender : TObject; NewPosition : integer);
    
    TwgTrackbar = class(TpgfWidget)
	private
	    FSliderWidth : integer;
	    FMin : integer;
	    FMax : integer;
	    FPosition : integer;
	    FOrientation : TOrientation;
	    procedure DrawSlider(p : integer);
	    procedure SetMin(aValue : integer);
	    procedure SetMax(aValue : integer);
	    procedure SetPosition(aValue : integer);
	    procedure SetOrientation(aValue : TOrientation);
	    procedure SetSliderWidth(aValue : integer);
	    procedure DoChange;
	protected
		FBackgroundColor: TpgfColor;
	    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;
	    procedure HandleKeyPress(var KeyCode : word; var shiftstate : word; var consumed : boolean); override;
	public	    
	    onChange : TTrackbarChange;
	    procedure HandlePaint; override;
	    
	    constructor Create(aOwner : TComponent); override;
	    
	    property Max : integer read FMax write SetMax;
	    property Min : integer read FMin write SetMin;
	    property Position : integer read FPosition write SetPosition;
	    property Orientation : TOrientation read FOrientation write SetOrientation;
	    property SliderWidth : integer read FSliderWidth write SetSliderWidth;
    end;

function CreateTrackbar(AOwner : TComponent; x, y, Width, Height : TpgfCoord) : TwgTrackBar;

implementation

function CreateTrackbar(AOwner : TComponent; x, y, Width, Height : TpgfCoord) : TwgTrackBar;
begin
  Result := TwgTrackBar.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TwgTrackbar.SetSliderWidth(aValue : integer);
begin
    if aValue <> FSliderWidth then
    begin
	if aValue > 11 then
	begin
	    FSliderWidth := aValue;
	    RePaint;
	end;
    end;
end;

procedure TwgTrackbar.DoChange;
begin
    if assigned(onChange) then OnChange(self,Position);
end;

procedure TwgTrackbar.HandleLMouseUp(x,y : integer; shiftstate : word);
var
    p : integer;
    tmax, tmin : integer;
    linepos : double;
    drawwidth : integer;
    oldpos : integer;
begin
    OldPos := Position;
    if Max < Min then
    begin
	tmax := min;
	tmin := max;
    end
    else
    begin
	tmax := max;
	tmin := min;
    end;
    linepos := tmax - tmin;    
    if Orientation = orHorizontal then
    begin
	drawwidth := Width - 5 - FSliderWidth;
	linepos := drawwidth / linepos;
	Position := round((x - 2 - FSliderWidth div 2) / linepos) + tmin;
    end;
    if Orientation = orVertical then
    begin
	drawwidth := Height - 5 - FSliderWidth;
	linepos := drawwidth / linepos;
	Position := round((x - 2 - FSliderWidth div 2) / linepos) + tmin;    
    end;
    if Position <> OldPos then DoChange;
end;

procedure TwgTrackbar.HandleKeyPress(var KeyCode : word; var shiftstate : word; var consumed : boolean);
var
    oldpos : integer;
begin
    Consumed := true;
    oldpos := Position;
    if Orientation = orHorizontal then
    begin
	case KeyCode of
	    KEY_LEFT: Position := Position - 1;	    
	    KEY_RIGHT: Position := Position + 1;
	    KEY_PGUP : Position := FMin;
	    KEY_PGDN : Position := FMax;
	    else Consumed := false;
	end;
    end;
    if Orientation = orVertical then
    begin
	case KeyCode of
	    KEY_UP: Position := Position - 1;	    
	    KEY_DOWN: Position := Position + 1;
	    KEY_PGUP : Position := FMin;
	    KEY_PGDN : Position := FMax;
	    else Consumed := false;
	end;
    end;
    inherited HandleKeyPress(KeyCode, ShiftState, Consumed);
    if Position <> OldPos then DoChange;
end;

procedure TwgTrackbar.DrawSlider(p : integer);
var
    h : integer;
begin
    if Orientation = orHorizontal then
    begin
	h := Height div 2 - 1;
	Canvas.SetColor(clHilite1);
	Canvas.DrawLine(p - FSliderWidth div 2,5, p + FSliderWidth div 2, 5);
	Canvas.DrawLine(p - FSliderWidth div 2,5, p - FSliderWidth div 2, h - FSliderWidth div 2);
	Canvas.DrawLine(p - FSliderWidth div 2, h - FSliderWidth div 2, p, h + FSliderWidth div 2);
	Canvas.SetColor(clHilite2);
	Canvas.DrawLine(p - FSliderWidth div 2 + 1,6, p + FSliderWidth div 2 - 1, 6);
	Canvas.DrawLine(p - FSliderWidth div 2 + 1,6, p - FSliderWidth div 2 + 1, h - FSliderWidth div 2);
	Canvas.DrawLine(p - FSliderWidth div 2 + 1, h - FSliderWidth div 2, p, h + FSliderWidth div 2 - 1);
	Canvas.SetColor(clShadow2);
	Canvas.DrawLine(p + FSliderWidth div 2, 6, p + FSliderWidth div 2, h - FSliderWidth div 2);
	Canvas.DrawLine(p + FSliderWidth div 2, h - FSliderWidth div 2, p + 1, h + FSliderWidth div 2 - 1);
	Canvas.SetColor(clShadow1);
	Canvas.DrawLine(p + FSliderWidth div 2 - 1, 7, p + FSliderWidth div 2 - 1, h - FSliderWidth div 2);	
	Canvas.DrawLine(p + FSliderWidth div 2 - 1, h - FSliderWidth div 2, p + 1, h + FSliderWidth div 2 - 2);	
    end;
    if Orientation = orVertical then
    begin
	h := Width div 2 - 1;
	Canvas.SetColor(clHilite1);
	Canvas.DrawLine(5,p - FSliderWidth div 2, 5, p + FSliderWidth div 2);
	Canvas.DrawLine(5,p - FSliderWidth div 2, h - FSliderWidth div 2, p - FSliderWidth div 2);
	Canvas.DrawLine( h - FSliderWidth div 2, p - FSliderWidth div 2, h + FSliderWidth div 2,p);
	Canvas.SetColor(clHilite2);
	Canvas.DrawLine(6,p - FSliderWidth div 2 + 1, 6, p + FSliderWidth div 2 - 1);
	Canvas.DrawLine(6,p - FSliderWidth div 2 + 1, h - FSliderWidth div 2, p - FSliderWidth div 2 + 1);
	Canvas.DrawLine(h - FSliderWidth div 2,p - FSliderWidth div 2 + 1, h + FSliderWidth div 2 - 1,p);
	Canvas.SetColor(clShadow2);
	Canvas.DrawLine( 6,p + FSliderWidth div 2, h - FSliderWidth div 2, p + FSliderWidth div 2);
	Canvas.DrawLine( h - FSliderWidth div 2,p + FSliderWidth div 2, h + FSliderWidth div 2 - 1, p + 1);
	Canvas.SetColor(clShadow1);
	Canvas.DrawLine( 7, p + FSliderWidth div 2 - 1, h - FSliderWidth div 2,p + FSliderWidth div 2 - 1);
	Canvas.DrawLine( h - FSliderWidth div 2, p + FSliderWidth div 2 - 1, h + FSliderWidth div 2 - 2, p + 1);
    end;
end;

procedure TwgTrackbar.HandlePaint;
var
    linepos : double;
    tmin, tmax : integer;
    drawwidth : integer;
    p : double;
    i : integer;
begin
    if FWinHandle = 0 then exit;
	Canvas.BeginDraw;
    if FFocused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
    Canvas.Clear(FBackgroundColor);
    Canvas.DrawRectangle(0,0,width,height);
    
    if FMax < FMin then	// change the values
    begin
	tmin := FMax;
	tmax := FMin;
    end
    else
    begin
	tmin := FMin;
	tmax := FMax;
    end;
    
    if FPosition < tmin then FPosition := tmin;
    if FPosition > tmax then FPosition := tmax;
    if Orientation = orHorizontal then
    begin
	linepos := tmax - tmin;
	drawwidth := Width - 5 - FSliderWidth;
	if linepos <> 0 then
	begin
	    linepos := drawwidth / linepos;
	    Canvas.SetColor(clWidgetFrame);
	    for i := 0 to tmax - tmin do
	    begin
		Canvas.DrawLine(round(2 + (FSliderWidth div 2) + (linepos * i)),height div 2 + FSliderWidth * 2,round(2 + FSliderWidth div 2 + linepos * i), Height - 5);
	    end;
	    DrawSlider(round(2 + FSliderWidth div 2 + linepos * position));
	end;
    end;
    
    if Orientation = orVertical then
    begin
	drawwidth := Height - 5 - FSliderWidth;
	linepos := tmax - tmin;
	if linepos <> 0 then
	begin
	    linepos := DrawWidth / linepos;
	    for i := 0 to tmax - tmin do
		Canvas.DrawLine(width div 2 + FSliderWidth * 2,round(2 + (FSliderWidth div 2) + (linepos * i)), Width - 5,round(2 + FSliderWidth div 2 + linepos * i));
	    DrawSlider(round(2 + FSliderWidth div 2 + linepos * position));
	end;
    end;
    Canvas.EndDraw;
end;

procedure TwgTrackbar.SetOrientation(aValue : TOrientation);
begin
    if aValue <> FOrientation then
    begin
	FOrientation := aValue;
	RePaint;
    end;
end;

constructor TwgTrackbar.Create(aOwner : TComponent);
begin
    inherited Create(aOwner);
    FFocusable := true;
    FMin := 0;
    FMax := 10;
    FPosition := 0;
    FSliderWidth := 11;
    FOrientation := orHorizontal;
    FBackgroundColor := clWindowBackground; //*clWidgetFrame;
end;

procedure TwgTrackbar.SetPosition(aValue : integer);
begin
    if aValue <> FPosition then
    begin
	FPosition := aValue;
	RePaint;
    end;
end;

procedure TwgTrackbar.SetMax(aValue : integer);
begin
    if aValue <> FMax then
    begin
	FMax := aValue;
	RePaint;
    end;
end;

procedure TwgTrackbar.SetMin(aValue : integer);
begin
    if aValue <> FMin then
    begin
	FMin := aValue;
	RePaint;
    end;
end;

end.
