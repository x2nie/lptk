unit lp_progressbar;

{ feature-requests or bug-reports - mail to: erik@grohnwaldt.de
    History
    19.05.2003		nvitya: centering texts and rage check error bugfix at 100%
    18.05.2003	0.1	First Release
    13.09.2012 migrated to PasGF by Preben Bjørn Madsen - corrected division by zero in line 92 - 93
}


{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses classes, sysutils, lp_defs, lp_main, lp_widget;

type
  TlpProgressbar = class(TpgfWidget)
	private
	    FPosition : longint;
	    FMin : longint;
	    FMax : longint;
	    FStep : longint;
	    FFont : TpgfFont;
	    FDisplayPercent : boolean;
	    procedure SetPosition(aValue : longint);
	    procedure SetMin(aValue : longint);
	    procedure SetMax(aValue : longint);
	    procedure SetDisplayPercent(aValue : boolean);
	    procedure SetStep(aValue : longint);
	protected
	    FBackgroundColor: TpgfColor;
	    procedure HandleResize(aWidth, aHeight : integer); override;
	public
	    constructor Create(aOwner : TComponent); override;

	    procedure HandlePaint; override;

	    procedure StepIt;
	    procedure StepBy(aStep : longint);
  published
	    property DisplayPercent : boolean read FDisplayPercent write SetDisplayPercent;
	    property Max : longint read FMax write SetMax;
	    property Min : longint read FMin write SetMin;
	    property Position : longint read FPosition write SetPosition;
	    property Step : longint read FStep write SetStep;
    end;

function CreateProgressbar(AOwner : TComponent; x, y, Width, Height : TpgfCoord) : TlpProgressbar;

implementation

function CreateProgressbar(AOwner : TComponent; x, y, Width, Height: TpgfCoord) : TlpProgressbar;
begin
  Result := TlpProgressbar.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TlpProgressbar.HandleResize(aWidth, aHeight : integer);
begin
    inherited HandleResize(aWidth, aHeight);
    RePaint;
end;

procedure TlpProgressbar.HandlePaint;
var
    aPos : longint;
    aG : longint;
    p : longint;
    percent : double;
    t : WideString;
    r : TpgfRect;
    i : longint;
    y : integer;
begin
    if FWinHandle <= 0 then exit;
    inherited HandlePaint;
	Canvas.BeginDraw;
    Canvas.ClearClipRect;
    Canvas.Clear(FBackgroundColor);
    Canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(0,0,width,height); // border for the box
    Canvas.SetColor(clSelection);
    // calculate position

    aG := Max - Min; // diff..
    aPos := Position - Min;	// absolute position

    if aG = 0 then aG := 1; //* pbm added to avoid division by zero
    percent := round(((100 / aG) * aPos));
    t := u8(inttostr(round(percent))+'%');
    p := round(percent * (Width-2) / 100);

    Canvas.FillRectangle(1, 1, p, height - 2);
    if FDisplayPercent then
    begin
	i := width - 2 - p - 1;
	y := Height div 2 - FFont.Height div 2;
	r.SetRect(p+1,1,width - 2 - p - 1, height - 2);
	Canvas.SetTextColor(clSelectionText);
	Canvas.DrawString(Width div 2 - FFont.TextWidth(t) div 2, y,t);	Canvas.DrawString(Width div 2 - FFont.TextWidth(t) div 2, y,t);
	if (i > 1) and (r.width > 0) then
	begin
	    Canvas.SetClipRect(r);
	    Canvas.SetTextColor(clInactiveSelText);
	    Canvas.DrawString(Width div 2 - FFont.TextWidth(t) div 2, y,t);
	end;
    end;
    Canvas.EndDraw;
end;

procedure TlpProgressbar.StepBy(aStep : longint);
begin
    Position := Position + aStep;
end;

procedure TlpProgressbar.Stepit;
begin
    Position := Position + Step;
end;

procedure TlpProgressbar.SetStep(aValue : longint);
begin
    if aValue = 0 then exit; // 0 is no valid step-length
    if aValue <> FStep then
    begin
	FStep := aValue;
    end;
end;

procedure TlpProgressbar.SetDisplayPercent(aValue : boolean);
begin
    if aValue <> FDisplayPercent then
    begin
	FDisplayPercent := aValue;
	RePaint;
    end;
end;

procedure TlpProgressbar.SetMax(aValue : longint);
begin
    if FMin > aValue then FMin := aValue - 1;		// correct wrong inputs
    if FPosition > aValue then FPosition := aValue;
    if aValue <> FMax then
    begin
	FMax := aValue;
	RePaint;
    end;
end;

procedure TlpProgressbar.SetMin(aValue : longint);
begin
    if aValue > FPosition then FPosition := aValue;
    if aValue > FMax then FMax := aValue+1;
    if aValue <> FMin then
    begin
	FMin := aValue;
	RePaint;
    end;
end;

procedure TlpProgressbar.SetPosition(aValue : longint);
begin
    if aValue < Min then aValue := Min;	// correct wrong inputs
    if aValue > Max then aValue := Max;
    if aValue <> FPosition then
    begin
	FPosition := aValue;
	RePaint;
    end;
end;

constructor TlpProgressbar.Create(aOwner : TComponent);
begin
    inherited create(aOwner);
    FBackgroundColor := clWindowBackground;
    FDisplayPercent := false;
    FPosition := 0;
    FFocusable := false;
    FMax := 100;
    FFont := pgfGetFont('#Label1');
    FMin := 0;
    FStep := 1;
    width := 100;
    height := 20;
end;

end.
