unit wgprogress;

{ feature-requests or bug-reports - mail to: erik@grohnwaldt.de
    History
    19.05.2003		nvitya: centering texts and rage check error bugfix at 100%
    18.05.2003	0.1	First Release
}

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ENDIF}

interface

uses gfxwidget, classes, gfxbase;

type
    TwgProgressbar = class(TWidget)
	private
	    FPosition : longint;
	    FMin : longint;
	    FMax : longint;
	    FStep : longint;
	    FFont : TgfxFont;
	    FDisplayPercent : boolean;
	    procedure SetPosition(aValue : longint);
	    procedure SetMin(aValue : longint);
	    procedure SetMax(aValue : longint);
	    procedure SetDisplayPercent(aValue : boolean);
	    procedure SetStep(aValue : longint);
	protected
	    procedure HandleResize(aWidth, aHeight : integer); override;
	public
	    constructor Create(aOwner : TComponent); override;

	    procedure RePaint; override;

	    procedure StepIt;
	    procedure StepBy(aStep : longint);

	    property DisplayPercent : boolean read FDisplayPercent write SetDisplayPercent;
	    property Max : longint read FMax write SetMax;
	    property Min : longint read FMin write SetMin;
	    property Position : longint read FPosition write SetPosition;
	    property Step : longint read FStep write SetStep;
    end;

implementation

uses
    gfxstyle, schar16, sysutils;

procedure TwgProgressbar.HandleResize(aWidth, aHeight : integer);
begin
    inherited HandleResize(aWidth, aHeight);
    RePaint;
end;

procedure TwgProgressbar.RePaint;
var
    aPos : longint;
    aG : longint;
    p : longint;
    percent : double;
    t : string16;
    r : TgfxRect;
    i : longint;
    y : integer;
begin
    if FWinHandle <= 0 then exit;
    inherited RePaint;
    Canvas.ClearClipRect;
    Canvas.Clear(BackgroundColor);
    Canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(0,0,width,height); // border for the box
    Canvas.SetColor(clSelection);
    // calculate position

    aG := Max - Min; // diff..
    aPos := Position - Min;	// absolute position

    percent := round(((100 / aG) * aPos));
    t := str8to16(inttostr(round(percent))+'%');
    p := round(percent * (Width-2) / 100);

    Canvas.FillRectangle(1, 1, p, height - 2);
    if FDisplayPercent then
    begin
	i := width - 2 - p - 1;
	y := Height div 2 - FFont.Height div 2;
	r.SetRect(p+1,1,width - 2 - p - 1, height - 2);
	Canvas.SetTextColor(clSelectionText);
	Canvas.DrawString16(Width div 2 - FFont.TextWidth16(t) div 2, y,t);
	if (i > 1) and (r.width > 0) then
	begin
	    Canvas.SetClipRect(r);
	    Canvas.SetTextColor(clInactiveSelText);
	    Canvas.DrawString16(Width div 2 - FFont.TextWidth16(t) div 2, y,t);
	end;
    end;
end;

procedure TwgProgressbar.StepBy(aStep : longint);
begin
    Position := Position + aStep;
end;

procedure TwgProgressbar.Stepit;
begin
    Position := Position + Step;
end;

procedure TwgProgressbar.SetStep(aValue : longint);
begin
    if aValue = 0 then exit; // 0 is no valid step-length
    if aValue <> FStep then
    begin
	FStep := aValue;
    end;
end;

procedure TwgProgressbar.SetDisplayPercent(aValue : boolean);
begin
    if aValue <> FDisplayPercent then
    begin
	FDisplayPercent := aValue;
	RePaint;
    end;
end;

procedure TwgProgressbar.SetMax(aValue : longint);
begin
    if FMin > aValue then FMin := aValue - 1;		// correct wrong inputs
    if FPosition > aValue then FPosition := aValue;
    if aValue <> FMax then
    begin
	FMax := aValue;
	RePaint;
    end;
end;

procedure TwgProgressbar.SetMin(aValue : longint);
begin
    if aValue > FPosition then FPosition := aValue;
    if aValue > FMax then FMax := aValue+1;
    if aValue <> FMin then
    begin
	FMin := aValue;
	RePaint;
    end;
end;

procedure TwgProgressbar.SetPosition(aValue : longint);
begin
    if aValue < Min then aValue := Min;	// correct wrong inputs
    if aValue > Max then aValue := Max;
    if aValue <> FPosition then
    begin
	FPosition := aValue;
	RePaint;
    end;
end;

constructor TwgProgressbar.Create(aOwner : TComponent);
begin
    inherited create(aOwner);
    BackgroundColor := clListBox;
    FDisplayPercent := false;
    FPosition := 0;
    FFocusable := false;
    FMax := 100;
    FFont := guistyle.LabelFont1;
    FMin := 0;
    FStep := 1;
end;

end.
