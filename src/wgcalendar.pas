{
    feature-requests or bugs? - mail to: erik@grohnwaldt.de
    History
    14.05.2003	0.2	Some bugs fixed... order in class declaration delphi-conform
    07.05.2003	0.1	first release
}

unit
    wgcalendar;

{$IFDEF FPC}
    {$mode objfpc}{$H+}
{$ENDIF}

// {$DEFINE DEBUG}
interface

uses classes, sysutils, gfxbase, gfxwidget, schar16, wgbutton, wglabel;

const DayName : array[1..7] of string = ('Mo','Di','Mi','Do','Fr','Sa','So');
      MonthName : array[1..12] of string = ('Jan.','Feb.','Maerz','April','Mai','Juni','Juli','Aug.','Sept.','Okt.','Nov.','Dez.');
type
    TCalendarHilight = procedure(Sender : TObject; Date : TDateTime; var hilite : boolean);
    TCalendarChange = procedure(Sender : TObject; NewDate : TDateTime);

    TwgCalendar = class(TWidget)
      private
	FFont : TgfxFont;

	FDate : TDateTime;
	FActiveMonth : word;
	FActiveYear : word;
	FMargin : word;
	ButtonL : TwgButton;
	ButtonR : TwgButton;
	DateLabel : TwgLabel;

	procedure SetFont(aValue : TgfxFont);
	procedure SetDate(aValue : TDateTime);
	procedure SetDay(aValue : byte);
	function GetDay : byte;
	function GetMonth : byte;
	procedure SetMonth(aValue : byte);
	function GetYear : word;
	procedure SetYear(aValue : word);
	procedure ButtonLClick(Sender : TObject);
	procedure ButtonRClick(Sender : TObject);
	procedure DoChange(NewDate : TDateTime);

	procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
	procedure HandleKeyPress(var KeyCode : word; var Shiftstate : word; var consumed : boolean); override;
	procedure HandleReSize(dwidth, dheight : integer); override;
        procedure CalcButtonLabel;
      public
	onDrawDay : TCalendarHilight;  // to select holy days.... returns true: day hilited
	onChange : TCalendarChange;

        constructor Create(aOwner : TComponent); override;

        procedure RePaint; override;
        procedure DoShow; override;
        property Font : TgfxFont read FFont write SetFont;

	property Date : TDateTime read FDate write SetDate;
	property Day : byte read GetDay write SetDay;
	property Month : byte read GetMonth write SetMonth;
	property Year : word read GetYear write SetYear;

    end;

implementation

uses
    gfxstyle;

procedure TwgCalendar.DoChange(NewDate : TDateTime);
begin
    {$IFDEF DEBUG}writeln('DoChange');{$ENDIF}
    if Assigned(onChange) then OnChange(self, NewDate);
end;

procedure TwgCalendar.HandleResize(dwidth, dheight : integer);
begin
    inherited HandleResize(dwidth, dheight);
    calcButtonLabel;
    RePaint;
end;

procedure TwgCalendar.HandleKeyPress(var KeyCode : word; var shiftstate : word; var consumed : boolean);
var
    OldDate : TDateTime;
begin
    OldDate := Date;
    consumed := true;
    case KeyCode of
	KEY_UP : begin
	    Date := Date - 7;
	end;
	KEY_DOWN : begin
	    Date := Date + 7;
	end;
	KEY_LEFT: begin
	    Date := Date - 1;
	end;
	KEY_RIGHT : begin
	    Date := Date + 1;
	end;
	KEY_PGUP : begin
	    ButtonLClick(self);
	end;
	KEY_PGDN : begin
	    ButtonRClick(self);
	end;
	else Consumed := false;
    end;
    if OldDate <> Date then DoChange(date);
  if not Consumed then inherited HandleKeyPress(KeyCode, ShiftState, consumed);
  RePaint;
end;

procedure TwgCalendar.HandleMouseUp(x,y : integer; button : word; shiftstate : word);
var
    xp : integer;
    yp : integer;
    i, i1 : integer;
    start : TDateTime;
    OldDate : TDateTime;
begin
    inherited HandleMouseUp(x,y,button,shiftstate);
    {$IFDEF DEBUG}writeln('HandleMouseUp');{$ENDIF}
    OldDate := Date;
    yp := Height - ButtonL.Height - ButtonL.Top - FMargin;
    xp := Width - FMargin * 2;
    xp := xp div 7;
    yp := yp div 7;
    {$IFDEF DEBUG}writeln('XP: ',xp); writeln('YP: ',yp);{$ENDIF}
    x := x - FMargin * 2;
    y := y - ButtonL.Height - ButtonL.Top - FMargin;
    y := y div yp;
    x := x div xp;
    {$IFDEF DEBUG}writeln('X: ',x); writeln('Y: ',y);{$ENDIF}
    if DayOfWeek(encodeDate(Year, Month, 1)) = 1 then
	start := encodeDate(Year, Month, 1) - 6
    else start := EncodeDate(Year, Month, 1) - DayOfWeek(encodeDate(Year, Month, 1)) + 2;
    if y >= 1 then	// die tagesnamen
    begin
	for i := 1 to 6 do
	    for i1 := 1 to 7 do
	    begin
		if (i = y) and (i1 = x + 1) then
		begin
		    FDate := start;
		    if OldDate <> Date then
		    begin
			RePaint;
			DoChange(Date);
		    end;
		    exit;
		end;
		start := start + 1;
	    end;
    end;
    RePaint;
end;

procedure TwgCalendar.calcButtonLabel;
var
    i : integer;
    i1 : integer;
begin
    ButtonR.Width := ButtonR.Height;
    ButtonL.Width := ButtonL.Height;
    ButtonL.SetDimensions(FMargin,FMargin,ButtonL.Width, ButtonL.Height);
    ButtonR.SetDimensions(Width - FMargin - ButtonR.Width, FMargin, ButtonL.Width, ButtonL.Height);
    i := DateLabel.Font.TextWidth16(DateLabel.Text);
    i1 := DateLabel.Font.Ascent + Datelabel.Font.Descent;
    DateLabel.SetDimensions(Width div 2 - i div 2,ButtonL.Height div 2 - i1 div 2, i,i1);
end;

procedure TwgCalendar.DoShow;
begin
    inherited DoShow;
    calcButtonLabel;
end;

procedure TwgCalendar.ButtonLClick(Sender : TObject);
var
    y,m,d : word;
    done : boolean;
begin
    {$DEFINE DEBUG}writeln('ButtonLClick');
    DecodeDate(date, y,m,d);
    if m = 1 then
    begin
	m := 12;
	dec(y);
    end
    else
	dec(m);
    done := true;
    repeat
      try
	date := EncodeDate(y,m,d);
	done := true;
      except
	dec(d);
	done := false;
      end;
    until done;
    RePaint;
end;

procedure TwgCalendar.ButtonRClick(Sender : TObject);
var
    y,m,d : word;
    done : boolean;
begin
    DecodeDate(date, y,m,d);
    if m = 12 then
    begin
	inc(y);
	m := 1;
    end
    else inc(m);
    repeat
      try
	done := true;
	Date := EncodeDate(y,m,d);
      except
	dec(d);
	done := false;
      end;
    until done;
    RePaint;
end;

procedure TwgCalendar.RePaint;
var
    i, i1 : integer;
    r : TgfxRect;
    start : TDateTime;
    d,m,y : word;
    cw : integer;
    black : boolean;
    hilite : boolean;
    rh : integer;
begin
    if not Windowed then exit;
//    inherited RePaint;
    Canvas.ClearClipRect;
    Canvas.Clear(FBackgroundColor);
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.FillRectangle(ButtonL.Left + ButtonL.Width, FMargin, Width - ButtonL.Width * 2 - FMargin * 2, ButtonL.Height);
    decodedate(date,y,m,d);
    DateLabel.Text := Str8To16(MonthName[m]+' '+inttostr(y));
    i := DateLabel.Font.TextWidth16(DateLabel.Text);
    i1 := DateLabel.Font.Ascent + Datelabel.Font.Descent;
    DateLabel.SetDimensions(Width div 2 - i div 2,ButtonL.Height div 2 - i1 div 2, i,i1);

    if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0,0,Width,Height);
    r.SetRect(FMargin,ButtonL.Height + FMargin, Width - 2*FMargin, Height - ButtonL.Height - 2 * FMargin);
    Canvas.SetClipRect(r);
    if DayOfWeek(encodeDate(Year, Month, 1)) = 1 then
	      start := encodeDate(Year, Month, 1) - 6
    else start := EncodeDate(Year, Month, 1) - DayOfWeek(encodeDate(Year, Month, 1)) + 2;

    decodedate(start,y,m,d);
    if m <> month then black := false;
    cw := (Width - 2 * FMargin) div 7; // column width
    rh := Height - ButtonL.Height - ButtonL.Top - FMargin;
    rh := rh div 7;
    Canvas.SetTextColor(clText1);

    for i := 1 to 7 do
	// kopfzeilen und spalten zeichnen
    begin
	    Canvas.DrawString16(cw * i - cw div 2 - FFont.TextWidth16(Str8To16(DayName[i])) div 2,ButtonL.Height + ButtonL.Top,Str8To16(DayName[i]));
    end;
    for i := 1 to 6 do	// 6 zeilen a 7 spalten
    begin		// beginnt mit spalte
	    for i1 := 1 to 7 do
	    begin
	      decodedate(start,y,m,d);

	      if Assigned(onDrawDay) then OnDrawDay(self, start, hilite)
	      else Hilite := false;  // check if day should be hilited
        if d = 1 then black := not Black;
	      if black then Canvas.SetTextColor(clText1) else Canvas.SetTextColor(clInactiveWgFrame);
	      if hilite then
	      begin
		      Canvas.SetColor(clInactiveSel);
		      Canvas.FillRectangle(cw * (i1 - 1),  ButtonL.Height + ButtonL.Top + (rh) * (i) , cw - 1, rh);
		      Canvas.SetTextColor(clInactiveSelText);
	      end;
	      if date = start then
	      begin
		      if Focused then
		      begin
		        Canvas.SetColor(clSelection);
		        Canvas.SetTextColor(clSelectionText);
		      end
		      else
		      begin
		        Canvas.SetColor(clInactiveSel);
		        Canvas.SetTextColor(clInactiveSelText);
		      end;
		      Canvas.FillRectangle(cw * (i1 - 1),  ButtonL.Height + ButtonL.Top + (rh) * (i) , cw - 1, rh);
	      end;
	      Canvas.DrawString16(cw * i1 - cw div 2 - FFont.TextWidth16(Str8To16(IntToStr(d))) div 2, ButtonL.Height + ButtonL.Top + rh * (i) + rh div 2 - FFont.Height div 2, Str8To16(IntToStr(d)));
	      start := start + 1;
	    end;
    end;
end;

procedure TwgCalendar.SetYear(aValue : word);
var
    d,m,y : word;
begin
    try
	decodedate(date,y,m,d);
	if aValue <> y then
	begin
	    FDate := encodedate(aValue,m,d);
	    RePaint;
	    DoChange(FDate);
	end;
    except
	on e : exception do
	begin
	end;
    end;
end;

function TwgCalendar.GetYear : word;
var
    d,m,y : word;
begin
    decodedate(date,y,m,d);
    result := y;
end;

function TwgCalendar.GetMonth : byte;
var
    d,m,y : word;
begin
    DecodeDate(date,y,m,d);
    result := m;
end;

procedure TwgCalendar.SetMonth(aValue : byte);
var
    d,m,y : word;
begin
    try
	decodedate(date,y,m,d);
	if aValue <> m then
	begin
	    FDate := encodedate(y,aValue,d);
	    RePaint;
	    DoChange(FDate);
	end;
    except
	on e : exception do
	begin
	end;
    end;
end;

constructor TwgCalendar.Create(aOwner : TComponent);
var
    i : word;
    d,m,y : word;
begin
    inherited Create(aOwner);
    FDate := trunc(Now);
    DecodeDate(FDate,FActiveYear, FActiveMonth, i);
    ButtonL := TwgButton.Create(self);
    ButtonL.Width := 25;
    ButtonL.Text := Str8To16('<');
    ButtonL.OnClick := {$IFDEF fpc}@{$ENDIF}ButtonLClick;
    ButtonR := TwgButton.Create(self);
    FMargin := 1;
    ButtonR.Width := 25;
    ButtonR.Text := Str8To16('>');
    ButtonR.OnClick := {$IFDEF fpc}@{$ENDIF}ButtonRClick;
    FBackgroundColor := clListBox;
    DateLabel := TwgLabel.Create(self);
    decodedate(date,y,m,d);
    DateLabel.Text := Str8To16(MonthName[m]+' '+IntToStr(y));
    DateLabel.BackgroundColor := clInactiveWgFrame;
    FFont := guistyle.LabelFont1;
    Focusable := true;
end;

function TwgCalendar.GetDay : byte;
var
    d,m,y : word;
begin
    try
	decodedate(date,y,m,d);
	result := d;
    except
	on e : exception do
	begin
	    decodedate(date, y,m,d);
	    result := d;
	end;
    end;
end;

procedure TwgCalendar.SetDay(aValue : byte);
var
    d,m, y : word;
begin
    try
	decodedate(date,y,m,d);
	if aValue <> d then
	begin
	    FDate := encodedate(y,m,aValue);
	    RePaint;
	    DoChange(FDate);
	end;
    except
	// bei einer exception braucht nichts gemacht zu werden, das datum wurde ja nicht veraendert
	on e : exception do
	begin
	end;
    end;
end;

procedure TwgCalendar.SetDate(aValue : TDateTime);
begin
    if aValue <> FDate then
    begin
	FDate := trunc(aValue);
	if FDate = 0 then FDate := trunc(now);
	RePaint;
    end;
end;

procedure TwgCalendar.SetFont(aValue : TgfxFont);
begin
    if FFont <> aValue then
    begin
	FFont := aValue;
	RePaint;
    end;
end;

end.