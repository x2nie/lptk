program caltest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses sysutils, unitkeys,messagequeue, schar16, gfxbase, gfxstyle, gfxform, wgcalendar;

type
    TMainForm = class(TGfxForm)
      public
	calendar : TwgCalendar;
	procedure AfterCreate; override;
    end;

var
    MainForm : TGfxForm;

procedure DoHilite(Sender : TObject; Datum : TDateTime; var Hilite : boolean);
var
    d,m,y : word;
begin
    decodedate(datum,y,m,d);
    Hilite := (m = 5) and (d=18);
end;

procedure TMainForm.AfterCreate;
begin
    inherited;
    SetDimensions(500,10,400,400);
    WindowTitle8 := 'TestTitle';
    calendar := TwgCalendar.Create(self);
    calendar.top := 100;
    calendar.left := 10;
    calendar.height := 200;
    calendar.width := 200;
    calendar.anchors := [anLeft,anBottom, anRight, anTop];
    calendar.OnDrawDay := @DoHilite;
end;


begin
    GfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    GfxDoMessageLoop;    
    GfxCloseDisplay;    
end.

