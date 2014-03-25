program aligntest;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  pgf_defs, pgf_main, pgf_form, pgf_widget,
  wglabel;

type

  { TmyForm }

  TmyForm = class(TpgfForm)
  public
    ltop    : array[1..3] of TwgLabel;
    lbottom : array[1..3] of TwgLabel;
    lleft   : array[1..3] of TwgLabel;
    lright  : array[1..3] of TwgLabel;

    lclient : TwgLabel;

    AlignRect : TpgfRect;

    procedure AfterCreate; override;

  end;

{ TmyForm }

procedure TmyForm.AfterCreate;
var
  x,y : integer;
  n : integer;
begin
  x := 10;
  y := 10;
  for n:=low(ltop) to high(ltop) do
  begin
    ltop[n] := CreateLabel(self, x, y, 'alTop '+IntToStr(n));
    ltop[n].BackgroundColor := $FFFFFF;
    ltop[n].Align := alTop;
    ltop[n].Width := 100;
    inc(y,20);
  end;

  y := 280;
  for n:=low(lbottom) to high(lbottom) do
  begin
    lbottom[n] := CreateLabel(self, x, y, 'alBottom '+IntToStr(n));
    lbottom[n].BackgroundColor := $FFFFFF;
    lbottom[n].Align := alBottom;
    dec(y,20);
  end;

  y := 100;
  x := 10;
  for n:=low(lleft) to high(lleft) do
  begin
    lleft[n] := CreateLabel(self, x, y, 'L'+IntToStr(n));
    lleft[n].BackgroundColor := $FFFFFF;
    lleft[n].Align := alLeft;
    inc(x,30);
  end;

  x := 200;
  for n:=low(lright) to high(lright) do
  begin
    lright[n] := CreateLabel(self, x, y, 'R'+IntToStr(n));
    lright[n].BackgroundColor := $FFFFFF;
    lright[n].Align := alRight;
    dec(x,30);
  end;

  lclient := CreateLabel(self, 150, 150, 'alClient');
  lclient.BackgroundColor := $0F0FFF;
  lclient.Align := alClient;
end;

procedure MainProc;
var
  frm : TmyForm;
begin
  Writeln('PasGF Align test...');
  pgfOpenDisplay('');
  frm := TmyForm.Create(nil);
  frm.Left := 100;
  frm.Top := 100;
  frm.Width := 300;
  frm.Height := 300;

  //frm.Sizeable := false;
  frm.MinWidth  := 200;
  frm.MinHeight := 150;
  frm.WindowTitle := 'PasGF Align Test';
  frm.Show;

  pgfRunMessageLoop;
end;

begin
  MainProc;
end.
