unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_form, lp_button, lp_progressbar,
  lp_trackbar, lp_memo, lp_edit;

type

  { Tfrm1 }

  Tfrm1 = class(TlpForm)
    lpButton1: TlpButton;
    lpEdit1: TlpEdit;
    lpProgressbar1: TlpProgressbar;
    lpTimer1: TlpTimer;
    lpTrackbar1: TlpTrackbar;
    procedure lpButton1Click(Sender: TObject);
    procedure lpTimer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm1: Tfrm1;

implementation

{$R *.lfm}

{ Tfrm1 }

procedure Tfrm1.lpButton1Click(Sender: TObject);
begin

end;

procedure Tfrm1.lpTimer1Timer(Sender: TObject);
begin
  lpProgressbar1.StepIt;
  if lpProgressbar1.Position >= lpProgressbar1.Max then
     lpProgressbar1.Position:= 0;
end;

end.

