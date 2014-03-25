program trackbartest;

{$mode objfpc}

uses pgf_defs, pgf_main, pgf_form, wgtrackbar;

type
    TMainForm = class(TpgfForm)
        public
            trackbar : TwgTrackBar;
            procedure AfterCreate; override;
    end;

procedure TMainForm.AfterCreate;
begin
    Height := 250;
    Width := 200;
    inherited AfterCreate;
    WindowTitle := 'Trackbar-Test';
    trackbar := TwgTrackBar.create(self);
    trackbar.setPosition(10,10,180,180);
    trackbar.anchors := [anleft,anright,antop,anbottom];
    trackbar.Position := 10;
end;

var
    MainForm : TMainForm;

begin
    pgfOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    pgfRunMessageLoop;
    pgfCloseDisplay;
end.
