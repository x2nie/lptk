program trackbartest;

{$mode objfpc}

uses gfxbase, gfxform, wgtrackbar;

type
    TMainForm = class(TGfxForm)
        public
            trackbar : TwgTrackBar;
            procedure AfterCreate; override;
    end;

procedure TMainForm.AfterCreate;
begin
    Height := 250;
    Width := 200;
    inherited AfterCreate;
    WindowTitle8 := 'Trackbar-Test';
    trackbar := TwgTrackBar.create(self);
    trackbar.setdimensions(10,10,180,180);
    trackbar.anchors := [anleft,anright,antop,anbottom];
    trackbar.Position := 10;
end;

var
    MainForm : TMainForm;

begin
    gfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    gfxDoMessageLoop;
    gfxCloseDisplay;
end.
