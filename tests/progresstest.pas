program progresstest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses gfxbase, wgprogress, gfxform, wgbutton, classes, schar16;


type
    TMainForm = class(TGfxForm)
        Bar : TwgProgressBar;
        Button : TwgButton;
        procedure ButtonClick(Sender : TObject);
        procedure AfterCreate; override;
    end;

procedure TMainForm.ButtonClick(Sender : TObject);
begin
    Bar.StepIt;
end;

procedure TMainForm.AfterCreate;
begin
    inherited AfterCreate;
    Width := 200;
    Height := 200;
    Bar := TwgProgressBar.Create(self);
    Bar.Position := 70;
    Bar.Top := 100;
    bar.anchors := [antop,anleft,anright,anbottom];
    Bar.Left := 10;
    Bar.Height := 30;
    Bar.Width := 150;
    Bar.DisplayPercent := true;
    Button := CreateButton(self,10,10,100,'Step bar',{$ifdef FPC}@{$endif}ButtonClick);
end;

var
    Form : TMainForm;

begin
    GfxOpenDisplay('');
    Form := TMainForm.Create(nil);
    Form.Show;
    GfxDoMessageLoop;
    GfxCloseDisplay;
end.
