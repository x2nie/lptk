program tabtest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses gfxbase, gfxstyle, wgtab, gfxform, schar16, wgbutton, wgedit;

type
    TMainForm = class(TgfxForm)
	control : TwgTabControl;
	sheet1 : TwgTabSheet;
	sheet2 : TwgTabSheet;
	sheet3 : TwgTabSheet;
	Button1 : TwgButton;
	Button2 : TwgButton;
	Button3 : TwgButton;
        edit1   : TwgEdit;
        edit2   : TwgEdit;
	procedure OnClick1(Sender : TObject);
	procedure OnClick2(Sender : TObject);
	procedure AfterCreate; override;
    end;

procedure TMainForm.OnClick1(Sender : TObject);
begin
    control.ActiveTabSheet := sheet2;
end;

procedure TMainForm.OnClick2(Sender : TObject);
begin
    control.ActiveTabSheet := sheet1;
end;

procedure TMainForm.AfterCreate;
begin
    width := 500;
    height := 500;
    WindowPosition := wpAuto;
    inherited AfterCreate;
    control := TwgTabControl.Create(self);
    control.Anchors := [anleft,anright,antop,anbottom];
    control.Height := 300;
    control.Width := 300;
    control.top := 10;
    control.left := 10;
    sheet1 := TwgTabSheet.Create(control);
    sheet1.index := 20;
    sheet1.Text := Str8to16('Testsheet');
    Button1 := TwgButton.Create(sheet1);
    Button1.Text := Str8To16('Testbutton1');
    Button1.SetDimensions(10,10,200,-1);
    Button1.OnClick := {$IFDEF fpc}@{$ENDIF}OnClick1;

    Button3 := TwgButton.Create(sheet1);
    Button3.Text := Str8To16('Testbutton3');
    Button3.SetDimensions(10,80,200,-1);
    
    edit1 := CreateEdit(sheet1, 10,130, 150, 0);

    sheet2 := TwgTabSheet.Create(control);
    sheet2.Text := Str8to16('Testsheet1');
    sheet2.index := 5;

    sheet3 := TwgTabSheet.Create(control);
    sheet3.Text := Str8to16('Testsheet 2 mit noch mehr');

    Button2 := TwgButton.Create(sheet2);
    Button2.Text := Str8To16('Testbutton2');
    Button2.SetDimensions(10,10,200,40);
    Button2.OnClick := {$IFDEF fpc}@{$ENDIF}OnClick2;
    
    edit2 := CreateEdit(sheet2, 10,130, 150, 0);
    
    CreateEdit(self, 10, 450, 150, 0);

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
