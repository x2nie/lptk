program
    filedialogtest;
{$IFDEF fpc}{$mode objfpc}
{$else}
{$apptype CONSOLE}
{$ENDIF}

uses
    gfxbase, classes, gfxform, wgfiledialog, wgbutton;
    
type TMainForm = class(TgfxForm)
           FFileDialog : TwgFileDialog;
           FButton : TwgButton;
           procedure AfterCreate; override;
           procedure OnClick(ASender : TObject);
     end;

procedure TMainForm.OnClick(ASender : TObject);
begin
     FFileDialog := TwgFileDialog.Create(nil);
     FFileDialog.WindowTitle8 := 'Open file';
     FFileDialog.Execute;
     FFileDialog.Free;
end;

procedure TMainForm.AfterCreate;
begin
     SetDimensions(0,0,100,100);
     FButton := CreateButton(self, 10,10,80, 'Filedialog',{$IFDEF FPC}@{$ENDIF}OnClick);
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
