program menutest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses SysUtils, Classes, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  popupwindow, gfxmenu;

type

  TMainForm = class(TGfxForm)
  public
  
    pmenu : TPopupMenu;
  
    procedure AfterCreate; override;
    
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    
    procedure MenuExit(sender : TObject);
    
    procedure MenuSelect(sender : TObject);
    
  end;


procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  
  Height := 150;
  Width := 300;

  WindowTitle8 := 'Menu test';

  pmenu := TPopupMenu.Create(nil);

  pmenu.AddMenuItem8('This is a &very long && menu item','',nil);
  pmenu.AddMenuItem8('-','',nil);
  pmenu.AddMenuItem8('First is invisible','',{$ifdef FPC}@{$endif}MenuSelect).Visible := false;
  pmenu.AddMenuItem8('&Second...','CTRL-X',{$ifdef FPC}@{$endif}MenuSelect);
  pmenu.AddMenuItem8('&Third is disabled','',{$ifdef FPC}@{$endif}MenuSelect).Enabled := false;
  pmenu.AddMenuItem8('Unicode: bend&^337','',{$ifdef FPC}@{$endif}MenuSelect);
  pmenu.AddMenuItem8('-','',nil);
  pmenu.AddMenuItem8('E&xit','CTRL-Q', {$ifdef FPC}@{$endif}MenuExit);
end;

procedure TMainForm.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited;
  
  if button = 1 then Exit;
  
  writeln('Right click');
  
  pmenu.ShowAt(self.WinHandle,x,y);
  
end;

procedure TMainForm.MenuExit(sender: TObject);
begin
  Halt(0);
end;

procedure TMainForm.MenuSelect(sender: TObject);
begin
  Writeln('Selected menu: ',u16u8(TmenuItem(sender).Text));
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
