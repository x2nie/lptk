program menutest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses SysUtils, Classes, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  popupwindow, gfxmenu, wgedit;

type

  TMainForm = class(TGfxForm)
  public

    mbar : TMenuBar;

    pmenu : TPopupMenu;

    subm, subsubm : TPopupMenu;

    sub2, sub3 : TPopupMenu;

    ed : TwgEdit;

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

  pmenu := TPopupMenu.Create(self);
  pmenu.AddMenuItem8('This is a &very long && menu item','',nil);
  pmenu.AddMenuItem8('-','',nil);
  pmenu.AddMenuItem8('First is invisible','',{$ifdef FPC}@{$endif}MenuSelect).Visible := false;
  pmenu.AddMenuItem8('&Second...','CTRL-X',{$ifdef FPC}@{$endif}MenuSelect);
  pmenu.AddMenuItem8('&Third is disabled','',{$ifdef FPC}@{$endif}MenuSelect).Enabled := false;
  pmenu.AddMenuItem8('Unicode: bend&^337','',{$ifdef FPC}@{$endif}MenuSelect);
  pmenu.AddMenuItem8('-','',nil);
  subm := TPopupMenu.Create(self);
  pmenu.AddMenuItem8('Sub&Menu','',nil).SubMenu := subm;
    subm.AddMenuItem8('Sub1','',nil);
    subm.AddMenuItem8('Sub2','',nil);
    subsubm := TPopupMenu.Create(self);
    subm.AddMenuItem8('Sub&3','',nil).SubMenu := subsubm;
      subsubm.AddMenuItem8('SubSub1','',nil);
      subsubm.AddMenuItem8('SubSub2','',nil);
  pmenu.AddMenuItem8('-','',nil);
  sub2 := TPopupMenu.Create(self);
  pmenu.AddMenuItem8('SubMenu2','',nil).SubMenu := sub2;
    sub2.AddMenuItem8('Sub1','',nil);
    sub2.AddMenuItem8('Sub2','',nil);
  pmenu.AddMenuItem8('-','',nil);
  pmenu.AddMenuItem8('E&xit','CTRL-Q', {$ifdef FPC}@{$endif}MenuExit);

  mbar := TMenuBar.Create(self);
  mbar.SetDimensions(2,2,220,30);
  mbar.AddMenuItem8('&First',nil).SubMenu := pmenu;
  sub3 := TPopupMenu.Create(self);
  mbar.AddMenuItem8('Se&cond',nil).SubMenu := sub3;
    sub3.AddMenuItem8('Menu3 - 1','',nil);
    sub3.AddMenuItem8('Menu3 - 2','',nil);
  mbar.AddMenuItem8('Thir&d',nil);


  ed := CreateEdit(self, 20,50, 80,0)

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

