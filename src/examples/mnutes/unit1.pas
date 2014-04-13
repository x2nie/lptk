unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_form, lp_menu;

type

  { TlpForm1 }

  TlpForm1 = class(TlpForm)
    lpMenuBar1: TlpMenuBar;
  private
    { private declarations }
    FFileSubMenu: TlpPopupMenu;
    FFileSubMenu2: TlpPopupMenu;
  public
    { public declarations }
    procedure AfterCreate; override;
  end;

var
  lpForm1: TlpForm1;

implementation

{$R *.lfm}

{ TlpForm1 }

procedure TlpForm1.AfterCreate;
begin
  FFileSubMenu := TlpPopupMenu.Create(self);
  with FFileSubMenu do
  begin
    Name := 'FFileSubMenu';
    SetPosition(264, 60, 120, 20);
    AddMenuItem('&Open', 'Ctrl-O', nil);
    AddMenuItem('&Save', 'Ctrl-S', nil);
    AddMenuItem('S&ave As', 'Ctrl+Shift+S', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl-Q', nil);
  end;

  FFileSubMenu2 := TlpPopupMenu.Create(self);
    with FFileSubMenu2 do
  begin
    Name := 'FFileSubMenu2';
    //SetPosition(264, 60, 120, 20);
    AddMenuItem('C&ut', 'Ctrl-x', nil);
    AddMenuItem('&Save && Close Copy', 'Ctrl-C', nil);
    AddMenuItem('Edit S&ave As', 'Ctrl+Shift+X', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Dui&t', 'Ctrl-T', nil);
  end;
  lpMenuBar1.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;

  FFileSubMenu := TlpPopupMenu.Create(self);
  with FFileSubMenu do
  begin
    Name := 'FFileSubMenuXZ';
    //SetPosition(264, 60, 120, 20);
    AddMenuItem('&Open', 'Ctrl-O', nil);
    AddMenuItem('&Save', 'Ctrl-S', nil);
    AddMenuItem('S&ave As', 'Ctrl+Shift+S', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl-Q', nil);
  end;
  lpMenuBar1.AddMenuItem('&Eile', nil).SubMenu := FFileSubMenu;
  lpMenuBar1.AddMenuItem('V&ile', nil).SubMenu := FFileSubmenu2;
end;

end.

