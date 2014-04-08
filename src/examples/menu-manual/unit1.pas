unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

{$apptype console}
interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_form, lp_menu;

type

  { TlpForm1 }

  TlpForm1 = class(TlpForm)
  private
    { private declarations }
    lpMenuBar1: TlpMenuBar;
        { private declarations }
        FFileSubMenu, sub3,
        FFileSubMenu2: TlpPopupMenu;
  public
    { public declarations }
    procedure AfterCreate; override;
  end;

var
  lpForm1: TlpForm1;

implementation

{$R *.dfm}

{ TlpForm1 }

procedure TlpForm1.AfterCreate;
begin
  lpMenuBar1:= CreateMenuBar(self, 10,10,200,20);
    sub3 := TlpPopupMenu.Create(self);
    with sub3 do
    begin
      Name := 'Sub__3Menu';
      SetPosition(264, 60, 220, 20);
      AddMenuItem('&Open', 'Ctrl-O', nil);
      AddMenuItem('&Save', 'Ctrl-S', nil);
      AddMenuItem('S&ave As', 'Ctrl+Shift+S', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('Save && Reload', '', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('&Quit', 'Ctrl-Q', nil);
    end;

    FFileSubMenu := TlpPopupMenu.Create(self);
    with FFileSubMenu do
    begin
      Name := 'FFile_Menu';
      SetPosition(264, 60, 120, 20);
      AddMenuItem('&Open', 'Ctrl-O', nil);
      AddMenuItem('R&ecents...', 'Ctrl-S', nil).SubMenu := sub3;
      AddMenuItem('S&ave As', 'Ctrl+Shift+S', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('Save && Reload', '', nil);
      AddMenuItem('-', '', nil);
      AddMenuItem('&Quit', 'Ctrl-Q', nil);
    end;

    FFileSubMenu2 := TlpPopupMenu.Create(self);
    with FFileSubMenu2 do
    begin
      Name := 'Edit__mnu';
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
      Name := 'Help_MenuXZ';
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

