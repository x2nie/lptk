unit Unit1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}


interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_form, lp_menu;

type
  TlpForm1 = class(TlpForm)
  private
    { private declarations }
    { private declarations }
    lpMenuBar1: TlpMenuBar;
        { private declarations }
        FFileSubMenu, sub3,
        FFileSubMenu2: TlpPopupMenu;
  public
    { public declarations }
    procedure AfterCreate; override;
    procedure MenuClick(Sender: TObject);
  end;

var
  lpForm1: TlpForm1;

implementation

{$R *.lfm}

procedure TlpForm1.AfterCreate;
begin
  lpMenuBar1:= CreateMenuBar(self, 10,10,200,20);
    sub3 := TlpPopupMenu.Create(self);
    with sub3 do
    begin
      Name := 'Sub__3Menu';
      SetPosition(264, 60, 220, 20);
      AddMenuItem('&Open', 'Ctrl+O', @MenuClick);
      AddMenuItem('&Save', 'Ctrl+S', @MenuClick);
      AddMenuItem('S&ave As', 'Ctrl+Shift+S', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('Save && Reload', '', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('&Quit', 'Ctrl+Q', @MenuClick);
    end;

    FFileSubMenu := TlpPopupMenu.Create(self);
    with FFileSubMenu do
    begin
      Name := 'FFile_Menu';
      SetPosition(264, 60, 120, 20);
      AddMenuItem('&Open', 'Ctrl+O', @MenuClick);
      AddMenuItem('R&ecents...', 'Ctrl+S', @MenuClick).SubMenu := sub3;
      AddMenuItem('S&ave As', 'Ctrl+Shift+S', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('Save && Reload', '', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('&Quit', 'Ctrl+Q', @MenuClick);
    end;

    FFileSubMenu2 := TlpPopupMenu.Create(self);
    with FFileSubMenu2 do
    begin
      Name := 'Edit__mnu';
      //SetPosition(264, 60, 120, 20);
      AddMenuItem('C&ut', 'Ctrl+x', @MenuClick);
      AddMenuItem('&Save && Close Copy', 'Ctrl+C', @MenuClick);
      AddMenuItem('Edit S&ave As', 'Ctrl+Shift+X', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('Save && Reload', '', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('Dui&t', 'Ctrl+T', @MenuClick);
    end;
    lpMenuBar1.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;

    FFileSubMenu := TlpPopupMenu.Create(self);
    with FFileSubMenu do
    begin
      Name := 'Help_MenuXZ';
      //SetPosition(264, 60, 120, 20);
      AddMenuItem('&Open', 'Ctrl+O', @MenuClick);
      AddMenuItem('&Save', 'Ctrl+S', @MenuClick);
      AddMenuItem('S&ave As', 'Ctrl+Shift+S', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('Save && Reload', '', @MenuClick);
      AddMenuItem('-', '', @MenuClick);
      AddMenuItem('&Quit', 'Ctrl+Q', @MenuClick);
    end;
    lpMenuBar1.AddMenuItem('&Eile', nil).SubMenu := FFileSubMenu;
    lpMenuBar1.AddMenuItem('V&ile', nil).SubMenu := FFileSubmenu2;
end;

procedure TlpForm1.MenuClick(Sender: TObject);
var s : widestring;
  k : Word; ss : TShiftState;
begin
  s := format('Menu %s |%s',[TfpgMenuItem(Sender).Text, ShortCutToText(TfpgMenuItem(Sender).ShortCut)]);
  writeln(s);
  //ShortCutToKey(TfpgMenuItem(Sender).ShortCut, k, ss);
  //writeln('Translated Shortcut=>>',ShortCutToText(k,ss));
end;

end.

