program edittest;

{.$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  pgf_defs, pgf_main, pgf_form, pgf_imgfmt_bmp,
  wglabel, wgbutton, wgedit, wgmemo;

type

  { TmyForm }

  TmyForm = class(TpgfForm)
  public
    label1 : TwgLabel;

    edit1, edit2 : TwgEdit;

    btn : TwgButton;

    memo : TwgMemo;

    procedure AfterCreate; override;
  end;

{ TmyForm }

procedure TmyForm.AfterCreate;
begin
  SetPosition(100,100,300,300);
  WindowTitle := 'PasGF Widget Test';

  edit1 := CreateEdit(self, 10,10, 120, 22);
  edit2 := CreateEdit(self, 10,40, 160, 22);

  btn := CreateButton(self, 10, 80, 90, 'Button', nil);

  memo := TwgMemo.Create(self);
  memo.SetPosition(10,120,250,80);
end;

procedure MainProc;
var
  frm : TmyForm;
begin
  Writeln('PasGF Widget test...');
  pgfOpenDisplay('');
  frm := TmyForm.Create(nil);
  frm.Show;
  pgfRunMessageLoop;
end;

begin
  MainProc;
end.
