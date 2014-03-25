program widgettest;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  pgf_defs, pgf_main, pgf_form, pgf_imgfmt_bmp,
  wglabel, wgbutton, wgedit, wgmemo, wgcheckbox, wgradio, 
  wgbevel, wglistbox, wgchoicelist;

type

  { TmyForm }

  TmyForm = class(TpgfForm)
  public
    label1 : TwgLabel;

    edit1, edit2 : TwgEdit;

    btn : TwgButton;

    memo : TwgMemo;
    
    cb : TwgCheckBox;
    
    rb1, rb2 : TwgRadioButton;
    rb3, rb4 : TwgRadioButton;

    bv  : TwgBevel;
    lb : TwgTextListBox;

    cmb : TwgChoiceList;

    procedure AfterCreate; override;

    procedure ButtonClick(Sender : TObject);
  end;

{ TmyForm }

procedure TmyForm.AfterCreate;
begin
  SetPosition(100,100,300,520);
  WindowTitle := 'PasGF Widget Test';

  label1 := CreateLabel(self, 10,16, 'Simple Label');

  edit1 := CreateEdit(self, 10,40, 120, 22);
  edit1.text := 'Edit box';

  btn := CreateButton(self, 10, 70, 90, 'Button', ButtonClick);

  memo := TwgMemo.Create(self);
  memo.SetPosition(10,110,250,80);
  memo.text := 'memo - Multiline text editor';
  
  cb := CreateCheckBox(self, 10, 200, 'Check Box');
  
  rb1 := CreateRadioButton(self, 10,240, 'Radio 1');
  rb2 := CreateRadioButton(self, 100,240, 'Radio 2');
  
  rb3 := CreateRadioButton(self, 10,270, 'Radio 3');
  rb3.GroupIndex := 1;
  rb4 := CreateRadioButton(self, 100,270, 'Radio 4');
  rb4.GroupIndex := 1;
 
//*  bv := CreateBevel(self, 10, 300, 250, 50, bsFrame, bsRaised);
  bv := TwgBevel.Create(self);
  with bv do
  begin
    SetPosition(10, 300, 250, 50);
    Shape := bsFrame;
    Style := bsRaised;
  end;

  lb := TwgTextListBox.Create(self);
  lb.SetPosition(10,360,160,100);
  lb.Items.Add(u8('First'));
  lb.Items.Add(u8('Second'));
  lb.Items.Add(u8('third'));
  lb.Items.Add(u8('fourth'));
  lb.Items.Add(u8('fifth'));
  lb.Items.Add(u8('sixth'));
  lb.Items.Add(u8('seventh'));
  lb.Items.Add(u8('eightth'));
  lb.Items.Add(u8('nineth'));

  cmb := TwgChoiceList.Create(self);
  cmb.SetPosition(10, 470, 160, cmb.Height);
  cmb.Items.Add(u8('First'));
  cmb.Items.Add(u8('Second'));
  cmb.Items.Add(u8('third'));
  cmb.Items.Add(u8('fourth'));
  cmb.Items.Add(u8('fifth'));
  cmb.Items.Add(u8('sixth'));
  cmb.Items.Add(u8('seventh'));
  cmb.Items.Add(u8('eightth'));
  cmb.Items.Add(u8('nineth'));
end;

procedure TmyForm.ButtonClick(Sender : TObject);
begin
  //pop.HandleShow;
  //pop.ShowAt(self, 10,10);
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
