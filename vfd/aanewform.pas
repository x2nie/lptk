unit aanewform;

interface

uses
  SysUtils, Classes, inifiles, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox, vfddesigner;

type

  TForm1 = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: Form1}
    lbLabel1 : TwgLabel;
    edEdit1 : TwgEdit;
    {@VFD_HEAD_END: Form1}

    procedure AfterCreate; override;
  end;

  TForm2 = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: Form2}
    lbLabel1 : TwgLabel;
    btnButton1 : TwgButton;
    cbCheck1 : TwgCheckBox;
    edEdit1 : TwgEdit;
    chlChoice1 : TwgChoiceList;
    valami1 : TwgValami;
    lstList1 : TwgTextListBox;
    Twgabc1 : Twgabc;
    {@VFD_HEAD_END: Form2}

    procedure AfterCreate; override;
  end;

  TColumnEdit = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: ColumnEdit}
    gridGrid1 : TwgDBGrid;
    {@VFD_HEAD_END: ColumnEdit}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TColumnEdit.AfterCreate;
begin
  {@VFD_BODY_BEGIN: ColumnEdit}
  SetDimensions(281,96,415,317);
  WindowTitle := 'Column editor';

  gridGrid1 := TwgDBGrid.Create(self);
  with gridGrid1 do
  begin
    SetDimensions(12,12,380,284);
    AddColumn8('Newsd','',50,alLeft);
    AddColumn8('New','',50,alCenter);
  end;

  {@VFD_BODY_END: ColumnEdit}
end;

procedure TForm2.AfterCreate;
begin
  {@VFD_BODY_BEGIN: Form2}
  SetDimensions(598,77,300,251);
  WindowTitle := 'Form2';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(28,28,54,16);
    Text := u8('lbLabel1');
  end;

  btnButton1 := TwgButton.Create(self);
  with btnButton1 do
  begin
    SetDimensions(16,60,105,24);
    Text := u8('btnButton1');
  end;

  cbCheck1 := TwgCheckBox.Create(self);
  with cbCheck1 do
  begin
    SetDimensions(12,104,82,20);
    Text := u8('cbCheck1');
  end;

  edEdit1 := TwgEdit.Create(self);
  with edEdit1 do
  begin
    SetDimensions(8,140,150,20);
    Text := u8('');
  end;

  chlChoice1 := TwgChoiceList.Create(self);
  with chlChoice1 do
  begin
    SetDimensions(12,176,150,22);
    Items.Add(u8('egy ketto'));
    Items.Add(u8('harom'));
    Items.Add(u8('negy'));
    Items.Add(u8('ot hat'));
  end;

  valami1 := TwgValami.Create(self);
  with valami1 do
  begin
    SetDimensions(12,212,148,24);
    text8 := 'asdfsa';
  end;

  lstList1 := TwgTextListBox.Create(self);
  with lstList1 do
  begin
    SetDimensions(136,15,160,101);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Items.Add(u8('lstList1s'));
    Items.Add(u8('df g'));
    Items.Add(u8('sdfg'));
    Items.Add(u8('sdf'));
    Items.Add(u8('gsdfsdf gsdf'));
  end;

  Twgabc1 := Twgabc.Create(self);
  with Twgabc1 do
  begin
    SetDimensions(168,140,124,96);
    // egy ketto harom;
  end;

  {@VFD_BODY_END: Form2}
end;


procedure TForm1.AfterCreate;
begin
  {@VFD_BODY_BEGIN: Form1}
  SetDimensions(597,372,211,110);
  WindowTitle := 'Form1';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(32,20,54,16);
    Text := u8('lbLabel1');
  end;

  edEdit1 := TwgEdit.Create(self);
  with edEdit1 do
  begin
    SetDimensions(32,52,150,20);
    Text := u8('edEdit1יב^^^337ת^369ף');
  end;

  {@VFD_BODY_END: Form1}
end;


end.
