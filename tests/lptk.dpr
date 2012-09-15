program lptk;

// for debug writelines use APPTYPE CONSOLE instead of GUI

{$APPTYPE GUI}
{.$APPTYPE CONSOLE}

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

uses
  Classes, SysUtils, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  messagequeue, gfxwidget, gfxform, wglabel, wgbutton, popupwindow,
  wgscrollbar, wglistbox, gfxclipboard, wgmemo, wgchoicelist, wggrid, gfxdialogs,
  wgcheckbox, wgcustomgrid;

type

  TstdImageList = class(TwgCustomGrid)
  public
    constructor Create(AOwner : TComponent); override;

    function GetRowCount : integer; override;

    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;
  end;

  TMyForm = class(TGfxForm)
  public
    ed,e2, e3   : TwgEdit;

    l1,l2,l3,l4,l5,l6,l7 : TwgLabel;

    //menu : TwgMenuBar;

    btn, btnMsg, btnClip, b3 : TwgButton;

    sb,sb2 : TwgScrollBar;

    lb : TwgTextListBox;

    memo : TwgMemo;

    cb : TwgCheckBox;

    choicel : TwgChoiceList;

    imglist : TstdImageList;

    procedure AfterCreate; override;

    procedure btnClick(sender : TObject);

    procedure btnMsgClick(sender : TObject);

    //procedure btnClipClick(sender : TObject);

    //procedure btnClipSet(sender : TObject);
  end;

  TModalForm = class(TGfxForm)
  public
    l1 : TwgLabel;
    e1 : TwgEdit;
    btn : TwgButton;
    procedure AfterCreate; override;

    procedure btnClick(Sender : TObject);
  end;

  TTestForm = class(TGfxForm)
  public
    memo : TwgMemo;
    grid : TwgGrid;

    procedure AfterCreate; override;

  end;

{ TstdImageList }

constructor TstdImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rowselect := true;
  AddColumn8('Image ID',80);
  AddColumn8('Pic.',30);
  FRowHeight := 20;
end;

function TstdImageList.GetRowCount: integer;
begin
  Result:=GfxImageLibrary.Count;
end;

procedure TstdImageList.DrawCell(row, col: integer; rect: TGfxRect; flags: integer);
begin
  if col = 1 then
  begin
    Canvas.DrawString16(rect.Left+1,rect.Top+1,u8(lowercase(GfxImageLibrary.Strings[row-1])));
  end
  else
  begin
    Canvas.DrawImage(rect.Left+1,rect.Top+1,GfxLibGetImage(GfxImageLibrary.Strings[row-1]));
  end;
end;

{ TMyForm }

procedure TMyForm.AfterCreate;
begin
  inherited AfterCreate;

  writeln('aftercreate');

  WindowTitle8 := 'LPTK test form';

  SetDimensions(30,10,550,340);

  WindowPosition := wpAuto;

  l1 := CreateLabel(self, 10,10, 'LPTK test 1.');
  l1.BackGroundColor := $A0A0A0;
  l2 := CreateLabel(self, 10,35, 'wgEdit:');
  //l2.Visible := false;

  ed := CreateEdit(self, 10,60, 100, 0);
  e2 := CreateEdit(self, 10,90, 120, 0);
  e3 := CreateEdit(self, 10,120, 150, 0);
  e3.Text := Str8To16('editbox');
  //e3.TabOrder := -1;

  l3 := CreateLabel(self, 10, 160, 'wgButton:');

  btn := CreateButton(self, 10,180,85, 'ModalForm', btnClick);

  btnMsg := CreateButton(self, 105,180,85, 'Message', btnMsgClick);
  btnMsg.ImageName := 'stdimg.ok';

//  btnClip := CreateButton(self, 105,160,80, 'Clipboard', @btnClipClick);

//  b3 := CreateButton(self, 10,190,120, 'SetClipboard', @btnClipSet);

  //sb := TwgScrollBar.Create(self);
  //sb.SetDimensions(200,10,18,150);
  //sb.Orientation := orVertical;

  l3 := CreateLabel(self, 200, 10, 'wgTextListBox:');

  lb := TwgTextListBox.Create(self);
  lb.SetDimensions(200,30,160,100);
  lb.Items.Add(Str8to16('First'));
  lb.Items.Add(Str8to16('Second'));
  lb.Items.Add(Str8to16('third'));
  lb.Items.Add(Str8to16('fourth'));
  lb.Items.Add(Str8to16('fifth'));
  lb.Items.Add(Str8to16('sixth'));
  lb.Items.Add(Str8to16('seventh'));
  lb.Items.Add(Str8to16('eightth'));
  lb.Items.Add(Str8to16('nineth'));

  l4 := CreateLabel(self, 10, 220, 'wgScrollbar:');

  sb2 := TwgScrollBar.Create(self);
  sb2.SetDimensions(10,240,150,18);
  sb2.Orientation := orHorizontal;

  l5 := CreateLabel(self, 200, 140, 'wgMemo:');

  memo := TwgMemo.Create(self);
  memo.SetDimensions(200,160,160,120);
  memo.Lines.Clear;
  memo.Lines.Add(Str8to16('Memo features:'));
  memo.Lines.Add(Str8to16(' automatic scrollbars'));
  memo.Lines.Add(Str8to16(' clipboard'));
  memo.Lines.Add(Str8to16(' mouse wheels'));
  memo.Lines.Add('');
  memo.Lines.Add(Str8to16('type here anything...'));

  l6 := CreateLabel(self, 10, 280, 'wgChoiceList:');

  choicel := CreateChoiceList(self, 10,300,150, nil);
  choicel.Items.Add(Str8to16('chlFirst'));
  choicel.Items.Add(Str8to16('chlSecond'));
  choicel.Items.Add(Str8to16('chlThird'));

  cb := TwgCheckBox.Create(self);
  cb.Left := 200;
  cb.Top := 300;
  cb.Width := 150; //(200,300,150,20);
  cb.Text := str8to16('CheckBox Widget');

  l7 := CreateLabel(self, 380, 10, 'Std Images:');

  imglist := TstdImageList.Create(self);
  imglist.SetDimensions(380,30,160,200);
{
  menu := TwgMenuBar.Create(frm);
  menu.left := 10;
  menu.top  := 160;
  menu.width := 200;
  menu.height := 25;
}

end;

procedure TMyForm.btnClick(sender : TObject);
var
  mdf : TModalForm;
begin
  writeln('OnClick');

  mdf := TModalForm.Create(nil);
  mdf.ShowModal;
  writeln('modal result is: ', mdf.ModalResult);
  mdf.Free;
end;

procedure TMyForm.btnMsgClick(sender: TObject);
begin
  writeln('msg');
  ShowMessage8(memo.Text8,'MsgTitle');
  Writeln('message finished');
end;


procedure TModalForm.AfterCreate;
begin
  WindowTitle8 := 'Modal form';

  SetDimensions(100,100,200,120);

  l1 := CreateLabel(self, 10,10, 'Hello');
  e1 := CreateEdit(self, 10,30,120,0);

  btn := CreateButton(self, 10,70,85, 'Close', btnClick);

end;

procedure TModalForm.btnClick(Sender: TObject);
begin
  Close;
end;

procedure TTestForm.AfterCreate;
begin
  inherited AfterCreate;
  WindowTitle8 := 'Widget Test Form';

  SetDimensions(10,10,450,300);

//  memo := TwgMemo.Create(self);
//  memo.SetDimensions(30,60, 300, 150);
  //memo.Text := Str8to16('memo');

  grid := TwgGrid.Create(self);
  grid.SetDimensions(30,60, 300, 150);

end;

var
  mf : TMyForm;
  testform : TTestForm;

begin
  GfxOpenDisplay('');

  Writeln('LPTK test');

//  test_sql;

//  testform := TTestForm.Create(nil);
//  testform.Show;

  mf := TMyForm.Create(nil);
  mf.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;
  writeln('test finished.');
end.

