program styleeditor;

{$APPTYPE GUI}

{$IFDEF FPC}
    {$mode delphi}
    {$H+}
{$ENDIF}

uses SysUtils, Classes, gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg,
  wgedit, gfxwidget, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, gfxdialogs, wgcheckbox,
  wgcustomgrid
  ;

type

  TlptkSettingType = (stColor,stFont,stInteger);

  TlptkStyleSetting = class
  public
    name : string;
    stype : TlptkSettingType;
    value : string;
    used  : boolean;

    constructor Create(aname : string; atype : TlptkSettingType; avalue : string);
  end;

  TwgStyleGrid = class(TwgCustomGrid)
  protected
    styles : TList;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DrawCell(row,col : integer; rect : TGfxRect; flags : integer); override;

    procedure AddStyle(aname : string; atype : TlptkSettingType; avalue : string);

    function CurrentStyle : TlptkStyleSetting;

  end;

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnOpen : TwgButton;
    btnSave : TwgButton;
    btnClose : TwgButton;
    grid : TwgStyleGrid;
    btnNew : TwgButton;
    btnEdit : TwgButton;  
    btnDelete : TwgButton;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;

    procedure CloseClick(sender : TObject);

    procedure EditClick(sender : TObject);

    procedure GridDoubleClick(Sender: TObject; x,y : integer; var button : word; var shiftstate : word);

  end;

  TfrmNumEdit = class(TGfxForm)           
  public
    {@VFD_HEAD_BEGIN: frmNumEdit}
    lbCaption : TwgLabel;
    edEdit : TwgEdit;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: frmNumEdit}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  mfrm : TfrmMain;

{@VFD_NEWFORM_IMPL}

procedure TfrmNumEdit.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmNumEdit}
  SetDimensions(362,219,254,67);
  WindowTitle8 := 'Number Edit';

  lbCaption := TwgLabel.Create(self);
  with lbCaption do
  begin
    SetDimensions(4,8,54,16);
    Text := u8('lbCaption');
  end;

  edEdit := TwgEdit.Create(self);
  with edEdit do
  begin
    SetDimensions(4,28,150,22);
    Text := u8('edEdit');
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(164,8,81,24);
    Text := u8('OK');
    ImageName := 'stdimg.ok';
    ModalResult := 1;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(164,36,81,24);
    Text := u8('Cancel');
    imagename := 'stdimg.cancel';
    ModalResult := -1;
  end;

  {@VFD_BODY_END: frmNumEdit}
end;


{ TlptkStyleSetting }

constructor TlptkStyleSetting.Create(aname: string;
  atype: TlptkSettingType; avalue: string);
begin
  name := aname;
  stype := atype;
  value := avalue;
  used  := true;
end;

{ TfrmMain }

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(300,100,500,400);
  WindowTitle8 := 'LPTK Style Editor';

  btnOpen := TwgButton.Create(self);
  with btnOpen do
  begin
    SetDimensions(4,4,80,24);
    Text := u8('Open...');
    ImageName := 'stdimg.open';
  end;

  btnSave := TwgButton.Create(self);
  with btnSave do
  begin
    SetDimensions(92,4,80,24);
    Text := u8('Save');
    ImageName := 'stdimg.save';
  end;

  btnClose := TwgButton.Create(self);
  with btnClose do
  begin
    SetDimensions(413,4,80,24);
    Anchors := [anRight,anTop];
    Text := u8('Close');
    ImageName := 'stdimg.exit';
    OnClick := CloseClick;
  end;

  grid := TwgStyleGrid.Create(self);
  with grid do
  begin
    SetDimensions(4,36,489,325);
    Anchors := [anLeft,anRight,anTop,anBottom];
    OnDoubleClick := GridDoubleClick;
  end;

  btnNew := TwgButton.Create(self);
  with btnNew do
  begin
    SetDimensions(4,368,92,24);
    Anchors := [anLeft,anBottom];
    Text := u8('New');
    ImageName := 'stdimg.new';
  end;

  btnEdit := TwgButton.Create(self);
  with btnEdit do
  begin
    SetDimensions(103,368,92,24);
    Anchors := [anLeft,anBottom];
    Text := u8('Edit');
    ImageName := 'stdimg.edit';
    OnClick := EditClick;
  end;

  btnDelete := TwgButton.Create(self);
  with btnDelete do
  begin
    SetDimensions(200,368,92,24);
    Anchors := [anLeft,anBottom];
    Text := u8('Delete');
    ImageName := 'stdimg.delete';
  end;

  {@VFD_BODY_END: frmMain}

end;

procedure TfrmMain.CloseClick(sender: TObject);
begin
  Close;
end;

procedure TfrmMain.EditClick(sender: TObject);
var
  st : TlptkStyleSetting;
  c : TGfxColor;
  frm : TfrmNumEdit;
begin
  st := grid.CurrentStyle;
  if st = nil then Exit;

  if st.stype = stFont then SelectFontDialog(st.Value)
  else if st.stype = stcolor then
  begin
    c := StrToHex(st.Value);
    if SelectColorDialog(c) then st.value := IntToHex(c,6);
  end
  else if st.stype = stInteger then
  begin
    frm := TfrmNumEdit.Create(nil);
    frm.lbCaption.Text8 := st.name+':';
    frm.edEdit.Text8 := st.value;
    while frm.ShowModal = 1 do
    begin
      try
        st.value := IntToStr(StrToInt(frm.edEdit.Text8));
        break;
      except
        ShowMessage8('Invalid number','Error');
      end;
    end;
    frm.Free;
  end;
end;

procedure TfrmMain.GridDoubleClick(Sender: TObject; x,y : integer; var button : word; var shiftstate : word);
begin
  EditClick(self);
end;

{ TwgStyleGrid }

constructor TwgStyleGrid.Create(AOwner: TComponent);
begin
  inherited;
  styles := TList.Create;

  RowSelect := true;
  FRowHeight := 22;

  AddColumn8('Settings Name',120);   // 1
  AddColumn8('Type',40);             // 2
  AddColumn8('Use',24);              // 3
  AddColumn8('Sample',120);          // 4
  AddColumn8('Value',160);           // 5
end;

procedure TwgStyleGrid.DrawCell(row, col: integer; rect: TGfxRect;
  flags: integer);
var
  st : TlptkStyleSetting;
  x,y : integer;
  s : string;
  fnt : TGfxFont;
  c : TGfxColor;
begin
  if (row < 1) or (row > styles.Count) then exit;
  st := styles[row-1];

  y := RowHeight div 2 - Font.Height div 2;
  if y < 0 then y := 0;
  inc(y,Rect.top);
  x := Rect.left+1;

  case col of
  1 : Canvas.DrawString16(x,y,u8(st.name));
  2 : begin
        case st.stype of
        stColor : s := 'Color';
        stFont  : s := 'Font';
        stInteger : s := 'Int';
        else
          s := '???';
        end;
        Canvas.DrawString16(x,y,u8(s));
      end;
  3 : begin
        if st.used then s := 'Y' else s := 'N';
        Canvas.DrawString16(x,y,u8(s));
      end;
  4 : begin
        // sample
        case st.stype of
        stFont:
          begin
            fnt := GfxGetFont(st.value);
            if fnt <> nil then
            begin
              Canvas.SetFont(fnt);
              Canvas.DrawString16(x,Rect.top+1,u8('Sample Text'));
              Canvas.SetFont(Font);
              fnt.Free;
            end;
          end;
        stColor:
          begin
            c := StrToHex(st.value);
            Canvas.SetColor(c);
            canvas.FillRectangle(Rect.left+3,Rect.top+3,Rect.Width-6,Rect.Height-6);
          end;
        stInteger : Canvas.DrawString16(x,y,u8(st.Value));
        end;
      end;

  5 : Canvas.DrawString16(x,y,u8(st.Value));
  else
    inherited;
  end;
end;

destructor TwgStyleGrid.Destroy;
var
  n : integer;
begin
  for n:=0 to styles.count-1 do TObject(styles[n]).Free;
  styles.Free;
  inherited;
end;

procedure TwgStyleGrid.AddStyle(aname: string; atype: TlptkSettingType; avalue: string);
var
  st : TlptkStyleSetting;
begin
  st := TlptkStyleSetting.Create(aname, atype, avalue);
  styles.Add(st);
  RowCount := styles.count;
end;

function TwgStyleGrid.CurrentStyle : TlptkStyleSetting;
begin
  if (FocusRow < 1) or (FocusRow > styles.Count) then result := nil
  else result := styles[FocusRow-1];
end;


begin
  gfxOpenDisplay('');
  mfrm := TfrmMain.Create(nil);
  with mfrm.grid do
  begin
    AddStyle('fontset',stFont,'arial-10');
    AddStyle('colset',stColor,'001Acc');
    AddStyle('numset',stInteger,'123');
  end;  
  mfrm.Show;
  gfxDoMessageLoop;
  gfxCloseDisplay;
end.
