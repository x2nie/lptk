{ gfxdialogs.pas: message box functionality, here will be the standard dialogs later
  File maintainer: nvitya@freemail.hu

History:
}

unit gfxdialogs;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, GfxBase, GfxForm, schar16, GfxStyle,
  gfxwidget, wgButton, wgLabel, wgListBox, wgCheckBox, wgEdit;

type

  TMessageBox = class(TGfxForm)
  private
    FLines : TStringList;
    FFont : TGfxFont;
    FTextY : integer;
    FLineHeight : integer;
    FMaxLineWidth : integer;
    FMinWidth : integer;

  protected

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  public
    btn : TwgButton;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SetMessage8(txt : string);

    procedure btnClick(sender : TObject);

    procedure RePaint; override;

  end;

  TfrmFontSelect = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmFontSelect}
    lbLabel1 : TwgLabel;
    lstFaces : TwgTextListBox;
    lstSize : TwgTextListBox;
    lbLabel2 : TwgLabel;
    cbBold : TwgCheckBox;
    cbItalic : TwgCheckBox;
    cbAntiAlias : TwgCheckBox;
    lbLabel3 : TwgLabel;
    lbLabel4 : TwgLabel;
    edSample : TwgEdit;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: frmFontSelect}
    
    curfont : TGfxFont;

    procedure AfterCreate; override;

    destructor Destroy; override;

    procedure OkClick(sender : TObject);
    procedure CancelClick(sender : TObject);

    procedure OnParamChange(sender : TObject);

    procedure CreateFontList;

    function GetFontDesc : string;
    procedure SetFontDesc(desc : string);
  end;
  
  TwgShowColor = class(TWidget)
  public
    Color : TGfxColor;

    OnClick : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;

    procedure RePaint; override;
    procedure HandleMouseDown(X, Y: Integer; Button: Word; ShiftState: Word); override;

  end;

  TwgPredefColors = class(TWidget)
  protected
    Flastx, FLasty : integer;

    FBoxWidth : integer;
    FBoxHeight : integer;
    FGap : integer;

    FColors : TList;

  public
    OnColorClick : TNotifyEvent;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddColor(col : TGfxColor);

  end;

  TfrmSelectColor = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmSelectColor}
    lbLabel1 : TWGLABEL;
    lbLabel2 : TWGLABEL;
    lbLabel3 : TWGLABEL;
    edRComp : TWGEDIT;
    edGComp : TWGEDIT;
    edBComp : TWGEDIT;
    lbLabel4 : TWGLABEL;
    edHex : TWGEDIT;
    lbLabel5 : TWGLABEL;
    ShowColor : TwgShowColor;
    PredefColors : TwgPredefColors;
    btnOK : TWGBUTTON;
    btnCancel : TWGBUTTON;
    {@VFD_HEAD_END: frmSelectColor}

    procedure AfterCreate; override;

    procedure ColorClick(sender : TObject);

    procedure SetCurColor(col : TGfxColor);

    procedure RGBChange(sender : TObject);
    procedure HexChange(sender : TObject);

    procedure ButtonClick(sender : TObject);

  end;


{@VFD_NEWFORM_DECL}

procedure ShowMessage8(msg, title : string); overload;
procedure ShowMessage8(msg : string); overload;

function SelectFontDialog(var fontdesc : string) : boolean;
function SelectColorDialog(var color : TGfxColor) : boolean;

function StrToHex(hnum : string) : longword;

implementation

{@VFD_NEWFORM_IMPL}

function StrToHex(hnum : string) : longword;
var
  n : integer;
begin
  result := 0;
  for n:=1 to length(hnum) do
  begin
    result := (result shl 4) + longword(pos(upcase(hnum[n]),'123456789ABCDEF'));
  end;
end;

function SelectColorDialog(var color : TGfxColor) : boolean;
var
  frm : TfrmSelectColor;
begin
  result := false;
  frm := TfrmSelectColor.Create(nil);
  frm.SetCurColor(color);
  if frm.ShowModal > 0 then
  begin
    color := frm.ShowColor.Color;
    result := true;
  end;
  frm.Free;
end;

function SelectFontDialog(var fontdesc : string) : boolean;
var
  frm : TfrmFontSelect;
begin
  result := false;
  frm := TfrmFontSelect.Create(nil);
  frm.SetFontDesc(fontdesc);
  if frm.ShowModal > 0 then
  begin
    fontdesc := frm.GetFontDesc;
    result := true;
  end;
  frm.Free;
end;

procedure ShowMessage8(msg, title : string); overload;
var
  mf : TMessageBox;
begin
  mf := TMessageBox.Create(nil);
  mf.WindowTitle8 := title;
  mf.SetMessage8(msg);
  mf.ShowModal;
  mf.Free;
end;

procedure ShowMessage8(msg : string); overload;
begin
  ShowMessage8(msg,'Message');
end;

{ TMessageBox }

constructor TMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TStringList.Create;
  FFont := guistyle.LabelFont1;
  FTextY := 10;
  FLineHeight := FFont.Height + 4;
  FMinWidth := 200;
  FMaxLineWidth := 500;
  btn := TwgButton.Create(self);
  btn.Text := str8to16('OK');
  btn.Width := 80;
  btn.OnClick := {$ifdef FPC}@{$endif}btnClick;
  Resizeable := false;
end;

destructor TMessageBox.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TMessageBox.SetMessage8(txt: string);
var
  maxw : integer;
  n : integer;
  s,s16 : string16;
  c : char;

  procedure AddLine(all : boolean);
  var
    w : integer;
    m : integer;
  begin
    s16 := Str8to16(s);
    w := FFont.TextWidth16(s16);
    if w > FMaxLineWidth then
    begin
      while w > FMaxLineWidth do
      begin
        m := Length(s);
        repeat
          dec(m);
          s16 := str8to16(copy(s,1,m));
          w := FFont.TextWidth16(s16);
        until w <= FMaxLineWidth;
        if w > maxw then maxw := w;
        FLines.Add(s16);
        s := copy(s,m+1,length(s));
        s16 := Str8to16(s);
        w := FFont.TextWidth16(s16);
      end;
      if all then
      begin
        FLines.Add(s16);
        s := '';
      end;
    end
    else
    begin
      FLines.Add(s16);
      s := '';
    end;

    if w > maxw then maxw := w;
  end;

begin
  s := '';
  FLines.Clear;
  n := 1;
  maxw := 0;
  while n <= length(txt) do
  begin
    c := txt[n];
    if (c = #13) or (c = #10) then
    begin
      AddLine(false);
      if (c = #13) and (n < length(txt)) and (txt[n+1] = #10) then inc(n);
    end
    else s := s + c;
    inc(n);
  end;
  AddLine(true);

  width := maxw + 2*10;

  if width < FMinWidth then width := FMinWidth;

  btn.Top := FTextY + FLineHeight*FLines.Count + FTextY;

  btn.Left := (Width div 2) - (btn.Width div 2);

  height := btn.Top + btn.Height + FTextY;
end;

procedure TMessageBox.btnClick(sender: TObject);
begin
  ModalResult := 1;
end;

procedure TMessageBox.RePaint;
var
  n, y : integer;
  tw : integer;
begin
//  inherited RePaint;
  canvas.Clear(FBackgroundColor);
  canvas.SetFont(FFont);

  y := FTextY;
  for n:=0 to FLines.Count-1 do
  begin
    tw := FFont.TextWidth16(FLines[n]);
    canvas.DrawString16(width div 2 - tw div 2, y, FLines[n]);
    inc(y, FLineHeight);
  end;
end;

procedure TMessageBox.HandleKeyPress(var keycode, shiftstate: word; var consumed: boolean);
begin
  inherited;
  if keycode = KEY_ESC then
  begin
    Close;
  end;
end;

{ TfrmFontSelect }

procedure TfrmFontSelect.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmFontSelect}
  SetDimensions(300,100,428,380);
  WindowTitle8 := 'Font Selection';

  lbLabel1 := TwgLabel.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,8,73,16);
    Text := u8('Font face:');
  end;

  lstFaces := TwgTextListBox.Create(self);
  with lstFaces do
  begin
    SetDimensions(8,28,232,236);
    Items.Add(u8('lstFace'));
    OnChange := OnParamChange;
  end;

  lstSize := TwgTextListBox.Create(self);
  with lstSize do
  begin
    SetDimensions(248,28,52,236);
    Items.Add(u8('6'));
    Items.Add(u8('7'));
    Items.Add(u8('8'));
    Items.Add(u8('9'));
    Items.Add(u8('10'));
    Items.Add(u8('11'));
    Items.Add(u8('12'));
    Items.Add(u8('13'));
    Items.Add(u8('14'));
    Items.Add(u8('15'));
    Items.Add(u8('16'));
    Items.Add(u8('18'));
    Items.Add(u8('20'));
    Items.Add(u8('24'));
    Items.Add(u8('28'));
    Items.Add(u8('32'));
    Items.Add(u8('48'));
    Items.Add(u8('64'));
    Items.Add(u8('72'));
    OnChange := OnParamChange;
    FocusItem := 5;
  end;

  lbLabel2 := TwgLabel.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(308,8,54,16);
    Text := u8('Type:');
  end;

  cbBold := TwgCheckBox.Create(self);
  with cbBold do
  begin
    SetDimensions(308,32,87,20);
    Text := u8('Bold');
    OnChange := OnParamChange;
  end;

  cbItalic := TwgCheckBox.Create(self);
  with cbItalic do
  begin
    SetDimensions(308,56,87,20);
    Text := u8('Italic');
    OnChange := OnParamChange;
  end;

  cbAntiAlias := TwgCheckBox.Create(self);
  with cbAntiAlias do
  begin
    SetDimensions(308,124,99,20);
    Text := u8('Anti aliasing');
    OnChange := OnParamChange;
    Checked := true;
  end;

  lbLabel3 := TwgLabel.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(248,8,54,16);
    Text := u8('Size:');
  end;

  lbLabel4 := TwgLabel.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(8,268,55,16);
    Text := u8('Sample:');
  end;

  edSample := TwgEdit.Create(self);
  with edSample do
  begin
    SetDimensions(8,288,414,52);
    Text := u8('ABC abc gyjwstIl 123 ^337^369^336^368');
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,348,105,24);
    Text := u8('OK');
    ImageName := 'stdimg.ok';
    OnClick := OkClick;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(316,348,105,24);
    Text := u8('Cancel');
    ImageName := 'stdimg.cancel';
    OnClick := CancelClick;
  end;

  {@VFD_BODY_END: frmFontSelect}

  curfont := nil;
  CreateFontList;
end;

procedure TfrmFontSelect.OkClick(sender: TObject);
begin
  ModalResult := 1;
  Close;
end;

procedure TfrmFontSelect.CancelClick(sender: TObject);
begin
  ModalResult := -1;
  Close;
end;

procedure TfrmFontSelect.OnParamChange(sender: TObject);
var
  fdesc : string;
begin
  if curfont <> nil then curfont.Free;
  fdesc := GetFontDesc;
  Writeln(fdesc);
  curfont := GfxGetFont(fdesc);
  edSample.Font := curfont;
end;

procedure TfrmFontSelect.CreateFontList;
var
  fl : TStringList;
  n : integer;
begin
  lstFaces.Items.Clear;

  fl := GfxGetFontFaceList;

  for n:=0 to fl.Count-1 do lstFaces.Items.Add(u8(fl.Strings[n]));

  fl.Free;
end;

function TfrmFontSelect.GetFontDesc: string;
var
  s : string;
begin
  s := str16to8(lstFaces.Text)+'-'+str16to8(lstSize.Text);
  if cbBold.Checked then s := s+':bold';
  if cbItalic.Checked then s := s+':italic';

  s := s + ':';
  if cbAntiAlias.Checked then s := s+'antialias=true' else s := s + 'antialias=false';

  result := s;
end;

procedure TfrmFontSelect.SetFontDesc(desc: string);
var
  cp : integer;
  c : char;
  i : integer;

  token : string;
  prop, propval : string;

  function NextC : char;
  begin
    inc(cp);
    if cp > length(desc) then c := #0
                         else c := desc[cp];
    result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ','a'..'z','A'..'Z','_','0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
//  Writeln('GfxGetFont(''',desc,''')');

  cp := 1;
  c := desc[1];

  cbBold.Checked := false;
  cbItalic.Checked := false;

  cbAntiAlias.Checked := true;

  NextToken;
  i := lstFaces.Items.IndexOf(u8(token));
  if i >= 0 then lstFaces.FocusItem := i+1;
  if c = '-' then
  begin
    NextC;
    NextToken;
    i := lstSize.Items.IndexOf(u8(token));
    if i >= 0 then lstSize.FocusItem := i+1;
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    if prop = 'BOLD' then
    begin
      cbBold.Checked := true;
    end
    else if prop = 'ITALIC' then
    begin
      cbItalic.Checked := true;
    end
    else if prop = 'ANTIALIAS' then
    begin
      if propval = 'FALSE' then cbAntialias.Checked := false;
    end
    ;

  end;

  OnParamChange(self);
end;

destructor TfrmFontSelect.Destroy;
begin
  if curfont <> nil then curfont.Free;
  inherited;
end;

{ Color selectors }

{ TwgPredefColors }

constructor TwgPredefColors.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLastx := 0; Flasty := 0;
  FBoxWidth := 32;
  FBoxHeight := 16;
  FGap := 4;

  OnColorClick := nil;

  FColors := TList.Create;
end;

destructor TwgPredefColors.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

procedure TwgPredefColors.AddColor(col: TGfxColor);
var
  sc : TwgShowColor;
begin
  FColors.Add(Pointer(col));

  sc := TwgShowColor.Create(self);
  sc.Color := col;
  sc.Left := FLastx;
  sc.Top := FLasty;
  sc.width := FBoxWidth;
  sc.height := FBoxHeight;
  sc.OnClick := OnColorClick;

  Inc(Flastx,FBoxWidth+FGap);
  if Flastx+FBoxWidth > Width then
  begin
    inc(Flasty,FBoxHeight+FGap);
    FLastx := 0;
  end;
end;

{ TwgShowColor }

constructor TwgShowColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  color := $AA2233;
  OnClick := nil;
end;

procedure TwgShowColor.RePaint;
begin
  canvas.SetColor(color);
  canvas.FillRectangle(1,1,Width-2,Height-2);
  canvas.SetColor(0);
  canvas.DrawRectangle(0,0,Width,Height);
end;

procedure TwgShowColor.HandleMouseDown(X, Y: Integer; Button: Word;
  ShiftState: Word);
begin
  inherited HandleMouseDown(X, Y, Button, ShiftState);
  if Assigned(OnClick) then OnClick(self);
end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmSelectColor.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmSelectColor}
  SetDimensions(286,126,328,262);
  WindowTitle8 := 'Select Color';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,10,22,16);
    Text := u8('R:');
  end;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(8,34,22,16);
    Text := u8('G:');
  end;

  lbLabel3 := TWGLABEL.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(8,58,22,16);
    Text := u8('B:');
  end;

  edRComp := TWGEDIT.Create(self);
  with edRComp do
  begin
    SetDimensions(24,8,62,20);
    Text := u8('255');
    OnChange := RGBChange;
  end;

  edGComp := TWGEDIT.Create(self);
  with edGComp do
  begin
    SetDimensions(24,32,62,20);
    Text := u8('64');
    OnChange := RGBChange;
  end;

  edBComp := TWGEDIT.Create(self);
  with edBComp do
  begin
    SetDimensions(24,56,62,20);
    Text := u8('45');
    OnChange := RGBChange;
  end;

  lbLabel4 := TWGLABEL.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(8,84,54,16);
    Text := u8('Hex:');
  end;

  edHex := TWGEDIT.Create(self);
  with edHex do
  begin
    SetDimensions(8,104,78,20);
    Text := u8('edHex');
    OnChange := HexChange;
  end;

  lbLabel5 := TWGLABEL.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(8,136,57,16);
    Text := u8('Preview:');
  end;

  ShowColor := TwgShowColor.Create(self);
  with ShowColor do
  begin
    SetDimensions(8,156,308,60);
  end;

  PredefColors := TwgPredefColors.Create(self);
  with PredefColors do
  begin
    SetDimensions(104,8,212,116);
    OnColorClick := ColorClick;
  end;

  btnOK := TWGBUTTON.Create(self);
  with btnOK do
  begin
    SetDimensions(8,228,105,24);
    Text := u8('OK');
    OnClick := ButtonClick;
    ImageName := 'stdimg.ok';
  end;

  btnCancel := TWGBUTTON.Create(self);
  with btnCancel do
  begin
    SetDimensions(212,228,105,24);
    Text := u8('Cancel');
    OnClick := ButtonClick;
    ImageName := 'stdimg.cancel';
  end;

  {@VFD_BODY_END: frmSelectColor}

  with PredefColors do
  begin
    AddColor($000000);
    AddColor($000080);
    AddColor($008000);
    AddColor($800000);
    AddColor($008080);
    AddColor($800080);
    AddColor($808000);
    AddColor($808080);
    AddColor($8080ff);
    AddColor($80ff80);
    AddColor($ff8080);
    AddColor($80ffff);
    AddColor($ff80ff);
    AddColor($ffff80);
    AddColor($ffffff);

//    AddColor($000000);
//    AddColor($000000);
//    AddColor($000000);
//    AddColor($000000);
  end;

  SetCurColor(12345);

end;

procedure TfrmSelectColor.ColorClick(sender: TObject);
begin
  Writeln('color clicked: ',IntToHex(TwgShowColor(sender).color,6));

  SetCurColor(TwgShowColor(sender).color);
end;

procedure TfrmSelectColor.SetCurColor(col: TGfxColor);
begin
  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edHex.Text8 := IntToHex(col,6);

  edBComp.Text8 := IntToStr((col and $0000FF));
  edGComp.Text8 := IntToStr((col and $00FF00) shr 8);
  edRComp.Text8 := IntToStr((col and $FF0000) shr 16);
end;

procedure TfrmSelectColor.RGBChange(sender: TObject);
var
  col : TGfxColor;
begin
  col := StrToIntDef(edBComp.Text8,0)
         + StrToIntDef(edGComp.Text8,0) shl 8
         + StrToIntDef(edRComp.Text8,0) shl 16;

  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edHex.Text8 := IntToHex(col,6);
end;

procedure TfrmSelectColor.HexChange(sender: TObject);
var
  col : TGfxColor;
begin
  col := StrToHex(edHex.Text8);

  showcolor.color := col;
  if showcolor.WinHandle > 0 then showcolor.RePaint;

  edBComp.Text8 := IntToStr((col and $0000FF));
  edGComp.Text8 := IntToStr((col and $00FF00) shr 8);
  edRComp.Text8 := IntToStr((col and $FF0000) shr 16);
end;

procedure TfrmSelectColor.ButtonClick(sender: TObject);
begin
  if sender = btnOK then ModalResult := 1 else ModalResult := -1;
  Close;
end;

end.

