{ gfxdialogs.pas: message box functionality, here will be the standard dialogs later
  File maintainer: nvitya@freemail.hu

History:
}

unit gfxdialogs;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, GfxBase, GfxForm, schar16, GfxStyle, wgButton;

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

procedure ShowMessage8(msg, title : string); overload;
procedure ShowMessage8(msg : string); overload;

implementation

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

end.

