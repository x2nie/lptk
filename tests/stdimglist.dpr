program stdimglist;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  pgf_defs, pgf_main, pgf_form, pgf_imgfmt_bmp,
  wglabel, hd_button, wgedit, wgmemo;

type

  { TmyForm }

  TmyForm = class(TpgfForm)
  public
    procedure AfterCreate; override;

    procedure HandlePaint; override;
  end;

{ TmyForm }

procedure TmyForm.AfterCreate;
begin
  SetPosition(100,100,700,500);
  WindowTitle := 'PasGF Standard Image Listing';
end;

procedure TmyForm.HandlePaint;
var
  sl : TStringList;
  img : TpgfImage;
  n : integer;
  x, y : TpgfCoord;
begin
  x := 8;
  y := 8;
  sl := TStringList.Create;

  canvas.BeginDraw;

  //inherited HandlePaint;

  canvas.Clear(FBackgroundColor);

  pgfImages.ListImages(sl);

  for n:=1 to sl.Count do
  begin
    canvas.DrawString(x,y, utf8(sl[n-1]+':') );

    img := TpgfImage(sl.Objects[n-1]);
    if img <> nil then
    begin
      canvas.DrawImage(x+150,y,img);
    end;

    inc(y,img.Height+8);

    if y > self.Height-24 then
    begin
      inc(x, 200);
      y := 8;
    end;
  end;

  canvas.EndDraw;

  sl.Free;
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
