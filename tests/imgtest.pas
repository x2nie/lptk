program imgtest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses gfxbase, gfxform, gfxbmpimage, schar16, gfxstyle, gfxstdimg;

type
  TMainForm = class(TGfxForm)
  public
    img : TGfxImage;
    fpy  : TGfxImage;
    flower : TGfxImage;

    procedure AfterCreate; override;

    procedure RePaint; override;
  end;

{ $I bmp_testimg.inc}

{$I bmp_wtest.inc}
{$I bmp_floppy.inc}

procedure TMainForm.AfterCreate;
begin
  Height := 150;
  Width := 300;

  img := CreateBMPImage(@bmp_wtest, sizeof(bmp_wtest));
  img.Invert;
  
  GfxLibAddImage('my.wtest', img);

//  flower := CreateBMPImage(@bmp_testimg, sizeof(bmp_testimg));

//  fpy := CreateBMPImage(@bmp_floppy, sizeof(bmp_floppy));
//  fpy.CreateMaskFromSample(0,0);
  
  fpy := GfxLibAddMaskedBMP('my.floppy',@bmp_floppy, sizeof(bmp_floppy), 0,0);

  inherited AfterCreate;
end;

procedure TMainForm.RePaint;
var
  im : TGfxImage;
begin
  inherited RePaint;
//  Canvas.Clear(clWindowBackground);
//  Canvas.DrawString16(10,20,u8('qqcska'));

  im := GfxLibGetImage('my.floppy');

  Canvas.DrawImage(10,20, im);
  Canvas.DrawImagePart(50,20, fpy,0,0,fpy.height,fpy.height);

  Canvas.DrawImagePart(75,20, fpy,fpy.height,0,fpy.height,fpy.height);

  Canvas.SetColor($FF0000);
  Canvas.DrawImage(100,20, img);

//  Canvas.DrawImage(10,50, flower);
end;

var
  MainForm : TMainForm;

begin
  gfxOpenDisplay('');
  MainForm := TMainForm.Create(nil);
  MainForm.Show;
  gfxDoMessageLoop;
  gfxCloseDisplay;
end.
