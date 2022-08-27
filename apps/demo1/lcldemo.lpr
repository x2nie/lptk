program lcldemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  //Interfaces, // this includes the LCL widgetset
  //Forms,
  gfxbase,
  lclunit1, lptk, lclunit2;

{$R *.res}

begin
  //RequireDerivedFormResource:=True;
  {Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;}
  GfxOpenDisplay('');

  Writeln('LPTK Dictionary');

  Form1 := TForm1.Create(nil);
  Form1.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;
end.

