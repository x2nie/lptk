program project1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

uses
  hd_defs, hd_main, hd_form, Unit1, Unit2 
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TpgfForm1, pgfForm1);
  Application.CreateForm(TpgfForm2, pgfForm2);
  Application.Run;
end.

