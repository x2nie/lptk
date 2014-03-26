program project1;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

uses
  hd_defs, hd_main, hd_form, Unit1 
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TpgfForm1, pgfForm1);
  Application.Run;
end.

