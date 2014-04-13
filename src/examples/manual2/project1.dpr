program project1;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}
{$apptype console}

uses
  lp_defs, lp_main, lp_form, Unit1 
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TlpForm1, lpForm1);
  Application.Run;
end.

