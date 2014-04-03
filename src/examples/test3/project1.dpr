program project1;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

uses
  lp_defs, lp_main, lp_form, Unit1 
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm1, frm1);
  Application.Run;
end.

