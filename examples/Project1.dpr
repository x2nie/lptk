program Project1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{.$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  hd_defs,
  hd_main,
  hd_form,
  hd_widget,
  Unit1 in 'Unit1.pas' {pgfForm1: TpgfForm};

{.$R *.res}

procedure main;
begin
  pgfOpenDisplay('');
  pgfForm1:= TpgfForm1.Create(nil);
  pgfForm1.WindowPosition := wpAuto;
  pgfForm1.SetPosition(100,100,300,300);
  pgfForm1.MinWidth  := 200;
  pgfForm1.MinHeight := 150;
  //pgfForm1.Visible := true;

  
  pgfForm1.Showmodal;
  pgfRunMessageLoop;
end;

begin
  main();



end.
