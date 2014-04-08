program MyProjectDemo;

uses
  Forms,
  unit1 in 'unit1.pas' {MyForm2: TMyForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMyForm2, MyForm2);
  Application.Run;
end.
