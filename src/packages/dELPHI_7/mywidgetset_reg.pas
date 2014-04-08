unit mywidgetset_reg;

interface

uses
  SysUtils, Classes;


procedure Register;

implementation

uses mywidgetset;

procedure Register;
begin
  RegisterComponents('Standard', [TMyGroupBox, TMyButton]);
end;

end.
