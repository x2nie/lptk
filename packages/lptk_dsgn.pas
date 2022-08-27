{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LPTK_Dsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  registerlptkcontrols, lptk_mediator, lptk_form_descriptor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerlptkcontrols', @registerlptkcontrols.Register);
end;

initialization
  RegisterPackage('LPTK_Dsgn', @Register);
end.
