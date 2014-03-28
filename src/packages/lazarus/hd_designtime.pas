{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hd_designtime;

interface

uses
  hd_designer, hd_form, hd_main, hd_widget, hd_defs, hd_button, hd_trackbar, 
  hd_progressbar, hd_application, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('hd_designer', @hd_designer.Register);
  RegisterUnit('hd_application', @hd_application.Register);
end;

initialization
  RegisterPackage('hd_designtime', @Register);
end.
