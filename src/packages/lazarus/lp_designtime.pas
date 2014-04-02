{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lp_designtime;

interface

uses
  lp_designer, lp_form, lp_main, lp_widget, lp_defs, lp_button, lp_trackbar, 
  lp_progressbar, lp_descriptors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lp_designer', @lp_designer.Register);
  RegisterUnit('lp_descriptors', @lp_descriptors.Register);
end;

initialization
  RegisterPackage('lp_designtime', @Register);
end.
