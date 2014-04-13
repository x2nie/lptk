unit hd_register;

interface
uses
Classes, SysUtils, DesignIntf, DesignEditors;





procedure Register;

implementation

uses hd_defs, hd_main, hd_form;

procedure Register;
begin
  RegisterCustomModule (TpgfForm, TCustomModule);
  //RegisterCustomModule (TPanel, TPanelModule);
//  RegisterLibraryExpert(TPanelEditExpert.Create);

end;

end.
