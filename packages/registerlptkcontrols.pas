unit registerlptkcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gfxform, wgbevel, wgbutton, LResources, Forms,
  FormEditingIntf, ProjectIntf,
  lptk_mediator, lptk_form_descriptor;

procedure Register;

implementation

procedure Register;
begin
  FormEditingHook.RegisterDesignerMediator(TLptkWidgetMediator);
  RegisterComponents('LPTK', [TwgBevel,TwgButton]);
  //RegisterComponentEditor(TExtendedTabControl, TExtendedTabControlComponentEditor); //RegisterLazControls.pas

  //FormEditingHook.RegisterDesignerBaseClass(TGfxForm);
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithMyForm.Create,
                                FileDescGroupName);
end;

end.

