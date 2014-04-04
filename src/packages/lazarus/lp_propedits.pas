unit lp_propedits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propedits;
type

  { TFontDescPropertyEditor }

  TFontDescPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    //function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;


implementation

uses lp_main;

{ TFontDescPropertyEditor }

function TFontDescPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList, paRevertable];
end;



procedure TFontDescPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  GetFontDescValues(Proc);
end;

procedure TFontDescPropertyEditor.SetValue(const NewValue: ansistring);
begin
  inherited SetValue(NewValue);
end;

end.

