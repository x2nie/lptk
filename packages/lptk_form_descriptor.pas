unit lptk_form_descriptor;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils,
   gfxForm,
   ProjectIntf;

type

{ TFileDescPascalUnitWithMyForm }

 TFileDescPascalUnitWithMyForm = class(TFileDescPascalUnitWithResource)
 public
   constructor Create; override;
   function GetInterfaceUsesSection: string; override;
   function GetLocalizedName: string; override;
   function GetLocalizedDescription: string; override;
 end;


implementation

{ TFileDescPascalUnitWithMyForm }

constructor TFileDescPascalUnitWithMyForm.Create;
begin
  inherited Create;
  Name:='LPTK Form';
  ResourceClass:=TGfxForm;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithMyForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, LPTK, gfxform, wgbutton';
end;

function TFileDescPascalUnitWithMyForm.GetLocalizedName: string;
begin
  Result:='LPTKForm';
end;

function TFileDescPascalUnitWithMyForm.GetLocalizedDescription: string;
begin
  Result:='Create a new Form of LPTK';
end;


end.

