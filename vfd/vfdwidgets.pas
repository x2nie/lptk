unit vfdwidgets;

interface

uses SysUtils, Classes, gfxbase, vfdwidgetclass, vfdprops;

procedure RegisterWidgets;

procedure RegisterVFDWidget(awc : TVFDWidgetClass);

function VFDWidgetCount : integer;
function VFDWidget(ind : integer) : TVFDWidgetClass;

implementation

uses wglabel;

var
  FVFDWidgets : TList;

function VFDWidgetCount : integer;
begin
  result := FVFDWidgets.Count;
end;

function VFDWidget(ind : integer) : TVFDWidgetClass;
begin
  result := TVFDWidgetClass(FVFDWidgets[ind]);
end;

procedure RegisterVFDWidget(awc : TVFDWidgetClass);
begin
  FVFDWidgets.Add(awc);
end;

procedure RegisterWidgets;
var
  wc : TVFDWidgetClass;
  //wp : TVFDWidgetProperty;
begin

  // Label
  wc := TVFDWidgetClass.Create(TwgLabel);
  wc.AddProperty('Text',TPropertyString16,'Label text (string16)');
  wc.AddProperty('Font',TPropertyString8,'The font used displaying the label text');
  RegisterVFDWidget(wc);

end;

initialization
begin
  FVFDWidgets := TList.Create;
end;

end.
