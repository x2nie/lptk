unit vfdwidgets;

interface

uses SysUtils, Classes, gfxbase, vfdwidgetclass, vfdprops;

procedure RegisterWidgets;

procedure RegisterVFDWidget(awc : TVFDWidgetClass);

function VFDWidgetCount : integer;
function VFDWidget(ind : integer) : TVFDWidgetClass;

implementation

uses wglabel, wgedit;

var
  FVFDWidgets : TList;

function VFDWidgetCount : integer;
begin
  result := FVFDWidgets.Count;
end;

function VFDWidget(ind : integer) : TVFDWidgetClass;
begin
  result := TVFDWidgetClass(FVFDWidgets[ind-1]);
end;

procedure RegisterVFDWidget(awc : TVFDWidgetClass);
begin
  FVFDWidgets.Add(awc);
end;

{$I icons.inc}

procedure LoadIcons;
begin
  GfxLibAddMaskedBMP(
                   'vfd.default',
            @stdimg_vfd_default,
      sizeof(stdimg_vfd_default),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.label',
            @stdimg_vfd_label,
      sizeof(stdimg_vfd_label),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.edit',
            @stdimg_vfd_edit,
      sizeof(stdimg_vfd_edit),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.checkbox',
            @stdimg_vfd_checkbox,
      sizeof(stdimg_vfd_checkbox),
            0,0 );
end;

procedure RegisterWidgets;
var
  wc : TVFDWidgetClass;
  //wp : TVFDWidgetProperty;
begin
  LoadIcons;

  // Label
  wc := TVFDWidgetClass.Create(TwgLabel);
  wc.AddProperty('Text',TPropertyString16,'Label text (string16)');
  wc.AddProperty('Font',TPropertyString8,'The font used displaying the label text');
  wc.WidgetIconName := 'vfd.label';
  RegisterVFDWidget(wc);

  // Edit
  wc := TVFDWidgetClass.Create(TwgEdit);
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('Font',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.edit';
  RegisterVFDWidget(wc);

end;

initialization
begin
  FVFDWidgets := TList.Create;
end;

end.
