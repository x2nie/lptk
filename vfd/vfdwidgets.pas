unit vfdwidgets;

interface

uses SysUtils, Classes, gfxbase, vfdwidgetclass, vfdprops;

procedure RegisterWidgets;

procedure RegisterVFDWidget(awc : TVFDWidgetClass);

function VFDWidgetCount : integer;
function VFDWidget(ind : integer) : TVFDWidgetClass;

function VFDFormWidget : TVFDWidgetClass;

implementation

uses gfxform,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox
  ;

var
  FVFDFormWidget : TVFDWidgetClass;
  FVFDWidgets : TList;

function VFDFormWidget : TVFDWidgetClass;
begin
  result := FVFDFormWidget;
end;

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

procedure AddWidgetPosProps(wgc : TVFDWidgetClass);
begin
  wgc.AddProperty('Left',TPropertyInteger,'');
  wgc.AddProperty('Top',TPropertyInteger,'');
  wgc.AddProperty('Width',TPropertyInteger,'');
  wgc.AddProperty('Height',TPropertyInteger,'');
end;

procedure RegisterWidgets;
var
  wc : TVFDWidgetClass;
  //wp : TVFDWidgetProperty;
begin
  LoadIcons;

  wc := TVFDWidgetClass.Create(TGfxForm);
  wc.NameBase := 'frm';
  wc.AddProperty('Left',TPropertyInteger,'');
  wc.AddProperty('Top',TPropertyInteger,'');
  wc.AddProperty('Width',TPropertyInteger,'');
  wc.AddProperty('Height',TPropertyInteger,'');
  wc.AddProperty('WindowTitle',TPropertyString16,'');
  FVFDFormWidget := wc;

  // Label
  wc := TVFDWidgetClass.Create(TwgLabel);
  wc.NameBase := 'lb';
  AddWidgetPosProps(wc);
  wc.AddProperty('Text',TPropertyString16,'Label text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the label text');
  wc.WidgetIconName := 'vfd.label';
  RegisterVFDWidget(wc);

  // Edit
  wc := TVFDWidgetClass.Create(TwgEdit);
  wc.NameBase := 'ed';
  AddWidgetPosProps(wc);
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.edit';
  RegisterVFDWidget(wc);

  // Button
  wc := TVFDWidgetClass.Create(TwgButton);
  wc.NameBase := 'btn';
  AddWidgetPosProps(wc);
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.AddProperty('ImageName',TPropertyString8,'');
  wc.WidgetIconName := 'vfd.button';
  RegisterVFDWidget(wc);

  // CheckBox
  wc := TVFDWidgetClass.Create(TwgCheckBox);
  wc.NameBase := 'cb';
  AddWidgetPosProps(wc);
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.checkbox';
  RegisterVFDWidget(wc);

  // ChoiceList
  wc := TVFDWidgetClass.Create(TwgChoiceList);
  wc.NameBase := 'chl';
  AddWidgetPosProps(wc);
  //wc.AddProperty('Text',TPropertyString16,'');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := '';
  RegisterVFDWidget(wc);


end;

initialization
begin
  FVFDWidgets := TList.Create;
end;

end.
