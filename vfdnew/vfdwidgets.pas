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
                   'vfd.arrow',
            @stdimg_vfd_arrow,
      sizeof(stdimg_vfd_arrow),
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
                   'vfd.memo',
            @stdimg_vfd_memo,
      sizeof(stdimg_vfd_memo),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.button',
            @stdimg_vfd_button,
      sizeof(stdimg_vfd_button),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.checkbox',
            @stdimg_vfd_checkbox,
      sizeof(stdimg_vfd_checkbox),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.listbox',
            @stdimg_vfd_listbox,
      sizeof(stdimg_vfd_listbox),
            0,0 );
  GfxLibAddMaskedBMP(
                   'vfd.choicelist',
            @stdimg_vfd_choicelist,
      sizeof(stdimg_vfd_choicelist),
            0,0 );

  GfxLibAddMaskedBMP(
                   'vfd.panel',
            @stdimg_vfd_panel,
      sizeof(stdimg_vfd_panel),
            0,0 );
{
  GfxLibAddMaskedBMP(
                   'vfd.',
            @stdimg_vfd_,
      sizeof(stdimg_vfd_),
            0,0 );
}            
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
  wc.AddProperty('WindowTitle',TPropertyString16,'');
  FVFDFormWidget := wc;

  // Label
  wc := TVFDWidgetClass.Create(TwgLabel);
  wc.NameBase := 'lb';
  wc.AddProperty('Text',TPropertyString16,'Label text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the label text');
  wc.WidgetIconName := 'vfd.label';
  RegisterVFDWidget(wc);

  // Edit
  wc := TVFDWidgetClass.Create(TwgEdit);
  wc.NameBase := 'ed';
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.edit';
  RegisterVFDWidget(wc);

  // Memo
  wc := TVFDWidgetClass.Create(TwgMemo);
  wc.NameBase := 'memo';
  //wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.memo';
  RegisterVFDWidget(wc);

  // Button
  wc := TVFDWidgetClass.Create(TwgButton);
  wc.NameBase := 'btn';
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.AddProperty('ImageName',TPropertyString8,'');
  wc.AddProperty('ModalResult',TPropertyInteger,'');
  wc.WidgetIconName := 'vfd.button';
  RegisterVFDWidget(wc);

  // CheckBox
  wc := TVFDWidgetClass.Create(TwgCheckBox);
  wc.NameBase := 'cb';
  wc.AddProperty('Text',TPropertyString16,'Initial text (string16)');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.checkbox';
  RegisterVFDWidget(wc);

  // ChoiceList
  wc := TVFDWidgetClass.Create(TwgChoiceList);
  wc.NameBase := 'chl';
  //wc.AddProperty('Text',TPropertyString16,'');
  wc.AddProperty('FontName',TPropertyString8,'The font used displaying the text');
  wc.WidgetIconName := 'vfd.choicelist';
  RegisterVFDWidget(wc);


end;

initialization
begin
  FVFDWidgets := TList.Create;
end;

end.
