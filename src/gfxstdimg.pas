unit gfxstdimg;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase;

procedure GfxCreateStandardImages;

implementation

{$I stdimages.inc}

procedure GfxCreateStandardImages;
var
  img : TGfxImage;
begin
  img := GfxLibAddMaskedBMP(
            'stdimg.cancel',
            @stdimg_cancel,
      sizeof(stdimg_cancel),
            0,0 );

  //GfxLibAddImage( 'STDIMG.NO', img );

  img := GfxLibAddMaskedBMP(
            'stdimg.close',
            @stdimg_close,
      sizeof(stdimg_close),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.close2',
            @stdimg_close2,
      sizeof(stdimg_close2),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.close3',
            @stdimg_close3,
      sizeof(stdimg_close3),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.configure',
            @stdimg_configure,
      sizeof(stdimg_configure),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.document',
            @stdimg_document,
      sizeof(stdimg_document),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.exit',
            @stdimg_exit,
      sizeof(stdimg_exit),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.find',
            @stdimg_find,
      sizeof(stdimg_find),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.folder',
            @stdimg_folder,
      sizeof(stdimg_folder),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.foldernew',
            @stdimg_foldernew,
      sizeof(stdimg_foldernew),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.folderopen',
            @stdimg_folderopen,
      sizeof(stdimg_folderopen),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.folderup',
            @stdimg_folderup,
      sizeof(stdimg_folderup),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.new',
            @stdimg_new,
      sizeof(stdimg_new),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.no',
            @stdimg_no,
      sizeof(stdimg_no),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.ok',
            @stdimg_ok,
      sizeof(stdimg_ok),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.open',
            @stdimg_open,
      sizeof(stdimg_open),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.save',
            @stdimg_save,
      sizeof(stdimg_save),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.yes',
            @stdimg_yes,
      sizeof(stdimg_yes),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.edit',
            @stdimg_edit,
      sizeof(stdimg_edit),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.delete',
            @stdimg_delete,
      sizeof(stdimg_delete),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.hidden',
            @stdimg_hidden,
      sizeof(stdimg_hidden),
            0,0 );

  img := GfxLibAddBMP(
            'stdimg.link',
            @stdimg_link,
      sizeof(stdimg_link)
                     );

  img := GfxLibAddMaskedBMP(
            'stdimg.dlg.help',
            @stdimg_dlg_help,
      sizeof(stdimg_dlg_help),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.dlg.info',
            @stdimg_dlg_info,
      sizeof(stdimg_dlg_info),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.dlg.warning',
            @stdimg_dlg_warning,
      sizeof(stdimg_dlg_warning),
            0,0 );

  img := GfxLibAddMaskedBMP(
            'stdimg.dlg.critical',
            @stdimg_dlg_critical,
      sizeof(stdimg_dlg_critical),
            0,0 );

{
  img := GfxLibAddMaskedBMP(
            'stdimg.',
            @stdimg_,
      sizeof(stdimg_),
            0,0 );
}
end;

end.

