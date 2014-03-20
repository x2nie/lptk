unit hd_stdimages;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  hd_main;

procedure pgfCreateStandardImages;

implementation

{$I stdimages.inc}

Const
  sb_arrow_down : Array[0..77] of byte = (
      66, 77, 78,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0,  7,  0,  0,  0,  4,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      16,  0,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,  0,  0,  0,  0,255,255,255,  0,238,  0,  0,  0,198,  0,
       0,  0,130,  0,  0,  0,  0,  0,  0,  0);

  sb_arrow_up : Array[0..77] of byte = (
      66, 77, 78,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0,  7,  0,  0,  0,  4,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      16,  0,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,  0,  0,  0,  0,255,255,255,  0,  0,  0,  0,  0,130,  0,
       0,  0,198,  0,  0,  0,238,  0,  0,  0);

  sb_arrow_left : Array[0..89] of byte = (
      66, 77, 90,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0,  4,  0,  0,  0,  7,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      28,  0,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,  0,  0,  0,  0,255,255,255,  0,224,  0,  0,  0,192,  0,
       0,  0,128,  0,  0,  0,  0,  0,  0,  0,128,  0,  0,  0,192,  0,  0,
       0,224,  0,  0,  0);

  sb_arrow_right : Array[0..89] of byte = (
      66, 77, 90,  0,  0,  0,  0,  0,  0,  0, 62,  0,  0,  0, 40,  0,  0,
       0,  4,  0,  0,  0,  7,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,
      28,  0,  0,  0, 19, 11,  0,  0, 19, 11,  0,  0,  2,  0,  0,  0,  2,
       0,  0,  0,  0,  0,  0,  0,255,255,255,  0,112,  0,  0,  0, 48,  0,
       0,  0, 16,  0,  0,  0,  0,  0,  0,  0, 16,  0,  0,  0, 48,  0,  0,
       0,112,  0,  0,  0);


procedure pgfCreateStandardImages;
var
  img : TpgfImage;
begin
  // system images

  img := pgfImages.AddBMP(
            'sys.sb.up',
            @sb_arrow_up,
      sizeof(sb_arrow_up) );

  img := pgfImages.AddBMP(
            'sys.sb.down',
            @sb_arrow_down,
      sizeof(sb_arrow_down) );

  img := pgfImages.AddBMP(
            'sys.sb.left',
            @sb_arrow_left,
      sizeof(sb_arrow_left) );

  img := pgfImages.AddBMP(
            'sys.sb.right',
            @sb_arrow_right,
      sizeof(sb_arrow_right) );


  // other more general purpose images:
  
  img := pgfImages.AddMaskedBMP(
            'stdimg.cancel',
            @stdimg_cancel,
      sizeof(stdimg_cancel),
            0,0 );
            
  //pgfImages.AddImage( 'STDIMG.NO', img );

  img := pgfImages.AddMaskedBMP(
            'stdimg.close',
            @stdimg_close,
      sizeof(stdimg_close),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.close2',
            @stdimg_close2,
      sizeof(stdimg_close2),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.close3',
            @stdimg_close3,
      sizeof(stdimg_close3),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.configure',
            @stdimg_configure,
      sizeof(stdimg_configure),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.document',
            @stdimg_document,
      sizeof(stdimg_document),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.exit',
            @stdimg_exit,
      sizeof(stdimg_exit),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.find',
            @stdimg_find,
      sizeof(stdimg_find),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.folder',
            @stdimg_folder,
      sizeof(stdimg_folder),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.foldernew',
            @stdimg_foldernew,
      sizeof(stdimg_foldernew),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.folderopen',
            @stdimg_folderopen,
      sizeof(stdimg_folderopen),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.folderup',
            @stdimg_folderup,
      sizeof(stdimg_folderup),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.new',
            @stdimg_new,
      sizeof(stdimg_new),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.no',
            @stdimg_no,
      sizeof(stdimg_no),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.ok',
            @stdimg_ok,
      sizeof(stdimg_ok),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.open',
            @stdimg_open,
      sizeof(stdimg_open),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.save',
            @stdimg_save,
      sizeof(stdimg_save),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.yes',
            @stdimg_yes,
      sizeof(stdimg_yes),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.edit',
            @stdimg_edit,
      sizeof(stdimg_edit),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.delete',
            @stdimg_delete,
      sizeof(stdimg_delete),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.hidden',
            @stdimg_hidden,
      sizeof(stdimg_hidden),
            0,0 );

  img := pgfImages.AddBMP(
            'stdimg.link',
            @stdimg_link,
      sizeof(stdimg_link)
                     );

  img := pgfImages.AddMaskedBMP(
            'stdimg.dlg.help',
            @stdimg_dlg_help,
      sizeof(stdimg_dlg_help),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.dlg.info',
            @stdimg_dlg_info,
      sizeof(stdimg_dlg_info),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.dlg.warning',
            @stdimg_dlg_warning,
      sizeof(stdimg_dlg_warning),
            0,0 );

  img := pgfImages.AddMaskedBMP(
            'stdimg.dlg.critical',
            @stdimg_dlg_critical,
      sizeof(stdimg_dlg_critical),
            0,0 );

{
  img := pgfImages.AddMaskedBMP(
            'stdimg.',
            @stdimg_,
      sizeof(stdimg_),
            0,0 );
}
end;

end.

