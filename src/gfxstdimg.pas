unit gfxstdimg;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase;
  
procedure GfxCreateStandardImages;

implementation

{$I stdimg_ok.inc}
{$I stdimg_cancel.inc}

procedure GfxCreateStandardImages;
begin
  GfxLibAddMaskedBMP( 'STDIMG.OK',
                      @stdimg_ok,
                sizeof(stdimg_ok),
                      0,0 );
                
  GfxLibAddMaskedBMP( 'STDIMG.CANCEL',
                      @stdimg_cancel,
                sizeof(stdimg_cancel),
                      0,0 );
end;

end.

