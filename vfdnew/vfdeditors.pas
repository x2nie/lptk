{ Copyright (c) 2003, Nagy Viktor

 $Id$
 
 Some property editors
}

unit vfdeditors;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wgcustomgrid, wgdbgrid, wgcheckbox, vfdforms;

type
  TItemEditorForm = class(TVFDDialog)
  public
    l1 : TwgLabel;

    edItems : TwgMemo;

    btnOK, btnCancel : TwgButton;

    procedure AfterCreate; override;

    procedure OnButtonClick(sender : TObject);

  end;

implementation

uses sqldb;

{ TItemEditorForm }

procedure TItemEditorForm.AfterCreate;
begin
  inherited;
//  WindowPosition := wpUser;

  WindowTitle8 := 'Items';

  SetDimensions(0,0,360,230);

  l1 := CreateLabel(self, 8,4, 'Items:');

  edItems := TwgMemo.Create(self);
  with edItems do
  begin
    SetDimensions(8,24,344,168);
    Anchors := [anLeft,anTop,anRight,anBottom];
  end;

  btnOK := CreateButton(self,8,200,105, 'OK', {$ifdef FPC}@{$endif}OnButtonClick);
  btnOK.Anchors := [anLeft, anBottom];
  btnCancel := CreateButton(self,244,200,105, 'Cancel', {$ifdef FPC}@{$endif}OnButtonClick);
  btnCancel.Anchors := [anRight, anBottom];

end;

procedure TItemEditorForm.OnButtonClick(sender: TObject);
begin
  if Sender = btnOK then ModalResult := 1 else ModalResult := 2;
end;

{
  $Log$
  Revision 1.3  2004/05/06 17:35:20  nvitya
  full functional VFD!

  Revision 1.2  2004/05/06 12:21:34  nvitya
  big update, external property editor added

  Revision 1.1  2004/05/05 01:35:32  nvitya
  new vfd branch

  Revision 1.6  2004/04/26 10:25:08  nvitya
  look corrections

  Revision 1.5  2003/11/10 00:01:50  nvitya
  using wgCustomGrid

}

end.
