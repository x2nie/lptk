{ Copyright (c) 2003, Nagy Viktor 

 Some utility functions
}

unit vfdutils;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox;

procedure SetWidgetText(wg : TWidget; txt : string16);
function GetWidgetText(wg : TWidget; var txt : string) : boolean;
  
implementation

procedure SetWidgetText(wg : TWidget; txt : string16);
begin
  if      wg is TGfxForm then TGfxForm(wg).WindowTitle := txt
  else if wg is TwgLabel then TwgLabel(wg).Text := txt
  else if wg is TwgEdit  then TwgEdit(wg).Text := txt
  else if wg is TwgMemo  then TwgMemo(wg).Text := txt
  else if wg is TwgButton then TwgButton(wg).Text := txt
  else if wg is TwgCheckBox then TwgCheckBox(wg).Text := txt
  ;
end;

function GetWidgetText(wg : TWidget; var txt : string) : boolean;
begin
  result := true;
  if wg is TGfxForm then txt := TGfxForm(wg).WindowTitle
  else if wg is TwgLabel then txt := TwgLabel(wg).Text
  else if wg is TwgEdit  then txt := TwgEdit(wg).Text
  else if wg is TwgMemo  then txt := TwgMemo(wg).Text
  else if wg is TwgButton then txt := TwgButton(wg).Text
  else if wg is TwgCheckBox then txt := TwgCheckBox(wg).Text
  else
  begin
    result := false;
    txt := '';
  end;
end;


end.
