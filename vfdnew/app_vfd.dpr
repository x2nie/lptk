{ Copyright (c) 2003, Nagy Viktor }

program app_vfd;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$APPTYPE CONSOLE}
{$endif}

uses
  SysUtils,
  Classes,
  inifiles,
  gfxbase,
  wgedit,
  unitkeys,
  schar16,
  gfxstyle,
  gfxwidget,
  gfxform,
  wglabel,
  wgbutton,
  wglistbox,
  wgmemo,
  wgchoicelist,
  wggrid,
  sqldb,
  sqluis,
  wgdbgrid,
  gfxdialogs,
  wgcheckbox,
  vfdfile,
  vfdresizer,
  vfdutils,
  vfdmain,
  vfdformparser,
  vfdeditors,
  vfdwidgetclass,
  vfdwidgets,
  vfdprops,
  vfddesigner,
  newformdesigner;

//var
  //vf : TVFDFile;
begin
  writeln('LPTK Visual Form Designer');

//  vf := TVFDFile.Create;
//  vf.LoadFile('./aanewform.pas');
//  vf.GetBlocks;
//  Writeln('Block count: ',vf.BlockCount);
  //Writeln(vf.MergeBlocks);

  GfxOpenDisplay('');

  RegisterWidgets;

{
  frmMain := TfrmMain.Create(nil);
  frmMain.Show;

  PropList := TPropertyList.Create;
  PropList.AddItem(TPropertyInteger.Create('Left'));
  PropList.AddItem(TPropertyInteger.Create('Top'));
  PropList.AddItem(TPropertyInteger.Create('Width'));
  PropList.AddItem(TPropertyInteger.Create('Height'));
  PropList.AddItem(TPropertyString16.Create('Text'));
  PropList.AddItem(TPropertyString8.Create('FontName'));
  //PropList.AddItem('Anchors',nil);
  //PropList.AddItem('Text',nil);

  frmProperties := TfrmProperties.Create(nil);
  frmProperties.Show;
}

  PropList := TPropertyList.Create;
  
  maindsgn := TMainDesigner.Create;
  maindsgn.CreateWindows;

  if ParamStr(1) <> '' then maindsgn.EditedFileName := ParamStr(1);

  maindsgn.OnLoadFile(maindsgn);


  //maindsgn.Show;

  repeat
    try
      GfxDoMessageLoop;
      break;
    except
      on e : Exception do ShowMessage8(e.message,'Exception');
    end;
  until false;

  //GfxDoMessageLoop;
  GfxCloseDisplay;

  writeln('Program finished.');
  readln;
end.

