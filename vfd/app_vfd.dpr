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
  vfddesigner,
  vfdfile,
  vfdresizer,
  vfdforms,
  vfdutils,
  vfdmain,
  vfdformparser,
  vfdeditors;

//var
//  vf : TVFDFile;

begin
  writeln('LPTK Visual Form Designer');

//  vf := TVFDFile.Create;
//  vf.LoadFile('./aanewform.pas');
//  vf.GetBlocks;
//  Writeln('Block count: ',vf.BlockCount);
  //Writeln(vf.MergeBlocks);

  GfxOpenDisplay('');

  maindsgn := TMainDesigner.Create;
  maindsgn.CreateWindows;

  if ParamStr(1) <> '' then MainForm.edFormFile.Text8 := ParamStr(1);

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

