{ Copyright (c) 2003, Nagy Viktor

 Small dictionary program which uses special formatted text files
}
program app_dic;

{$APPTYPE GUI}

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel,
  wglistbox, wgchoicelist,
  gfxdialogs;

type
  TDicForm = class(TGfxForm)
  public
    l1,l2,l3,l4,l5 : TwgLabel;
    chlMode  : TwgChoiceList;
    chlDic   : TwgChoiceList;
    edSearch : TwgEdit;

    lbList : TwgTextListBox;

    procedure AfterCreate; override;

    procedure SearchKeyPress(Sender : TObject; var keycode: word; var shiftstate: word; var consumed : boolean);

    procedure StartSearch;

  end;

{ TDicForm }

procedure TDicForm.AfterCreate;
var
  x,y,w : integer;
  gap : integer;
begin
  inherited AfterCreate;

  WindowTitle8 := 'Small dictionary';

  SetDimensions(10,10,320,400);
{$ifdef FPC}
  SizeParams.min_width := 200;
  SizeParams.min_height := 250;
{$endif}
  //SetMinSize;
  //WindowPosition := wpAuto;

  gap := 50;
  y := 10;
  x := 10;

  w := 300;

  l1 := CreateLabel(self, x,y, 'Dictionary file:');
  chlDic := CreateChoiceList(self, 10,y+20, w, nil);
{$ifdef Win32}
  chlDic.Items.Add(Str8to16('ENG-HUN.TXT'));
  chlDic.Items.Add(Str8to16('HUN-ENG.TXT'));
{$else}
  chlDic.Items.Add(Str8to16('/data/dict/ENG-HUN.TXT'));
  chlDic.Items.Add(Str8to16('/data/dict/HUN-ENG.TXT'));
{$endif}
  chlDic.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l2 := CreateLabel(self, x,y, 'Search mode:');
  chlMode := CreateChoiceList(self, 10,y+20, w, nil);
  chlMode.Items.Add(Str8to16('Any match'));
  chlMode.Items.Add(Str8to16('From the beginning'));
  chlMode.Items.Add(Str8to16('Whole words'));
  chlMode.FocusItem := 1;
  chlMode.Anchors := [anLeft,anTop,anRight];

  inc(y,gap);
  l2 := CreateLabel(self, x,y, 'Search text:');
  edSearch := CreateEdit(self, 10,y+20, w, 0);
  edSearch.Anchors := [anLeft,anTop,anRight];
  //edSearcg.Text := Str8to16('127.0.0.1');
  edSearch.OnKeyPress := {$ifdef FPC}@{$endif}SearchKeyPress;

  inc(y,30);
  lbList := TwgTextListBox.Create(self);
  lblist.Left := x;
  lblist.Top := y+20;
  lbList.Width := w;
  lblist.height := height - lblist.top - 10;
  lblist.Anchors := [anLeft,anTop,anRight,anBottom];

  lbList.Items.Add(str8to16('Press Enter or F9 to search'));

  ActiveWidget := edSearch;

end;

procedure TDicForm.SearchKeyPress(Sender : TObject; var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  consumed := True;
  case keycode of
  KEY_F9, KEY_ENTER:
    begin
      writeln('starting search: ',edSearch.Text8);
      StartSearch;
    end;
  KEY_ESC:
    begin
      edSearch.Text := '';
      edSearch.Repaint;
      consumed := True;
    end;
  else
    consumed := false;
  end;

end;

procedure TDicForm.StartSearch;
var
  f : TextFile;
  s,ss : string;
  sp,p : integer;
  n : integer;
begin
  lblist.Items.Clear;
  lblist.Update;
  lblist.FocusItem := 1;
  gfxFlush;
  AssignFile(f, chlDic.Text8);
  try
    Reset(f);
    ss := edSearch.Text8;
    if chlMode.FocusItem = 3 then ss := ' '+ss+' ';

    ss := UpperCase(ss);
    //Writeln('search: ',ss);

    n := 0;
    while not eof(f) do
    begin
      readln(f,s);
      sp := pos('->',s);
      p := pos(ss,UpperCase(s));
      if ( p > 0) and (p < sp) and
         ((chlMode.FocusItem <> 2) or (p=2)) then
      begin
        lblist.Items.Add(str8to16(s));
        inc(n);
      end;
    end;
    CloseFile(f);
    WindowTitle8 := IntToStr(n)+' matches';
    if n < 1 then lblist.Items.Add(Str8to16('no match'));

    lblist.Update;
  except
    ShowMessage8('File read error.');
  end;
end;

var
  dicform : TDicForm;

begin
  GfxOpenDisplay('');

  Writeln('LPTK Dictionary');

  dicform := TDicForm.Create(nil);
  dicform.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;
end.

