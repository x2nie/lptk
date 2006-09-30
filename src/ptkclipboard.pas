{ gfxclipboard.pas: clipboard handling
  File maintainer: nvitya@freemail.hu

History:
}
unit ptkclipboard;

{$include lptk_config.inc}

interface

uses
  Classes, SysUtils, lptk, ptkwidget;

var
  ClipboardWindow : TptkWinHandle;
  WaitingForSelection : boolean;

  ClipBoardData : string;

procedure InitClipBoard;
function GetClipboardText : string;
function GetClipboardText16 : widestring;

procedure SetClipboardText(value : string);
procedure SetClipboardText16(value : widestring);

implementation

{$ifdef Win32}
uses windows;
{$else}
uses X, Xlib;
{$endif}

procedure InitClipBoard;
begin
{$ifdef Win32}
  ClipboardWindow := Windows.CreateWindowEx(
    0,			// extended window style
    'LPTKWIN',				// registered class name
    nil,			// window name
    0,			// window style
    0,			// horizontal position of window
    0,			// vertical position of window
    10,			// window width
    10,			// window height
    0,			// handle to parent or owner window
    0,					// menu handle or child identifier
    MainInstance,			// handle to application instance
    nil       // window-creation data
    );
{$else}
  ClipboardWindow := XCreateSimpleWindow(Display, ptkRootWindow,  10,10,10,10, 0, 0, 0 );
{$endif}
end;

function GetClipboardText : string;
{$ifdef Win32}
var
  h : THANDLE;
  p : PChar;
begin
  Result := '';

  if not Windows.OpenClipboard(0) then Exit;

  h := GetClipboardData(CF_TEXT);

  if h <> 0 then
  begin
    p := Windows.GlobalLock(h);
    ClipBoardData := '';
    while p^ <> #0 do
    begin
      ClipBoardData := ClipBoardData + p^;
      inc(p);
    end;
    GlobalUnlock(h);
  end;
  CloseClipboard;

{$else}
begin
  XConvertSelection(display, xia_clipboard, XA_STRING, xia_clipboard, ClipboardWindow, 0);
  WaitingForSelection := true;

  // delivering the remaining messages
  ptkDeliverMessages;

  repeat
    WaitWindowMessage;
    ptkDeliverMessages;
  until not WaitingForSelection;

{$endif}
  result := ClipBoardData;
end;

function GetClipboardText16 : widestring;
var
  s : string;
begin
  s := GetClipboardText;
  result := s;
end;

procedure SetClipboardText(value : string);
begin
  Writeln('SetClipBoardText: ',value);
  ClipBoardData := value;

{$ifdef Win32}
  if OpenClipboard(ClipboardWindow) then
  begin
    EmptyClipboard();
    SetClipboardData(CF_TEXT, 0);
    CloseClipboard();
  end;
{$else}
  XSetSelectionOwner(display, xia_clipboard, ClipboardWindow, 0);
{$endif}
end;

procedure SetClipboardText16(value : widestring);
var
  s : string;
begin
  s := value;
  SetClipboardText(s);
end;

initialization
begin
  ClipboardWindow := 0;
  WaitingForSelection := false;
end;

end.

