{ wgmemo.pas: Memo widget (multiline text editor)
  File maintainer: nvitya@freemail.hu

History:
    13.01.2004  buffer-support in repaint added (Erik Grohnwaldt)
    17.07.2003	read only property Cursorline added (Erik Grohnwaldt)
}
unit wgmemo;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, messagequeue, gfxbase, GfxStyle, GfxWidget, wgScrollBar;

type

  TwgMemo = class(TWidget)
  private
    FLines : TStringList;

    FMaxLength : integer;

    FCursorPos  : integer;
    FCursorLine : integer;

    FSideMargin : integer;

    FSelStartLine, FSelEndLine, FSelStartPos, FSelEndPos : integer;

    FSelecting : boolean;
    FMouseDragging : boolean;
    FMouseDragPos  : integer;

    FFont : TGfxFont;

    FDrawOffset : integer;

    FLineHeight : integer;

    FFirstLine : integer;

    FVScrollBar : TwgScrollBar;
    FHScrollBar : TwgScrollBar;

    FWrapping   : boolean;

    FLongestLineWidth : TGfxCoord;

    function GetFontName: string;
    procedure RecalcLongestLine;

    procedure DeleteSelection;

    procedure DoCopy;
    procedure DoPaste;

    procedure AdjustCursor;

    function LineCount : integer;
    function GetLineText(linenum : integer) : string;
    procedure SetFontName(const AValue: string);
    procedure SetLineText(linenum : integer; value : string);
    function GetCursorX : integer;
    procedure SetCPByX(x : integer);

    function CurrentLine : string16;

    function VisibleLines : integer;
    function VisibleWidth : integer;

    procedure VScrollBarMove(Sender: TObject; position : integer);
    procedure HScrollBarMove(Sender: TObject; position : integer);

    procedure SetText(const AValue : String16);
    procedure SetText8(const AValue : String);
    function GetText : string16;
    function GetText8 : string;

    procedure SetCursorLine(aValue : integer);
  protected

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    procedure HandleResize(dwidth, dheight : integer); override;

    procedure HandleWindowScroll(direction, amount : integer); override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure DoShow; override;

    procedure UpdateScrollBar;

    function SelectionText : string16;

    property LineHeight : integer read FLineHeight;

    property CursorLine : integer read FCursorLine write SetCursorLine;

    property Text : string16 read GetText write SetText;
    property Text8 : string read GetText8 write SetText8;

    property Font : TGfxFont read FFont;

  public

    OnChange : TNotifyEvent;

  published

    property Lines : TStringList read FLines;
    property FontName : string read GetFontName write SetFontName;

  end;

implementation

uses
  GfxClipboard;

{ TwgMemo }

procedure TwgMemo.SetCursorLine(aValue : integer);
var
    i : integer;
    MaxLine : integer;
    yp : integer;
begin
    if (aValue < 1) or (aValue = FCursorLine) then exit; // wrong value
    if aValue < FFirstLine then
    begin
	FFirstLine := aValue; // moves the selected line to the top of the displayed rectangle
	FCursorLine := aValue;
	FCursorPos := 0;
	RePaint;
	exit;
    end;
    yp := 2;
    MaxLine := 0;
    for i := FFirstLine to LineCount do
    begin
	yp := yp + LineHeight;
	if yp > Height then
	begin
	    MaxLine := i - 1;
	    break;
	end;
    end;
    if MaxLine < aValue then
    begin
	FFirstLine := aValue;
	FCursorLine := aValue;
	FCursorPos := 0;
	RePaint;
	exit;
    end
    else
    begin
	FCursorLine := aValue;
	FCursorPos := 0;
	RePaint;
	exit;
    end;
end;

constructor TwgMemo.Create(AOwner : TComponent);
begin
  inherited;
  Focusable := true;
  FFont := GfxGetFont('#Edit1');
  FHeight := FFont.Height*3 + 4;
  FWidth := 120;
  FLineHeight := FFont.Height + 2;
  FBackgroundColor := $FFFFFF;

  FSelecting := false;
  FSideMargin := 3;
  FMaxLength := 0;

  OnChange := nil;

  FLines := TStringList.Create;
  FFirstLine := 1;
  FCursorLine := 1;

  FCursorPos   := 0;
  FSelStartPos := FCursorPos;
  FSelEndPos   := 0;
  FSelStartLine := 0;
  FSelEndLine   := 0;

  FDrawOffset := 0;
  FMouseDragging := false;

  FVScrollBar := TwgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := {$ifdef FPC}@{$endif}VScrollBarMove;
  FHScrollBar := TwgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := {$ifdef FPC}@{$endif}HScrollBarMove;

  FMouseCursor := CUR_EDIT;

  FWrapping := false;
end;

destructor TwgMemo.Destroy;
begin
  FLines.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TwgMemo.RecalcLongestLine;
var
  n : integer;
  lw : TGfxCoord;
begin
  FLongestLineWidth := 0;
  for n:=1 to LineCount do
  begin
    lw := FFont.TextWidth16(getlinetext(n));
    if lw > FlongestLineWidth then FlongestLineWidth := lw;
  end;
end;

function TwgMemo.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgMemo.DeleteSelection;
var
  n : integer;
  selsl, selsp, selel, selep : integer;

  ls  : string16;
  len : integer;
  st  : integer;
begin

  if FSelEndLine < 1 then Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine; selsp := FSelStartPos;
    selel := FSelEndLine;   selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine; selep := FSelStartPos;
    selsl := FSelEndLine;   selsp := FSelEndPos;
  end;

  for n := selsl to selel do
  begin
    ls := GetLineText(n);

    if selsl < n then st := 0 else st := selsp;
    if selel > n then len := Length16(ls) else len := selep - st;

    Delete16(ls,st+1,len);

    SetLineText(n,ls);

  end;

  if selsl < selel then
  begin
    ls := GetlineText(selsl);
    ls := ls + GetLineText(selel);
    SetLineText(selsl, ls);
  end;

  for n := selsl+1 to selel do
  begin
    FLines.Delete(selsl);
  end;

  FCursorPos := selsp;
  FCursorLine := selsl;

  FSelEndLine := 0;
end;

procedure TwgMemo.DoCopy;
var
  n : integer;
  selsl, selsp, selel, selep : integer;

  ls  : string16;
  len : integer;
  st  : integer;

  s : string;
begin

  if FSelEndLine < 1 then Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine; selsp := FSelStartPos;
    selel := FSelEndLine;   selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine; selep := FSelStartPos;
    selsl := FSelEndLine;   selsp := FSelEndPos;
  end;

  s := '';

  for n := selsl to selel do
  begin

    if n > selsl then s := s + LINEFEEDSTRING;

    ls := GetLineText(n);

    if selsl < n then st := 0 else st := selsp;
    if selel > n then len := Length16(ls) else len := selep - st;

    s := s + Str16to8(copy16(ls,st+1,len));
  end;

  SetClipboardText(s);
end;

procedure TwgMemo.DoPaste;
var
  s, si, si8, lineend : string16;
  n, l : integer;
  lcnt : integer;

begin
  DeleteSelection;
  s := GetClipboardText;

  si := Copy16(CurrentLine,1,FCursorPos);
  lineend := Copy16(CurrentLine,FCursorPos+1, length(CurrentLine));
  l := FCursorLine;
  n := 1;
  lcnt := 0;
  si8 := '';
  while n <= length(s) do
  begin
    if (s[n] = #13) or (s[n] = #10) then
    begin
      if lcnt = 0 then SetLineText(l, si + u8(si8))
                  else FLines.Insert(l-1, si + u8(si8));

      si := '';
      si8 := '';
      inc(lcnt);
      inc(l);

      // skip multibyte line end:
      if (s[n]=#13) and (n < length(s)) and (s[n+1]=#10) then inc(n);
    end
    else
    begin
      si8 := si8 + s[n];
    end;
    inc(n);
  end;

  si := si + u8(si8);

  FCursorPos := length16(si);
  si := si + lineend;

  if lcnt = 0 then
  begin
    SetLineText(l, si)
  end
  else
  begin
    FLines.Insert(l-1, si);
    FCursorLine := l;
  end;

  AdjustCursor;
  Repaint;
end;

procedure TwgMemo.AdjustCursor;
var
  tw : integer;
begin

  // horizontal adjust
  RecalcLongestLine;
  tw := FFont.TextWidth16(copy16(CurrentLine,1,FCursorPos));

  if tw - FDrawOffset > VisibleWidth - 2 then
  begin
    FDrawOffset := tw - VisibleWidth + 2;
  end
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then dec(FDrawOffset, 2);
  end;

  // vertical adjust

  if FCursorLine < FFirstLine then FFirstLine := FCursorLine;
  if FCursorline - FFirstLine + 1 > VisibleLines then FFirstLine := FCursorline - VisibleLines + 1;

  if FFirstLine + VisibleLines > LineCount then
  begin
    FFirstLine := LineCount - VisibleLines + 1;
    if FFirstline < 1 then FFirstLine := 1;
  end;

  UpdateScrollbar;

end;

procedure TwgMemo.UpdateScrollBar;
var
  vlines : integer;
  vsbw,x   : integer;
  hsbwas, vsbwas, vsbvis : boolean;
begin
  hsbwas := FHScrollBar.Visible;
  vsbwas := FVScrollBar.Visible;

  vlines := (Height - (FSideMargin shl 1)) div Lineheight;

  vsbvis := (LineCount > vlines);

  if vsbvis then vsbw := FVScrollBar.width else vsbw := 0;

  FHScrollBar.Visible := FLongestLineWidth > (Width - vsbw - FSideMargin*2) - 1;

  if FHScrollBar.Visible and not vsbvis then
  begin
    // recheck vertical scrollbar
    vlines := (Height - (FSideMargin shl 1) - FHScrollBar.Height) div Lineheight;
    vsbvis := (LineCount > vlines);
  end;
  
  FVScrollBar.Visible := vsbvis;
  
  if FHScrollBar.Visible then
  begin
    if not FVScrollBar.Visible then x := Width else x := Width - FVscrollBar.Width;
    if x <> FHScrollBar.Width then
    begin
      FHScrollBar.Width := x;
      FHScrollBar.UpdateWindowPosition;
    end;
  end;

  if FHScrollBar.Visible then
  begin
    FHScrollBar.Min := 0;
    FHScrollBar.Max := FLongestLineWidth - VisibleWidth - 1;
    if (FLongestLineWidth <= 0) or (FLongestLineWidth <= VisibleWidth)
      then FHScrollBar.SliderSize := 1
      else FHScrollBar.SliderSize := VisibleWidth / FLongestLineWidth;
    FHScrollBar.Position := FDrawOffset;
    FHScrollBar.RepaintSlider;
  end;

  if FVScrollBar.Visible then
  begin
    FVScrollBar.Min := 1;
    FVScrollBar.SliderSize := VisibleLines / LineCount;
    FVScrollBar.Max := LineCount-VisibleLines+1;
    FVScrollBar.Position := FFirstLine;

    FVScrollBar.RePaintSlider;
  end;

  if (hsbwas <> FHScrollBar.Visible) or (vsbwas <> FVScrollBar.Visible) then
  begin
    AdjustCursor;
  end;

end;

function TwgMemo.LineCount: integer;
begin
  result := FLines.Count;
end;

function TwgMemo.GetLineText(linenum: integer): string;
begin
  if LineCount < 1 then FLines.Add('');
  if (linenum >= 1) and (linenum <= LineCount)
    then result := FLines.Strings[linenum-1]
    else result := '';
end;

procedure TwgMemo.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

procedure TwgMemo.SetLineText(linenum: integer; value: string);
begin
  FLines.Strings[linenum-1] := value;
end;

function TwgMemo.GetCursorX: integer;
begin
  result := FFont.TextWidth16(copy16(CurrentLine,1,FCursorPos));
end;

procedure TwgMemo.SetCPByX(x: integer);
var
  s : string16;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  ls : string16;
begin
  // searching the appropriate character position

  ls := CurrentLine;

  cpx := FFont.TextWidth16(copy16(ls,1,FCursorPos)); // + FDrawOffset + FSideMargin;
  cp := FCursorPos;
  if cp > Length16(ls) then cp := Length16(ls);

  s := '';

  for n := 0 to Length16(ls) do
  begin
    cx := FFont.TextWidth16(copy16(ls,1,n)); // + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  FCursorPos := cp;
end;

function TwgMemo.CurrentLine: string16;
begin
  result := GetLineText(FCursorLine);
end;

function TwgMemo.VisibleLines: integer;
var
  sh : integer;
begin
  if FHScrollBar.Visible then sh := 18 else sh := 0;
  result := (Height - (FSideMargin shl 1) - sh) div Lineheight;
end;

function TwgMemo.VisibleWidth: integer;
var
  sw : integer;
begin
  if FVScrollBar.Visible then sw := FVScrollBar.width else sw := 0;
  result := (Width - (FSideMargin shl 1) - sw);
end;

procedure TwgMemo.DoShow;
begin
  FVScrollBar.SetDimensions(width-18,0,18,height);
  FHScrollBar.SetDimensions(0,height-18,width-18,18);
//  FHScrollBar.Visible := false;
  inherited DoShow;
  RecalcLongestLine;
  UpdateScrollBar;
end;

procedure TwgMemo.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstLine <> position then
  begin
    FFirstLine := position;
    repaint;
  end;
end;

procedure TwgMemo.HScrollBarMove(Sender: TObject; position: integer);
begin
  if position <> FDrawOffset then
  begin
    FDrawOffset := position;
    Repaint;
  end;
end;

procedure TwgMemo.RePaint;
var
  n : integer;
  tw, tw2, st, len : integer;
  yp : integer;
  ls : string16;
  r : TGfxRect;
  selsl, selsp, selel, selep : integer;
begin
  if WinHandle <= 0 then Exit;
  //inherited RePaint;

  Canvas.DrawOnBuffer := True;
  
  Canvas.ClearClipRect;
  DrawControlFrame(canvas,0,0,width,height);
{
  if Focused then
  begin
    Canvas.SetColor(clWidgetFrame);
    Canvas.DrawRectangle(0,0,width,height);
  end;
}
  r.Left := 2;
  r.Top  := 2;
  r.width := width - 4;
  r.height := height - 4;
  canvas.SetClipRect(r);

  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectAngle(2,2,width-4,height-4);

  Canvas.SetFont(FFont);

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine; selsp := FSelStartPos;
    selel := FSelEndLine;   selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine; selep := FSelStartPos;
    selsl := FSelEndLine;   selsp := FSelEndPos;
  end;

  yp := 3;
  for n := FFirstline to LineCount do
  begin
    ls := GetLineText(n);
    Canvas.DrawString16(- FDrawOffset + FSideMargin, yp, ls);

    if Focused then
    begin
      // drawing selection
      if (FSelEndLine > 0) and (selsl <= n) and (selel >= n) then
      begin
        if selsl < n then st := 0 else st := selsp;
        if selel > n then len := Length16(ls) else len := selep - st;

        tw := FFont.TextWidth16(copy16(ls,1,st));
        tw2 := FFont.TextWidth16(copy16(ls,1,st+len));
        Canvas.DrawSelectionRectangle(- FDrawOffset + FSideMargin + tw, yp, tw2-tw, LineHeight);
      end;

      //drawing cursor
      if FCursorLine = n then
      begin
        // drawing cursor
        tw := FFont.TextWidth16(copy16(ls,1,FCursorPos));
        Canvas.SetColor(clTextCursor);
        Canvas.FillRectangle(- FDrawOffset + FSideMargin + tw, yp, 2, FFont.Ascent+FFont.Descent);
      end;
    end;

    yp := yp + LineHeight;

    if yp > Height then Break;
  end;

  Canvas.SwapBuffer;
//  RepaintChildren;
end;

procedure TwgMemo.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  prevval : string16;
  s, ls, ls2 : string16;
  cx : integer;

  procedure StopSelection;
  begin
    FSelStartLine := FCursorLine;
    FSelStartPos  := FCursorPos;
    FSelEndLine := 0;
  end;

begin
  inherited;

  //writeln('wgmemo.keypress: ',IntToHex(keycode,4), ' shift: ',IntToHex(shiftstate,4));

  prevval := Text;

  s := chr(keycode and $00FF) + chr((keycode and $FF00) shr 8);

  Consumed := true;
  case GfxCheckClipBoardKey(keycode, shiftstate) of
    ckCopy:   DoCopy;
    ckPaste:  DoPaste;
    ckCut:    //if FSelEndLine > 0 then
              begin
                DoCopy;
                DeleteSelection;
              end;
  else
    Consumed := false;
  end;

  if not Consumed and (keycode and $8000 = $8000) then
  begin

    // checking for movement keys:
    consumed := true;
    FSelecting := (shiftstate and ss_shift) <> 0;

    case keycode of
      KEY_LEFT:
              begin  // left
                if FCursorPos > 0 then
                begin
                  dec(FCursorPos);

                  if (shiftstate and ss_control) <> 0 then
                  begin
                    // word search...
                    while (FCursorPos > 0) and not GfxIsAlphaNum16(copy16(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);

                    while (FCursorPos > 0) and GfxIsAlphaNum16(copy16(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);
                  end;

                end;
              end;
      KEY_RIGHT:
              begin  // right
                if FCursorPos < length16(CurrentLine) then
                begin
                  inc(FCursorPos);

                  if (shiftstate and ss_control) <> 0 then
                  begin
                    // word search...
                    while (FCursorPos < length16(CurrentLine)) and GfxIsAlphaNum16(copy16(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);

                    while (FCursorPos < length16(CurrentLine)) and not GfxIsAlphaNum16(copy16(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);
                  end;

                end;
              end;
      KEY_UP:
              begin  // up
                cx := GetCursorX;
                if FCursorLine > 1 then
                begin
                  dec(FCursorline);
                  SetCPByX(cx);
                end;
              end;

      KEY_DOWN:
              begin  // down
                cx := GetCursorX;
                if FCursorLine < LineCount then
                begin
                  inc(FCursorline);
                  SetCPByX(cx);
                end;
              end;
      KEY_HOME:
              begin  // home
                FCursorPos := 0;
              end;
      KEY_END:
              begin  // end
                FCursorPos := length16(CurrentLine);
              end;
      KEY_PGUP:
              begin // pgup
                if FCursorLine > 1 then
                begin
                  cx := GetCursorX;
                  dec(FCursorLine,VisibleLines);
                  if FCursorLine < 1 then FCursorLine := 1;
                  SetCPByX(cx);
                end;
              end;
      KEY_PGDN:
              begin // pgdown
                cx := GetCursorX;
                if FCursorLine < LineCount then
                begin
                  inc(FCursorline, VisibleLines);
                  if FCursorLine > LineCount then FCursorLine := LineCount;
                  SetCPByX(cx);
                end;
             end;

    else
      Consumed := false;
    end;

    if Consumed then
    begin
      AdjustCursor;

      if FSelecting then
      begin
        FSelEndPos  := FCursorPos;
        FSelEndLine := FCursorLine;
      end
      else
      begin
        StopSelection;
      end;
    end;

    if not Consumed then
    begin
      consumed := true;

      case keycode of
        KEY_ENTER:
                begin // enter
                  ls := Copy16(FLines[FCursorline-1],1,FCursorPos);
                  ls2 := Copy16(FLines[FCursorline-1],FCursorPos+1,Length16(FLines[FCursorline-1]));
                  FLines.Insert(FCursorLine-1,ls);
                  inc(FCursorLine);
                  SetLineText(FCursorLine, ls2);
                  FCursorPos := 0;
                end;
        KEY_BACKSPACE:
                begin // backspace
                  if FCursorPos > 0 then
                  begin
                    ls := GetLineText(FCursorLine);
                    Delete16(ls,FCursorPos,1);
                    SetLineText(FCursorLine,ls);
                    dec(FCursorPos);
                  end
                  else if FCursorLine > 1 then
                  begin
                    ls := CurrentLine;
                    FLines.Delete(FCursorLine-1);
                    dec(FCursorLine);
                    FCursorPos := Length16(FLines.Strings[FCursorLine-1]);
                    FLines.Strings[FCursorLine-1] := FLines.Strings[FCursorLine-1] + ls;
                  end;
                end;
      KEY_DELETE:
              begin // del
                  ls := GetLineText(FCursorLine);
                  if FSelEndLine > 0 then
                  begin
                    DeleteSelection;
                  end
                  else
                  if FCursorPos < length16(ls) then
                  begin
                    Delete16(ls,FCursorPos+1,1);
                    SetLineText(FCursorLine,ls);
                  end
                  else if FCursorLine < LineCount then
                  begin
                    ls2 := FLines.Strings[FCursorLine];
                    FLines.Delete(FCursorLine);
                    FLines.Strings[FCursorLine-1] := ls + ls2;
                  end;
                end;
{
        $FF1B:  begin // ESC
                  halt(0);
                end;
}
      else
        consumed := false;
      end;

      if Consumed then
      begin
        StopSelection;
        AdjustCursor;
      end;

    end;

  end
  else if not Consumed then
  begin
    // printeable
    //FText := FText + s;

    if (FMaxLength <= 0) or (length16(FLines.Text) < FMaxLength) then
    begin
      DeleteSelection;
      ls := GetLineText(FCursorLine);
      insert16(s,ls,FCursorPos+1);
      SetLineText(FCursorLine,ls);
      inc(FCursorPos);
      FSelStartPos := FCursorPos;
      FSelStartLine := FCursorLine;
      FSelEndLine := 0;
      AdjustCursor;
    end;

    consumed := true;
  end;

  if prevval <> Text then
  begin
    if Assigned(OnChange) then OnChange(self);
  end;

  if consumed then RePaint;

end;

procedure TwgMemo.HandleMouseDown(x, y : integer; button : word; shiftstate : word);
var
  s : string16;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  lnum : integer;
  ls : string16;
begin
  inherited HandleMouseDown(x, y, button, shiftstate);

  // searching the appropriate character position

  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then lnum := LineCount;
  ls := GetLineText(lnum);

  cpx := FFont.TextWidth16(copy16(ls,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(ls) do
  begin
    cx := FFont.TextWidth16(copy16(ls,1,n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  FMouseDragging := true;
  FMouseDragPos := cp;

  FCursorPos  := cp;
  FCursorLine := lnum;

  if (shiftstate and ss_shift) <> 0 then
  begin
    FSelEndLine := lnum;
    FSelEndpos  := cp;
  end
  else
  begin
    FSelStartLine := lnum;
    FSelStartPos  := cp;
    FSelEndLine := 0;
  end;
  Repaint;
end;

procedure TwgMemo.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
end;

procedure TwgMemo.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  s : string16;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  lnum : integer;
  ls : string16;
begin
  if not FMouseDragging or ((btnstate and 1) = 0) then
  begin
    FMouseDragging := false;
    Exit;
  end;

  // searching the appropriate character position

  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then lnum := LineCount;
  ls := GetLineText(lnum);

  cpx := FFont.TextWidth16(copy16(ls,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(ls) do
  begin
    cx := FFont.TextWidth16(copy16(ls,1,n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  if (cp <> FCursorPos) or (lnum <> FCursorLine) then
  begin
    FCursorLine := lnum;
    FSelEndLine := lnum;
    FSelEndPos  := cp;
    FCursorPos := cp;
    Repaint;
  end;


  // searching the appropriate character position
{
  cpx := FFont.TextWidth16(copy16(FText,1,FCursorPos)) + FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(Text) do
  begin
    cx := FFont.TextWidth16(copy16(Text,1,n)) + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  //FMouseDragPos := cp;
  FSelOffset := cp-FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
    Repaint;
  end;
}
end;

procedure TwgMemo.HandleWindowScroll(direction, amount: integer);
var
  pfl, pdo : integer;
begin
  inherited HandleWindowScroll(direction, amount);

  pfl := FFirstLine;
  pdo := FDrawOffset;

  if direction = 0 then
  begin
    dec(FFirstLine, amount);
  end;
  if direction = 1 then
  begin
    inc(FFirstLine, amount);
  end;
  if FFirstLine > LineCount - VisibleLines + 1 then FFirstLine := LineCount - VisibleLines + 1;
  if FFirstLine < 1 then FFirstLine := 1;

  if FHScrollBar.Visible then
  begin
    if Direction = 2 then
    begin
      dec(FDrawOffset, amount*16);
    end;
    if Direction = 3 then
    begin
      inc(FDrawOffset, amount*16);
    end;

    if FDrawOffset > FHScrollBar.Max then FDrawOffset := FHScrollBar.Max;
    if FDrawOffset < 0 then FDrawOffset := 0;
  end;

  if (pfl <> FFirstLine) or (pdo <> FDrawOffset) then
  begin
    UpdateScrollBar;
    Repaint;
  end;

end;

procedure TwgMemo.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);

  FVScrollBar.Height := Height;
  FVScrollBar.left := Width - FVScrollBar.width;
  FVScrollBar.UpdateWindowPosition;

  FHScrollBar.Top := height-FHScrollBar.Height;
  FHScrollBar.Width := width-FVScrollBar.Width;
  FHScrollBar.UpdateWindowPosition;

  UpdateScrollBar;
end;

function TwgMemo.SelectionText: string16;
begin
{
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Result := Copy16(FText,1+FSelStart + FSelOffset,-FSelOffset);
    end
    else
    begin
      result := Copy16(FText,1+FSelStart,FSelOffset);
    end;
  end
  else Result := '';
}
end;

function TwgMemo.GetText8: string;
var
  n : integer;
  s : string;
begin
  s := '';
  for n:=1 to LineCount do
  begin
    if n > 1 then s := s + #13#10;
    s := s + Str16To8(GetLineText(n));
  end;
  result := s;
end;

function TwgMemo.GetText: string16;
var
  n : integer;
  s : string;
begin
  s := '';
  for n:=1 to LineCount do
  begin
    if n > 1 then s := s + #13#10;
    s := s + GetLineText(n);
  end;
  result := s;
end;

procedure TwgMemo.SetText(const AValue : String16);
var
  n : integer;
  c : string[2];
  s : string16;
begin
  FLines.Clear;
  s := '';
  n := 1;
  while n <= length16(AValue) do
  begin
    c := copy16(AValue,n,1);
    if (c[1] = #13) or (c[1] = #10) then
    begin
      FLines.Add(s);
      s := '';
      c := copy16(AValue,n+1,1);
      if c[1] = #10 then inc(n);
    end
    else s := s + c;
    inc(n)
  end;

  if s <> '' then FLines.Add(s);

  FDrawOffset := 0;
  FCursorPos  := 0;
  FCursorLine := 1;
  FSelStartLine := FCursorLine;
  FSelStartPos  := FCursorPos;
  FSelEndLine := 0;

  AdjustCursor;
  Repaint;
end;

procedure TwgMemo.SetText8(const AValue: String);
begin
  text := u8(avalue);
end;

end.

