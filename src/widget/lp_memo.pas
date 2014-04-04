unit lp_memo;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main,
  lp_utils, lp_widget,
  lp_scrollbar;

type

  TlpMemo = class(TpgfWidget)
  private
    FLines : TWideStringList;

    FMaxLength : integer;

    FCursorPos  : integer;
    FCursorLine : integer;

    FSideMargin : integer;

    FSelStartLine, FSelEndLine, FSelStartPos, FSelEndPos : integer;

    FSelecting : boolean;
    FMouseDragging : boolean;
    FMouseDragPos  : integer;

    FFont : TpgfFont;
    FBackgroundColor : TpgfColor;

    FDrawOffset : integer;

    FLineHeight : integer;

    FFirstLine : integer;

    FVScrollBar : TwgScrollBar;
    FHScrollBar : TwgScrollBar;

    FWrapping   : boolean;

    FLongestLineWidth : TpgfCoord;

    function GetFontDesc: string;
    procedure SetFontDesc(const AValue: string);
    procedure RecalcLongestLine;

    procedure DeleteSelection;

    procedure DoCopy;
    procedure DoPaste;

    procedure AdjustCursor;

    function LineCount : integer;
    function GetLineText(linenum : integer) : string;
    procedure SetLineText(linenum : integer; value : string);
    function GetCursorX : integer;
    procedure SetCPByX(x : integer);

    function CurrentLine : WideString;

    function VisibleLines : integer;
    function VisibleWidth : integer;

    procedure VScrollBarMove(Sender: TObject; position : integer);
    procedure HScrollBarMove(Sender: TObject; position : integer);

    procedure SetText(const AValue : WideString);
    procedure SetText8(const AValue : String);
    function GetText : WideString;
    function GetText8 : string;

    procedure SetCursorLine(aValue : integer);
  protected

    procedure HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleLMouseDown(x,y : integer; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    procedure HandleResize(dwidth, dheight : integer); override;

    //procedure HandleWindowScroll(direction, amount : integer); override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

    procedure HandleShow; override;

    procedure UpdateScrollBar;

    function SelectionText : WideString;

    property LineHeight : integer read FLineHeight;

    property CursorLine : integer read FCursorLine write SetCursorLine;

    property Text : WideString read GetText write SetText;
    property Text8 : string read GetText8 write SetText8;

    property Font : TpgfFont read FFont;

  public

    OnChange : TNotifyEvent;

  published

    property Lines : TWideStringList read FLines;
    property FontDesc : string read GetFontDesc write SetFontDesc;

  end;

implementation

//uses
//  pgfClipboard;

{ TwgMemo }

procedure TlpMemo.SetCursorLine(aValue : integer);
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

constructor TlpMemo.Create(AOwner : TComponent);
begin
  inherited;
  Focusable := true;
  FFont := pgfGetFont('#Edit1');
  FHeight := FFont.Height*3 + 4;
  FWidth := 120;
  FLineHeight := FFont.Height + 2;
  //FBackgroundColor := $FFFFFF;

  FSelecting := false;
  FSideMargin := 3;
  FMaxLength := 0;

  OnChange := nil;

  FLines := TWideStringList.Create;
  FFirstLine := 1;
  FCursorLine := 1;

  FCursorPos   := 0;
  FSelStartPos := FCursorPos;
  FSelEndPos   := 0;
  FSelStartLine := 0;
  FSelEndLine   := 0;
  
  FBackgroundColor := clBoxColor;

  FDrawOffset := 0;
  FMouseDragging := false;

  FVScrollBar := TwgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := VScrollBarMove;
  FHScrollBar := TwgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := HScrollBarMove;

  FCursor := crIBeam;

  FWrapping := false;
end;

destructor TlpMemo.Destroy;
begin
  FLines.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TlpMemo.RecalcLongestLine;
var
  n : integer;
  lw : TpgfCoord;
begin
  FLongestLineWidth := 0;
  for n:=1 to LineCount do
  begin
    lw := FFont.TextWidth(getlinetext(n));
    if lw > FlongestLineWidth then FlongestLineWidth := lw;
  end;
end;

function TlpMemo.GetFontDesc: string;
begin
  result := FFont.FontDesc;
end;

procedure TlpMemo.DeleteSelection;
var
  n : integer;
  selsl, selsp, selel, selep : integer;

  ls  : WideString;
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
    if selel > n then len := Length(ls) else len := selep - st;

    Delete(ls,st+1,len);

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

procedure TlpMemo.DoCopy;
var
  n : integer;
  selsl, selsp, selel, selep : integer;

  ls  : WideString;
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

    if n > selsl then s := s + #13#10;

    ls := GetLineText(n);

    if selsl < n then st := 0 else st := selsp;
    if selel > n then len := Length(ls) else len := selep - st;

    s := s + wsToUtf8(copy(ls,st+1,len));
  end;

  //SetClipboardText(s);
end;

procedure TlpMemo.DoPaste;
{
var
  s, si, si8, lineend : WideString;
  n, l : integer;
  lcnt : integer;
}
begin
  exit;
(*
  DeleteSelection;
  s := GetClipboardText;

  si := Copy(CurrentLine,1,FCursorPos);
  lineend := Copy(CurrentLine,FCursorPos+1, length(CurrentLine));
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

  FCursorPos := length(si);
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
*)
end;

procedure TlpMemo.AdjustCursor;
var
  tw : integer;
begin

  // horizontal adjust
  RecalcLongestLine;
  tw := FFont.TextWidth(copy(CurrentLine,1,FCursorPos));

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

procedure TlpMemo.UpdateScrollBar;
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

function TlpMemo.LineCount: integer;
begin
  result := FLines.Count;
end;

function TlpMemo.GetLineText(linenum: integer): string;
begin
  if LineCount < 1 then FLines.Add('');
  if (linenum >= 1) and (linenum <= LineCount)
    then result := FLines.Strings[linenum-1]
    else result := '';
end;

procedure TlpMemo.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

procedure TlpMemo.SetLineText(linenum: integer; value: string);
begin
  FLines.Strings[linenum-1] := value;
end;

function TlpMemo.GetCursorX: integer;
begin
  result := FFont.TextWidth(copy(CurrentLine,1,FCursorPos));
end;

procedure TlpMemo.SetCPByX(x: integer);
var
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  ls : WideString;
begin
  // searching the appropriate character position

  ls := CurrentLine;

  cpx := FFont.TextWidth(copy(ls,1,FCursorPos)); // + FDrawOffset + FSideMargin;
  cp := FCursorPos;
  if cp > Length(ls) then cp := Length(ls);

  for n := 0 to Length(ls) do
  begin
    cx := FFont.TextWidth(copy(ls,1,n)); // + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  FCursorPos := cp;
end;

function TlpMemo.CurrentLine: WideString;
begin
  result := GetLineText(FCursorLine);
end;

function TlpMemo.VisibleLines: integer;
var
  sh : integer;
begin
  if FHScrollBar.Visible then sh := 18 else sh := 0;
  result := (Height - (FSideMargin shl 1) - sh) div Lineheight;
end;

function TlpMemo.VisibleWidth: integer;
var
  sw : integer;
begin
  if FVScrollBar.Visible then sw := FVScrollBar.width else sw := 0;
  result := (Width - (FSideMargin shl 1) - sw);
end;

procedure TlpMemo.HandleShow;
begin
//  FVScrollBar.SetDimensions(width-18,0,18,height);
//  FHScrollBar.SetDimensions(0,height-18,width-18,18);
//  FHScrollBar.Visible := false;
  inherited HandleShow;
  RecalcLongestLine;
  UpdateScrollBar;
end;

procedure TlpMemo.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstLine <> position then
  begin
    FFirstLine := position;
    repaint;
  end;
end;

procedure TlpMemo.HScrollBarMove(Sender: TObject; position: integer);
begin
  if position <> FDrawOffset then
  begin
    FDrawOffset := position;
    Repaint;
  end;
end;

procedure TlpMemo.HandlePaint;
var
  n : integer;
  tw, tw2, st, len : integer;
  yp : integer;
  ls : WideString;
  r : TpgfRect;
  selsl, selsp, selel, selep : integer;
begin
  Canvas.BeginDraw;

  Canvas.ClearClipRect;

  Canvas.DrawControlFrame(0,0,width,height);

  Canvas.ClearClipRect;
  pgfstyle.DrawControlFrame(canvas,0,0,width,height);
  
  r.Left := 2;
  r.Top  := 2;
  r.width := width - 4;
  r.height := height - 4;
  canvas.SetClipRect(r);

  if Enabled
    then Canvas.SetColor(FBackgroundColor)
    else Canvas.SetColor(clWindowBackground);

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
    Canvas.DrawString(- FDrawOffset + FSideMargin, yp, ls);

    if Focused then
    begin
      // drawing selection
      if (FSelEndLine > 0) and (selsl <= n) and (selel >= n) then
      begin
        if selsl < n then st := 0 else st := selsp;
        if selel > n then len := Length(ls) else len := selep - st;

        tw := FFont.TextWidth(copy(ls,1,st));
        tw2 := FFont.TextWidth(copy(ls,1,st+len));
        Canvas.XORFillRectangle(pgfColorToRGB(clSelection) xor $FFFFFF,
           - FDrawOffset + FSideMargin + tw, yp, tw2-tw, LineHeight);
      end;

      //drawing cursor
      if FCursorLine = n then
      begin
        // drawing cursor
        tw := FFont.TextWidth(copy(ls,1,FCursorPos));
        pgfCaret.SetCaret(Canvas, - FDrawOffset + FSideMargin + tw, yp, 1, FFont.Height);
      end

    end;
    
    yp := yp + LineHeight;

    if yp > Height then Break;
  end;
  
  if not Focused then pgfCaret.UnSetCaret(Canvas);

  Canvas.EndDraw;
end;

procedure TlpMemo.HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  prevval : WideString;
  s, ls, ls2 : WideString;
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

  s := WideChar(keycode);

  Consumed := false;
(*
  Consumed := true;
  case pgfCheckClipBoardKey(keycode, shiftstate) of
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
*)

  if not Consumed then
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
(*
                    while (FCursorPos > 0) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);

                    while (FCursorPos > 0) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);
*)
                  end;

                end;
              end;
      KEY_RIGHT:
              begin  // right
                if FCursorPos < length(CurrentLine) then
                begin
                  inc(FCursorPos);

                  if (shiftstate and ss_control) <> 0 then
                  begin
                    // word search...
(*
                    while (FCursorPos < length(CurrentLine)) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);

                    while (FCursorPos < length(CurrentLine)) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);
*)
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
                FCursorPos := length(CurrentLine);
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
  end;

  if not Consumed then
  begin
    consumed := true;

    case keycode of
      KEY_ENTER:
              begin // enter
                ls := Copy(FLines[FCursorline-1],1,FCursorPos);
                ls2 := Copy(FLines[FCursorline-1],FCursorPos+1,Length(FLines[FCursorline-1]));
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
                  Delete(ls,FCursorPos,1);
                  SetLineText(FCursorLine,ls);
                  dec(FCursorPos);
                end
                else if FCursorLine > 1 then
                begin
                  ls := CurrentLine;
                  FLines.Delete(FCursorLine-1);
                  dec(FCursorLine);
                  FCursorPos := Length(FLines.Strings[FCursorLine-1]);
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
                if FCursorPos < length(ls) then
                begin
                  Delete(ls,FCursorPos+1,1);
                  SetLineText(FCursorLine,ls);
                end
                else if FCursorLine < LineCount then
                begin
                  ls2 := FLines.Strings[FCursorLine];
                  FLines.Delete(FCursorLine);
                  FLines.Strings[FCursorLine-1] := ls + ls2;
                end;
              end;
    else
      consumed := false;
    end;

    if Consumed then
    begin
      StopSelection;
      AdjustCursor;
    end;
  end;

  if not Consumed and (keycode >= 32) and (keycode < $FF00) then
  begin
    // printeable
    //FText := FText + s;

    if (FMaxLength <= 0) or (length(FLines.Text) < FMaxLength) then
    begin
      DeleteSelection;
      ls := GetLineText(FCursorLine);
      insert(s,ls,FCursorPos+1);
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

procedure TlpMemo.HandleLMouseDown(x, y : integer; shiftstate : word);
var
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  lnum : integer;
  ls : WideString;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  // searching the appropriate character position

  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then lnum := LineCount;
  ls := GetLineText(lnum);

  cpx := FFont.TextWidth(copy(ls,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  for n := 0 to Length(ls) do
  begin
    cx := FFont.TextWidth(copy(ls,1,n)) - FDrawOffset + FSideMargin;
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

procedure TlpMemo.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  lnum : integer;
  ls : WideString;
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

  cpx := FFont.TextWidth(copy(ls,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  for n := 0 to Length(ls) do
  begin
    cx := FFont.TextWidth(copy(ls,1,n)) - FDrawOffset + FSideMargin;
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

(*
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
*)

procedure TlpMemo.HandleResize(dwidth, dheight: integer);
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

function TlpMemo.SelectionText: WideString;
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
  result := '';
end;

function TlpMemo.GetText8: string;
var
  n : integer;
  s : string;
begin
  s := '';
  for n:=1 to LineCount do
  begin
    if n > 1 then s := s + #13#10;
    s := s + wstoUtf8(GetLineText(n));
  end;
  result := s;
end;

function TlpMemo.GetText: WideString;
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

procedure TlpMemo.SetText(const AValue : WideString);
var
  n : integer;
  c : string[2];
  s : WideString;
begin
  FLines.Clear;
  s := '';
  n := 1;
  while n <= length(AValue) do
  begin
    c := copy(AValue,n,1);
    if (c[1] = #13) or (c[1] = #10) then
    begin
      FLines.Add(s);
      s := '';
      c := copy(AValue,n+1,1);
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

procedure TlpMemo.SetText8(const AValue: String);
begin
  text := u8(avalue);
end;

end.

