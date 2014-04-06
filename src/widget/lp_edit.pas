unit lp_edit;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_widget;

type

  TlpEdit = class(TpgfWidget)
  private
    FText : WideString;
    FMaxLength : integer;
    FCursorPos : integer;
    FSideMargin : integer;

    FBackgroundColor : TpgfColor;

    FSelStart, FSelOffset : integer;
    FSelecting : boolean;

    FMouseDragPos  : integer;

    FFont : TpgfFont;

    FDrawOffset : integer;
    FOnChange: TNotifyEvent;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    procedure SetText(const AValue : WideString);

    procedure DeleteSelection;

    procedure DoCopy;
    procedure DoPaste;

    procedure AdjustCursor;
    function GetDrawText : WideString;

  public
    PasswordMode : boolean;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

    procedure HandleKeyChar(var keycode: word; var shiftstate: TShiftState; var consumed : boolean); override;

    procedure HandleLMouseDown(x,y : integer; shiftstate : TShiftState); override;

    procedure HandleMouseMove(x,y : integer; btnstate:word; shiftstate : TShiftState); override;

    function SelectionText : WideString;

    property Font : TpgfFont read FFont;

  public

    //OnChange : TNotifyEvent;

  published

    property Text : WideString read FText write SetText;
    property FontName : string read GetFontName write SetFontName;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

  end;

function CreateEdit(AOwner : TComponent; x, y, w, h : TpgfCoord) : TlpEdit;

implementation

//uses
//  ptkClipboard;

function CreateEdit(AOwner : TComponent; x, y, w, h : TpgfCoord) : TlpEdit;
begin
  result := TlpEdit.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := w;
  if h > 0 then Result.Height := h;
end;

{ TlpEdit }

constructor TlpEdit.Create(AOwner : TComponent);
begin
  inherited;
  Focusable := true;

  FFont := pgfGetFont('#Edit1');  // owned object !

  FHeight := FFont.Height + 6;
  FWidth := 120;
  FBackgroundColor := clBoxColor;

  FSelecting := false;
  FSideMargin := 3;
  FMaxLength := 0;
  FText := '';
  FCursorPos := Length(FText);
  FSelStart  := FCursorPos;
  FSelOffset := 0;
  FDrawOffset := 0;
  PasswordMode := false;

  FCursor := crIBeam;

  FOnChange := nil;
end;

destructor TlpEdit.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TlpEdit.SetText(const AValue : WideString);
begin
  if FText=AValue then exit;
  FText:=AValue;
  FCursorPos := Length(FText);
  FSelStart  := FCursorPos;
  FSelOffset := 0;
  FDrawOffset := 0;
  AdjustCursor;
  if FWinHandle > 0 then RePaint;
end;

function TlpEdit.GetFontName: string;
begin
  result := FFont.FontDesc;
end;

procedure TlpEdit.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

procedure TlpEdit.DeleteSelection;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Delete(FText,1+FSelStart + FSelOffset,-FSelOffset);
      FCurSorPos := FSelStart + FSelOffset;
    end
    else
    begin
      Delete(FText,1+FSelStart,FSelOffset);
      FCurSorPos := FSelStart;
    end;
    FSelOffset := 0;
    FSelStart := FCursorPos;
  end;
end;

procedure TlpEdit.DoCopy;
begin
  if FSelOffset = 0 then Exit;
  //SetClipboardText16(SelectionText);
end;

procedure TlpEdit.DoPaste;
var
  s : WideString;
begin
  DeleteSelection;
  //s := GetClipboardText16;
  if (FMaxLength > 0) then
  begin
    if Length(FText)+Length(s) > FMaxLength then SetLength(s,FMaxLength-Length(FText));
  end;
  if Length(s) < 1 then Exit;
  Insert(s, FText, FCursorPos+1);
  FCursorPos := FCursorPos + Length(s);
  AdjustCursor;
  Repaint;
end;

procedure TlpEdit.AdjustCursor;
var
  tw : integer;
  VisibleWidth : integer;
begin
  tw := FFont.TextWidth(copy(GetDrawText,1,FCursorPos));

  VisibleWidth := (FWidth - 2*FSideMargin);

  if tw - FDrawOffset > VisibleWidth - 2 then
  begin
    FDrawOffset := tw - VisibleWidth + 2;
  end
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then dec(FDrawOffset, 2);
  end;

end;

function TlpEdit.GetDrawText: WideString;
begin
  if not PassWordMode then result := FText
    else result := utf8(StringOfChar('*',Length(FText)));
end;

procedure TlpEdit.HandlePaint;
var
  r : TpgfRect;
  tw, tw2, st, len : integer;
  dtext : WideString;
begin
  Canvas.BeginDraw;

  Canvas.ClearClipRect;

  Canvas.DrawControlFrame(0,0,width,height);
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

  if Enabled
    then Canvas.SetColor(FBackgroundColor)
    else Canvas.SetColor(clWindowBackground);

  Canvas.FillRectAngle(2,2,width-4,height-4);

  dtext := GetDrawText;

  Canvas.SetTextColor(clText1);
  Canvas.SetFont(FFont);
  Canvas.DrawString(- FDrawOffset + FSideMargin, 3, dtext);

  if Focused then
  begin
    // drawing selection
    if FSelOffset <> 0 then
    begin
      len := FSelOffset;
      st := FSelStart;
      if len < 0 then begin
                        st := st + len;
                        len := -len;
                      end;

      tw := FFont.TextWidth(copy(dtext,1,st));
      tw2 := FFont.TextWidth(copy(dtext,1,st+len));
      Canvas.XORFillRectangle(pgfColorToRGB(clSelection) xor $FFFFFF,
        - FDrawOffset + FSideMargin + tw, 3, tw2-tw, FFont.Height);
    end;

    // drawing cursor
    tw := FFont.TextWidth(copy(dtext,1,FCursorPos));
{
    tw2 := FFont.TextWidth(copy(dtext,FCursorPos+1,1));
    if tw2 <= 0 then tw2 := FFont.TextWidth(' ');
}

    //Canvas.SetColor(clTextCursor);
    //Canvas.XORFillRectangle($FFFFFF, - FDrawOffset + FSideMargin + tw, 3, {tw2+}1, FFont.Height);

    pgfCaret.SetCaret(Canvas, - FDrawOffset + FSideMargin + tw, 3, {tw2+}1, FFont.Height);
  end
  else
  begin
    pgfCaret.UnSetCaret(Canvas);
  end;

  Canvas.EndDraw;
end;

procedure TlpEdit.HandleKeyChar(var keycode: word; var shiftstate: TShiftState; var consumed : boolean);
var
  prevval : WideString;
  s : WideString;

  procedure StopSelection;
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

begin
  //inherited;

  //if Consumed then Exit;

  prevval := Text;

  //writeln('wgedit.keychar: ',IntToHex(keycode,4), ' shift: ',IntToHex(shiftstate,4));

  s := WideChar(keycode);

  Consumed := false;
{
  Consumed := true;
  case ptkCheckClipBoardKey(keycode, shiftstate) of
    ckCopy:   DoCopy;
    ckPaste:  DoPaste;
    ckCut:    begin
                DoCopy;
                DeleteSelection;
              end;
  else
    Consumed := false;
  end;
}


  if not Consumed then
  begin
    // checking for movement keys:
    consumed := true;

    case keycode of

      KEY_LEFT:
        begin
          if FCursorPos > 0 then
          begin
            dec(FCursorPos);

            if ssCtrl in shiftstate  then
            begin
              // word search...
//                    while (FCursorPos > 0) and not ptkIsAlphaNum(copy(FText,FCursorPos,1))
//                      do Dec(FCursorPos);

//                    while (FCursorPos > 0) and ptkIsAlphaNum(copy(FText,FCursorPos,1))
//                      do Dec(FCursorPos);
            end;

          end;
        end;

      KEY_RIGHT:
        begin
          if FCursorPos < Length(FText) then
          begin
            inc(FCursorPos);

            if ssCtrl in shiftstate  then
            begin
              // word search...
//                    while (FCursorPos < Length(FText)) and ptkIsAlphaNum(copy(FText,FCursorPos+1,1))
//                      do Inc(FCursorPos);

//                    while (FCursorPos < Length(FText)) and not ptkIsAlphaNum(copy(FText,FCursorPos+1,1))
//                      do Inc(FCursorPos);
            end;
          end;
        end;

      KEY_HOME:
        begin
          FCursorPos := 0;
        end;

      KEY_END:
        begin
          FCursorPos := Length(FText);
        end;
    else
      Consumed := false;
    end;

    if Consumed then
    begin
      AdjustCursor;

      FSelecting := ssShift in shiftstate;

      if FSelecting then
      begin
        FSelOffset  := FCursorPos - FSelStart;
      end
      else
      begin
        StopSelection;
      end;
    end;
  end; // movement key checking

  if not Consumed then
  begin
    consumed := true;

    case keycode of
      KEY_BACKSPACE:
        begin // backspace
          if FCursorPos > 0 then
          begin
            Delete(FText,FCursorPos,1);
            dec(FCursorPos);
          end;
        end;

      KEY_DELETE:
        begin
          if FSelOffset <> 0 then
          begin
            DeleteSelection;
          end
          else if FCursorPos < Length(FText) then
          begin
            Delete(FText,FCursorPos+1,1);
          end;
        end;
    else
      Consumed := false;
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
    if (FMaxLength <= 0) or (Length(FText) < FMaxLength) then
    begin
      DeleteSelection;
      insert(s,FText,FCursorPos+1);
      inc(FCursorPos);
      FSelStart := FCursorPos;
      AdjustCursor;
    end;

    consumed := true;
  end;

  if prevval <> Text then
  begin
    if Assigned(FOnChange) then FOnChange(self);
  end;

  if consumed then RePaint
              else inherited;
end;

procedure TlpEdit.HandleLMouseDown(x, y : integer; shiftstate : Tshiftstate  );
var
  s : WideString;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  dtext : WideString;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  // searching the appropriate character position

  dtext := GetDrawText;

  cpx := FFont.TextWidth(copy(dtext,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length(dtext) do
  begin
    cx := FFont.TextWidth(copy(dtext,1,n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  FMouseDragPos := cp;

  FCursorPos := cp;

  if ssShift in shiftstate then
  begin
    FSelOffset  := FCursorPos - FSelStart;
  end
  else
  begin
    FSelStart  := cp;
    FSelOffset := 0;
  end;
  Repaint;
end;

procedure TlpEdit.HandleMouseMove(x, y: integer; btnstate:Word; shiftstate: TShiftstate);
var
  s : WideString;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  dtext : WideString;
begin

  if (btnstate and MOUSE_LEFT) = 0 then Exit;

  // searching the appropriate character position
  dtext := GetDrawText;

  cpx := FFont.TextWidth(copy(dtext,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length(dtext) do
  begin
    cx := FFont.TextWidth(copy(dtext,1,n)) - FDrawOffset + FSideMargin;
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
end;

function TlpEdit.SelectionText: WideString;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Result := Copy(FText,1+FSelStart + FSelOffset,-FSelOffset);
    end
    else
    begin
      result := Copy(FText,1+FSelStart,FSelOffset);
    end;
  end
  else Result := '';
end;

end.

