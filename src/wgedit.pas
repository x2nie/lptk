{ wgedit.pas: Edit widget
  File maintainer: nvitya@freemail.hu

History:
}

unit wgedit;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, messagequeue, gfxbase, GfxStyle, GfxWidget;

type

  TwgEdit = class(TWidget)
  private
    FText : String16;
    FMaxLength : integer;
    FCursorPos : integer;
    FSideMargin : integer;

    FSelStart, FSelOffset : integer;
    FSelecting : boolean;

    FMouseDragPos  : integer;

    FFont : TGfxFont;

    FDrawOffset : integer;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    procedure SetText(const AValue : String16);

    procedure DeleteSelection;

    procedure DoCopy;
    procedure DoPaste;

    procedure AdjustCursor;
    function GetDrawText : string16;
    procedure SetText8(const Value: string);
    function GetText8: string;

  public
    PasswordMode : boolean;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseUp(x,y : integer; button : word; shiftstate : word); override;
    procedure HandleMouseMove(x,y : integer; btnstate, shiftstate : word); override;

    function SelectionText : string16;

    property Font : TGfxFont read FFont;

    property Text8 : string read GetText8 write SetText8;

  public

    OnChange : TNotifyEvent;

  published

    property Text : String16 read FText write SetText;
    property FontName : string read GetFontName write SetFontName;

  end;

function CreateEdit(AOwner : TComponent; x, y, w, h : TGfxCoord) : TwgEdit;

implementation

uses
  GfxClipboard;

function CreateEdit(AOwner : TComponent; x, y, w, h : TGfxCoord) : TwgEdit;
begin
  result := TwgEdit.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := w;
  if h > 0 then Result.Height := h;
end;

{ TwgEdit }

constructor TwgEdit.Create(AOwner : TComponent);
begin
  inherited;
  Focusable := true;
  
  FFont := GfxGetFont('#Edit1');  // owned object !
  
  FHeight := FFont.Height + 6;
  FWidth := 120;
  FBackgroundColor := clBoxColor;

  FSelecting := false;
  FSideMargin := 3;
  FMaxLength := 0;
  FText := '';
//  FText := str8to16('halihoka');
//  FText := FText + #80 + #1;
//  FText := FText + #81 + #1;
  FCursorPos := Length16(FText);
  FSelStart  := FCursorPos;
  FSelOffset := 0;
  FDrawOffset := 0;
  PasswordMode := false;

  FMouseCursor := CUR_EDIT;

  OnChange := nil;
end;

destructor TwgEdit.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TwgEdit.SetText(const AValue : String16);
begin
  if FText=AValue then exit;
  FText:=AValue;
  FCursorPos := Length16(FText);
  FSelStart  := FCursorPos;
  FSelOffset := 0;
  FDrawOffset := 0;
  AdjustCursor;
  if FWinHandle > 0 then RePaint;
end;

function TwgEdit.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgEdit.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

procedure TwgEdit.DeleteSelection;
begin
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Delete16(FText,1+FSelStart + FSelOffset,-FSelOffset);
      FCurSorPos := FSelStart + FSelOffset;
    end
    else
    begin
      Delete16(FText,1+FSelStart,FSelOffset);
      FCurSorPos := FSelStart;
    end;
    FSelOffset := 0;
    FSelStart := FCursorPos;
  end;
end;

procedure TwgEdit.DoCopy;
begin
  if FSelOffset = 0 then Exit;
  SetClipboardText16(SelectionText);
end;

procedure TwgEdit.DoPaste;
var
  s : string16;
begin
  DeleteSelection;
  s := GetClipboardText16;
  if (FMaxLength > 0) then
  begin
    if length16(FText)+length16(s) > FMaxLength then SetLength16(s,FMaxLength-length16(FText));
  end;
  if length16(s) < 1 then Exit;
  Insert16(s, FText, FCursorPos+1);
  FCursorPos := FCursorPos + length16(s);
  AdjustCursor;
  Repaint;
end;

procedure TwgEdit.AdjustCursor;
var
  tw : integer;
  VisibleWidth : integer;
begin
  tw := FFont.TextWidth16(copy16(GetDrawText,1,FCursorPos));

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

function TwgEdit.GetDrawText: string16;
begin
  if not PassWordMode then result := FText
    else result := Str8to16(StringOfChar('*',Length16(FText)));
end;

procedure TwgEdit.RePaint;
var
  r : TGfxRect;
  tw, tw2, st, len : integer;
  dtext : string16;
begin
  if WinHandle <= 0 then Exit;
  //inherited RePaint;

  Canvas.DrawOnBuffer := true;
  
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

  if Enabled
    then Canvas.SetColor(FBackgroundColor)
    else Canvas.SetColor(clWindowBackground);
    
  Canvas.FillRectAngle(2,2,width-4,height-4);

  dtext := GetDrawText;

  Canvas.SetTextColor(clText1);
  Canvas.SetFont(FFont);
  Canvas.DrawString16(- FDrawOffset + FSideMargin, 3, dtext);

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

      tw := FFont.TextWidth16(copy16(dtext,1,st));
      tw2 := FFont.TextWidth16(copy16(dtext,1,st+len));
      Canvas.DrawSelectionRectangle(- FDrawOffset + FSideMargin + tw, 3, tw2-tw, FFont.Height);
    end;

    // drawing cursor
    tw := FFont.TextWidth16(copy16(dtext,1,FCursorPos));
    Canvas.SetColor(clTextCursor);
    Canvas.FillRectangle(- FDrawOffset + FSideMargin + tw, 3, 2, FFont.Height);
  end;

  Canvas.SwapBuffer;
end;

procedure TwgEdit.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean);
var
  prevval : string16;
  s : string16;

  procedure StopSelection;
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

begin

  //inherited;

  //if Consumed then Exit;

  prevval := Text;

  //writeln('wgedit.keypress: ',IntToHex(keycode,4), ' shift: ',IntToHex(shiftstate,4));

  s := chr(keycode and $00FF) + chr((keycode and $FF00) shr 8);

  Consumed := true;
  case GfxCheckClipBoardKey(keycode, shiftstate) of
    ckCopy:   DoCopy;
    ckPaste:  DoPaste;
    ckCut:    begin
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
                    while (FCursorPos > 0) and not GfxIsAlphaNum16(copy16(FText,FCursorPos,1))
                      do Dec(FCursorPos);

                    while (FCursorPos > 0) and GfxIsAlphaNum16(copy16(FText,FCursorPos,1))
                      do Dec(FCursorPos);
                  end;

                end;
              end;
      KEY_RIGHT:
              begin  // right
                if FCursorPos < length16(FText) then
                begin
                  inc(FCursorPos);

                  if (shiftstate and ss_control) <> 0 then
                  begin
                    // word search...
                    while (FCursorPos < length16(FText)) and GfxIsAlphaNum16(copy16(FText,FCursorPos+1,1))
                      do Inc(FCursorPos);

                    while (FCursorPos < length16(FText)) and not GfxIsAlphaNum16(copy16(FText,FCursorPos+1,1))
                      do Inc(FCursorPos);
                  end;
                end;
              end;

      KEY_HOME:
              begin  // home
                FCursorPos := 0;
              end;

      KEY_END:
              begin  // end
                FCursorPos := length16(FText);
              end;
    else
      Consumed := false;
    end;

    if Consumed then
    begin
      AdjustCursor;

      if FSelecting then
      begin
        FSelOffset  := FCursorPos - FSelStart;
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
        KEY_BACKSPACE:
                begin // backspace
                  if FCursorPos > 0 then
                  begin
                    Delete16(FText,FCursorPos,1);
                    dec(FCursorPos);
                  end;
                end;

        KEY_DELETE:
                begin // del
                  if FSelOffset <> 0 then
                  begin
                    DeleteSelection;
                  end
                  else if FCursorPos < length16(FText) then
                  begin
                    Delete16(FText,FCursorPos+1,1);
                  end;
                end;
{
        $FF1B:  begin // ESC
                  halt(0);
                end;
}
      else
        Consumed := false;
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

    if (FMaxLength <= 0) or (length16(FText) < FMaxLength) then
    begin
      DeleteSelection;
      insert16(s,FText,FCursorPos+1);
      inc(FCursorPos);
      FSelStart := FCursorPos;
      AdjustCursor;
    end;

    consumed := true;
  end;

  if prevval <> Text then
  begin
    if Assigned(OnChange) then OnChange(self);
  end;

  if consumed then RePaint
              else inherited;
end;

procedure TwgEdit.HandleMouseDown(x, y : integer; button : word; shiftstate : word);
var
  s : string16;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  dtext : string16;
begin
  inherited HandleMouseDown(x, y, button, shiftstate);

  // searching the appropriate character position

  dtext := GetDrawText;

  cpx := FFont.TextWidth16(copy16(dtext,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(dtext) do
  begin
    cx := FFont.TextWidth16(copy16(dtext,1,n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  FMouseDragPos := cp;

  FCursorPos := cp;

  if (shiftstate and ss_shift) <> 0 then
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

procedure TwgEdit.HandleMouseUp(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseUp(x, y, button, shiftstate);
end;

procedure TwgEdit.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
var
  s : string16;
  n : integer;
  cpx : integer;
  cp : integer;
  cx : integer;
  dtext : string16;
begin

  if (btnstate and 1) = 0 then Exit;

  // searching the appropriate character position
  dtext := GetDrawText;

  cpx := FFont.TextWidth16(copy16(dtext,1,FCursorPos)) - FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(dtext) do
  begin
    cx := FFont.TextWidth16(copy16(dtext,1,n)) - FDrawOffset + FSideMargin;
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

function TwgEdit.SelectionText: string16;
begin
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
end;

function TwgEdit.GetText8: string;
begin
  result := str16to8(FText);
end;

procedure TwgEdit.SetText8(const Value: string);
begin
  Text := str8to16(Value);
end;

end.

