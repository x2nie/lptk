{ wgchoicelist.pas: ChoiceList widget (ComboBox)
  File maintainer: nvitya@freemail.hu

History:
}
unit wgchoicelist;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget, wgScrollBar, popupwindow, wglistbox;

type

  TChoiceListPopup = class(TPopupWindow)
  public
    ListBox : TwgTextListBox;
    ItemRef : TStringList;

    CallerWidget : TWidget;

    procedure DoShow; override;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  end;

  TwgChoiceList = class(TWidget)
  private
    FFocusItem: integer;
    function GetFontName: string;
    procedure SetFocusItem(const Value: integer);
    procedure SetFontName(const AValue: string);
  protected
    FFont : TGfxFont;

    FItems : TStringList;
    FMargin : integer;

    FListPopup : TChoiceListPopup;

    DropDownRows : integer;

    FDroppedDown : boolean;

    procedure ListSelect(sender : TObject);

  public

    property FocusItem : integer read FFocusItem write SetFocusItem;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;

    procedure DropDown;

    procedure HandleMouseDown(x,y : integer; button : word; shiftstate : word); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    function Text : string16;
    function Text8 : string;

    property Font : TGfxFont read FFont;

  public

    OnChange : TNotifyEvent;

  published

    property FontName : string read GetFontName write SetFontName;

    property Items : TStringList read FItems;

  end;

function CreateChoiceList(AOwner : TComponent; x, y, w : TGfxCoord; list8 : TStringList) : TwgChoiceList;

implementation

uses gfxstyle;

function CreateChoiceList(AOwner : TComponent; x, y, w : TGfxCoord; list8 : TStringList) : TwgChoiceList;
var
  n : integer;
begin
  result := TwgChoiceList.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := w;
  if list8 <> nil then
  begin
    for n:=0 to list8.Count-1 do result.Items.Add(str8to16(list8.Strings[n]));
  end;
end;

{ TwgChoiceList }

procedure TwgChoiceList.ListSelect(sender: TObject);
begin
  FocusItem := FListPopup.ListBox.FocusItem;
  FListPopup.Close;
  Repaint;
  if Assigned(OnChange) then OnChange(self);
end;

constructor TwgChoiceList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := GfxGetFont('#List');

  FItems := TStringList.Create;

  FBackgroundColor := clChoiceListBox;
  FFocusable := true;

  FListPopup := TChoiceListPopup.Create(nil);

  FListPopup.ListBox.SetItemref(FItems);
  FListPopup.ListBox.OnSelect := {$ifdef FPC}@{$endif}ListSelect;
  FListPopup.ListBox.HotTrack := true;
  FListPopup.ListBox.BackgroundColor := clChoicelistBox;
  FListPopup.ListBox.PopupFrame := true;

  FListPopup.CallerWidget := self;

  DropDownRows := 8;

  FFocusItem := 1;
  FMargin := 3;

  FHeight := FFont.Height + 2*FMargin;
  FWidth := 120;
  FDroppedDown := false;

  OnChange := nil;

end;

destructor TwgChoiceList.Destroy;
begin
  FItems.Free;
  FListPopup.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TwgChoiceList.RePaint;
var
  r : TGfxRect;
begin
  if FWinHandle <= 0 then Exit;

  Canvas.ClearClipRect;

  DrawControlFrame(canvas,0,0,width,height);

  r.Left := 2;
  r.Top  := 2;
  r.width := width - 4;
  r.height := height - 4;
  canvas.SetClipRect(r);

  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectAngle(2,2,width-4,height-4);
  
  //if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
  //Canvas.DrawRectangle(0,0,width,height);
  
  DrawButtonFace(canvas, width - height + 1, 1, height-2, height-2);
  DrawDirectionArrow(canvas, width - height + 1, 1, height-2, height-2, 1);

  canvas.SetFont(FFont);

  if FFocused then
  begin
    canvas.SetColor(clSelection);
    canvas.SetTextColor(clSelectionText);
  end
  else
  begin
    canvas.SetColor(BackgroundColor);
    canvas.SetTextColor(clText1);
  end;

  r.SetRect(2,2,width-height-2,height-4);
  canvas.FillRect(r); //2,2,width-height-2,height-4);
  canvas.SetClipRect(r);

  if FocusItem > 0 then canvas.DrawString16(FMargin+1,FMargin, Text);

end;

procedure TwgChoiceList.DropDown;
var
  rc : integer;
begin
  FListPopup.Width  := self.Width;
  rc := FItems.Count;
  if rc > DropDownRows then rc := DropDownRows;
  if rc < 1 then rc := 1;
  FListPopup.Height := (FListPopup.ListBox.RowHeight * rc) + 4;

  FDroppedDown := true;
  FListPopup.ShowAt(self.WinHandle,0,height);
  FListPopup.SetDontCloseWidget(self);
  FListPopup.ListBox.FocusItem := self.FocusItem;
end;

procedure TwgChoiceList.HandleMouseDown(x, y: integer; button: word; shiftstate: word);
begin
  inherited HandleMouseDown(x, y, button, shiftstate);
  
  if FListPopup.WinHandle <= 0 then DropDown else FListPopup.Close;
end;

procedure TwgChoiceList.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  consumed := true;
  case keycode of
    KEY_ENTER: begin // enter
                 DropDown;
               end;
  else
    consumed := false;
  end;
end;

function TwgChoiceList.Text: string16;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count)
    then result := FItems.Strings[FocusItem-1]
    else result := '';
end;

function TwgChoiceList.Text8: string;
begin
  result := str16to8(Text);
end;

procedure TwgChoiceList.SetFocusItem(const Value: integer);
begin
  FFocusItem := Value;
  if FFocusItem < 1 then FFocusItem := 1;
  if FFocusItem > FItems.Count then FFocusItem := FItems.Count;
  if FWinHandle > 0 then RePaint;
end;

function TwgChoiceList.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgChoiceList.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

{ TChoiceListPopup }

procedure TChoiceListPopup.DoShow;
begin
  ListBox.SetDimensions(0,0,self.Width,self.height);
  inherited DoShow;
  ActiveWidget := ListBox;
end;

constructor TChoiceListPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ItemRef := nil;
  ListBox := TwgTextListBox.Create(self);
  CallerWidget := nil;
end;

destructor TChoiceListPopup.Destroy;
begin
  ListBox.Free;
  inherited;
end;

procedure TChoiceListPopup.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if keycode = KEY_ESC then
  begin // ESC
    Close;
    consumed := true;
  end
  else if keycode = KEY_ENTER then
  begin // enter
    Writeln('ENTER pressed.');
    if CallerWidget <> nil then
      PostMessage(self, CallerWidget, MSG_KEYPRESS, KEY_TAB, 0, 0 ); //send TAB
  end;
end;

end.

