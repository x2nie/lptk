unit lp_combobox;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lp_defs, lp_main, lp_widget, lp_popup, lp_listbox;

type

  { TChoiceListPopup }

  TChoiceListPopup = class(TpgfPopupWindow)
  public
    ListBox : TwgTextListBox;
    ItemRef : TStringList;

    CallerWidget : TpgfWidget;

    procedure HandleShow; override;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

  end;

  { TwgChoiceList }

  TlpCombobox = class(TpgfWidget)
  private
    FFocusItem: integer;
    function GetFontName: string;
    procedure SetFocusItem(const Value: integer);
    procedure SetFontName(const AValue: string);
    procedure SetBackgroundColor(color : TpgfColor);
  protected
    FFont : TpgfFont;
    FBackgroundColor: TpgfColor;

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

    procedure HandlePaint; override;

    procedure DropDown;

    procedure HandleLMouseDown(x,y : integer; shiftstate : word); override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    function Text : widestring;
    function Text8 : string;

    property Font : TpgfFont read FFont;
    property BackgroundColor : TpgfColor read FBackgroundColor write SetBackgroundColor;

  public

    OnChange : TNotifyEvent;

  published

    property FontName : string read GetFontName write SetFontName;

    property Items : TStringList read FItems;

  end;

function CreateChoiceList(AOwner : TComponent; x, y, w : TpgfCoord; list8 : TStringList) : TlpCombobox;

implementation

function CreateChoiceList(AOwner : TComponent; x, y, w : TpgfCoord; list8 : TStringList) : TlpCombobox;
var
  n : integer;
begin
  result := TlpCombobox.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Width := w;
  if list8 <> nil then
  begin
    for n:=0 to list8.Count-1 do result.Items.Add(list8.Strings[n]);
  end;
end;

{ TlpCombobox }

procedure TlpCombobox.ListSelect(sender: TObject);
begin
  FocusItem := FListPopup.ListBox.FocusItem;
  FListPopup.Close;
  Repaint;
  if Assigned(OnChange) then OnChange(self);
end;

constructor TlpCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := PgfGetFont('#List');

  FItems := TStringList.Create;

  FBackgroundColor := clChoiceListBox;
  FFocusable := true;

  FListPopup := TChoiceListPopup.Create(nil);

  if not (csDesigning in ComponentState) then
  begin
  FListPopup.ListBox.SetItemref(FItems);
  FListPopup.ListBox.OnSelect := ListSelect;
  FListPopup.ListBox.HotTrack := true;
  FListPopup.ListBox.BackgroundColor := clChoicelistBox;
  FListPopup.ListBox.PopupFrame := true;

  FListPopup.CallerWidget := self;

  end;

  DropDownRows := 8;

  FFocusItem := 1;
  FMargin := 3;

  FHeight := FFont.Height + 2*FMargin;
  FWidth := 120;
  FDroppedDown := false;

  OnChange := nil;

end;

destructor TlpCombobox.Destroy;
begin
  FItems.Free;
  if assigned(FListPopup) then
     FListPopup.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TlpCombobox.HandlePaint;
var
  r : TpgfRect;
begin
  if not HasHandle then Exit;

  Canvas.BeginDraw;

  Canvas.ClearClipRect;

  Canvas.DrawControlFrame(0,0,width,height);

  r.Left := 2;
  r.Top  := 2;
  r.width := width - 4;
  r.height := height - 4;
  canvas.SetClipRect(r);

  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectAngle(2,2,width-4,height-4);
  
  //if Focused then Canvas.SetColor(clWidgetFrame) else Canvas.SetColor(clInactiveWgFrame);
  //Canvas.DrawRectangle(0,0,width,height);
  
  Canvas.DrawButtonFace(width - height + 1, 1, height-2, height-2);
  Canvas.DrawDirectionArrow(width - height + 1, 1, height-2, height-2, 1);

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

  if FocusItem > 0 then canvas.DrawString(FMargin+1,FMargin, Text);

  Canvas.EndDraw;
end;

procedure TlpCombobox.DropDown;
var
  rc : integer;
begin
  FListPopup.Width  := self.Width;
  rc := FItems.Count;
  if rc > DropDownRows then rc := DropDownRows;
  if rc < 1 then rc := 1;
  FListPopup.Height := (FListPopup.ListBox.RowHeight * rc) + 4;

  FDroppedDown := true;
  FListPopup.ShowAt(self,0,height);
  FListPopup.SetDontCloseWidget(self);
  FListPopup.ListBox.FocusItem := self.FocusItem;
end;

procedure TlpCombobox.HandleLMouseDown(x, y : integer; shiftstate : word);
begin
  inherited;
  
  if not FListPopup.HasHandle then DropDown else FListPopup.Close;
end;

procedure TlpCombobox.HandleKeyPress(var keycode: word; var shiftstate: word; var consumed: boolean);
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

function TlpCombobox.Text : widestring;
begin
  if (FocusItem > 0) and (FocusItem <= FItems.Count)
    then result := FItems.Strings[FocusItem-1]
    else result := '';
end;

function TlpCombobox.Text8: string;
begin
  result := wstoutf8(Text);
end;

procedure TlpCombobox.SetFocusItem(const Value: integer);
begin
  FFocusItem := Value;
  if FFocusItem < 1 then FFocusItem := 1;
  if FFocusItem > FItems.Count then FFocusItem := FItems.Count;
  if FWinHandle > 0 then RePaint;
end;

function TlpCombobox.GetFontName: string;
begin
  result := FFont.FontDesc;
end;

procedure TlpCombobox.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := PgfGetFont(AValue);
  RePaint;
end;

procedure TlpCombobox.SetBackgroundColor(color : TpgfColor);
begin
  FBackgroundColor := color;
  RePaint;
end;

{ TChoiceListPopup }

procedure TChoiceListPopup.HandleShow;
begin
  ListBox.SetPosition(0,0,self.Width,self.height);
  inherited;
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
    {
    Writeln('ENTER pressed.');
    if CallerWidget <> nil then
      PostMessage(self, CallerWidget, MSG_KEYPRESS, KEY_TAB, 0, 0 ); //send TAB
    }
  end;
end;

end.

