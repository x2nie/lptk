unit hd_button;

// Button Widget

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, hd_defs, hd_main, hd_widget;

type

  { TwgButton }

  TwgButton = class(TpgfWidget)
  private
    FImageName: string;
    FClicked : Boolean;
    FImage : TpgfImage;
    FOnClick: TNotifyEvent;
    FShowImage : Boolean;
    FClickOnPush : Boolean;
    FDown : Boolean;
    FImageMargin: integer;
    FImageSpacing: integer;
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    FModalResult : integer;
    function GetFontDesc: string;
    procedure SetFontDesc(const AValue: string);
    procedure SetImageName(const AValue: string);
    procedure SetText(const AValue : WideString);
    procedure SetDown(AValue : Boolean);
    procedure SetImageMargin(const Value: integer);
    procedure SetImageSpacing(const Value: integer);
    function GetAllowDown: Boolean;
    procedure SetAllowDown(const Value: Boolean);
    procedure SetAllowAllUp(const Value: boolean);

  protected
    FText : WideString;
    FFont : TpgfFont;
    procedure SetShowImage(AValue : Boolean);

  protected
    procedure HandlePaint; override;

    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean); override;
    procedure HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed : boolean); override;

    procedure HandleLMouseDown(X, Y: Integer; ShiftState: Word); override;
    procedure HandleLMouseUp(x,y : integer; shiftstate : word); override;

    procedure HandleMouseExit; override;
    procedure HandleMouseEnter; override;

  public
    //OnClick : TNotifyEvent;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DoPush;
    procedure DoRelease;

    procedure Click;

    property ShowImage : Boolean read FShowImage write SetShowImage;
    property Down : Boolean read FDown write SetDown;

    property Font : TpgfFont read FFont;

    property AllowDown : Boolean read GetAllowDown write SetAllowDown;

  published

    property Text : WideString read FText write SetText;

    property FontDesc : string read GetFontDesc write SetFontDesc;

    property ImageName : string read FImageName write SetImageName;

    property ImageMargin : integer read FImageMargin write SetImageMargin;
    property ImageSpacing : integer read FImageSpacing write SetImageSpacing;

    property GroupIndex : integer read FGroupIndex write FGroupIndex;
    property AllowAllUp : boolean read FAllowAllUp write SetAllowAllUp;

    property ModalResult : integer read FModalResult write FModalResult;

    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  end;

function CreateButton(AOwner : TComponent; x, y, w : TpgfCoord; txt8 : String; onclk : TNotifyEvent) : TwgButton;

implementation

uses hd_form;

function CreateButton(AOwner : TComponent; x, y, w : TpgfCoord; txt8 : String; onclk : TNotifyEvent) : TwgButton;
begin
  Result := TwgButton.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := utf8(txt8);
  Result.Width := w;
  Result.OnClick := onclk;
end;

{ TwgButton }

procedure TwgButton.SetDown(AValue : Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AllowDown then RePaint;
  end;
end;

procedure TwgButton.SetShowImage(AValue : Boolean);
begin
  if AValue <> FShowImage then
  begin
    FShowImage := AValue;
    if (FImage <> nil) and ShowImage then RePaint;
  end;
end;

procedure TwgButton.SetText(const AValue : WideString);
begin
  if FText=AValue then exit;
  FText:=AValue;
  RePaint;
end;

procedure TwgButton.SetImageName(const AValue: string);
begin
  FImageName:=AValue;
  //FImage := pgfImgLibGetImage(FImageName);
  Repaint;
  //FShowImage := true;
end;

function TwgButton.GetFontDesc: string;
begin
  result := FFont.FontDesc;
end;

procedure TwgButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

constructor TwgButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('Button');
  FFont := pgfGetFont('#Label1');
  FHeight := FFont.Height + 8;
  FWidth := 96;
  FFocusable := True;
  FOnClick := nil;
  FDown := False;
  FClicked := False;
  FDown := False;
  FClickOnPush := False;
  FGroupIndex := 0;
  FImage := nil;
  FImageName := '';
  FShowImage := true;
  FImageMargin := 3;
  FImageSpacing := -1;
  FModalResult := 0;
end;

destructor TwgButton.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgButton.HandlePaint;
var
  AText : WideString;
  x,y,iy,w : integer;
  r : TpgfRect;
  pofs : integer;
begin
  Canvas.BeginDraw;

  Canvas.Clear(clButtonFace);
  //Canvas.Clear(clSelection);
  Canvas.ClearClipRect;

  if not FDown then Canvas.SetColor(clHilite1)
           else Canvas.SetColor(clShadow2);

  Canvas.DrawLine(0,height-2, 0,0);
  Canvas.DrawLine(0,0, width-1,0);

  if not FDown then Canvas.SetColor(clHilite2)
           else Canvas.SetColor(clShadow1);

  Canvas.DrawLine(1,height-3, 1,1);
  Canvas.DrawLine(1,1, width-2,1);

  if not FDown then Canvas.SetColor(clShadow2)
           else Canvas.SetColor(clHilite1);

  Canvas.DrawLine(width-1,1, width-1,height-1);
  Canvas.DrawLine(0,height-1, width-1,height-1);

  if not FDown then Canvas.SetColor(clShadow1)
           else Canvas.SetColor(clHilite2);

  Canvas.DrawLine(width-2,2, width-2,height-2);
  Canvas.DrawLine(1,height-2, width-2,height-2);

  if FFocused then
  begin
    Canvas.SetColor(clSelection);
    Canvas.FillRectangle(3,3,width-5,height-5);
    Canvas.SetTextColor(clSelectionText);
    Canvas.SetColor(clSelectionText);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  r.left := 2;
  r.top  := 2;
  r.width := Width-4;
  r.height := Height-4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  AText := FText;
  y := Height div 2 - FFont.Height div 2;
  if y < 3 then y := 3;

  if FDown then pofs := 1 else pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := Height div 2 - FImage.Height div 2;
    if ImageMargin = -1 then // centered
    begin
      w := FFont.TextWidth(AText) + FImage.Width;
      if FImageSpacing > 0 then inc(w,FImageSpacing);
      x := (width div 2) - (w div 2);
      if x < 3 then x := 3;
    end
    else
    begin
      x := FImageMargin+3;
    end;

    Canvas.DrawImage(x+pofs, iy+pofs, FImage);
    inc(x, FImage.Width);
    if FImageSpacing > 0 then inc(x,FImageSpacing);

    if (FImageSpacing = -1) and (FImageMargin >= 0) then
    begin
      w := (Width-3-x) div 2 - FFont.TextWidth(AText) div 2;
      if w < 1 then w := 1; // minimal spaceing
      x := x + w;
    end;
  end
  else
  begin
    x := (width div 2) - (FFont.TextWidth(AText) div 2);
  end;
  if x < 3 then x := 3;
  Canvas.DrawString(x+pofs, y+pofs, AText);

  Canvas.EndDraw;
end;

procedure TwgButton.DoPush;
var
  n : integer;
  c : TComponent;
begin
  FClickOnPush := (not FDown) and AllowDown;

  // search the other buttons in the group
  for n:=0 to FParent.ComponentCount-1 do
  begin
    c := FParent.Components[n];
    if (c <> self) and (c is TwgButton) then
    begin
      with TwgButton(c) do
      begin
        if GroupIndex = self.GroupIndex then Down := false;
      end;
    end;
  end;

  FDown := True;
  FClicked := True;

  RePaint;

  if FClickOnPush then Click;
end;

procedure TwgButton.DoRelease;
begin
  if AllowDown then
  begin
    if FDown and (not FClickOnPush) and FAllowAllUp then
    begin
      FDown := False;
      RePaint;
      Click;
    end;
  end
  else
  begin
    if FDown and FClicked then Click;
    FDown := FALSE;
    RePaint;
  end;

  FClickOnPush := false;
  FClicked := false;
end;



procedure TwgButton.HandleKeyPress(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  if (keycode = KEYSC_ENTER) or (keycode = KEYSC_SPACE) then
  begin
    DoPush;
    Consumed := true;
  end
  else inherited;
end;

procedure TwgButton.HandleKeyChar(var keycode: word; var shiftstate: word;
  var consumed: boolean);
begin
  if (keycode = KEY_ENTER) or (keycode = KEY_SPACE)
    then Consumed := true
    else inherited;
end;

procedure TwgButton.HandleKeyRelease(var keycode : word; var shiftstate : word; var consumed : boolean);
begin
  if (keycode = KEYSC_ENTER) or (keycode = KEYSC_SPACE) then
  begin
    DoRelease;
    Consumed := true;
  end
  else inherited;
end;

procedure TwgButton.HandleLMouseDown(X, Y: Integer; ShiftState: Word);
begin
  inherited;
  DoPush;
end;

procedure TwgButton.HandleLMouseUp(x, y : integer; shiftstate : word);
begin
  inherited;
  DoRelease;
end;

procedure TwgButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if FDown and (not AllowDown) then
  begin
    FDown := FALSE;
    RePaint;
  end;
end;

procedure TwgButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if FClicked and (not AllowDown) then
  begin
     FDown := TRUE;
     RePaint;
  end;
end;

procedure TwgButton.Click;
var
  pform : TpgfForm;
begin
  pform := WidgetParentForm(self);
  if pform <> nil then pform.ModalResult := self.ModalResult;

  if Assigned(FOnClick) then FOnClick(self);
end;

procedure TwgButton.SetImageMargin(const Value: integer);
begin
  FImageMargin := Value;
  Repaint;
end;

procedure TwgButton.SetImageSpacing(const Value: integer);
begin
  FImageSpacing := Value;
  Repaint;
end;

function TwgButton.GetAllowDown: Boolean;
begin
  result := GroupIndex > 0;
end;

procedure TwgButton.SetAllowDown(const Value: Boolean);
begin
  GroupIndex := 1;
end;

procedure TwgButton.SetAllowAllUp(const Value: boolean);
begin
  FAllowAllUp := Value;
end;


end.
