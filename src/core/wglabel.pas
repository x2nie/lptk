unit wglabel;

// Label Widget

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, hd_defs, hd_main, hd_widget;

type

  TwgLabel = class(TpgfWidget)
  private
    FBackgroundColor: TpgfColor;
    FColor: TpgfColor;
    function GetFontDesc: string;
    procedure SetBackgroundColor(const AValue: TpgfColor);
    procedure SetFontDesc(const AValue: string);

    procedure SetColor(const AValue: TpgfColor);
    procedure SetText(const AValue : widestring);
  protected
    FText : widestring;
    FFont : TpgfFont;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint; override;

  public
    property Font : TpgfFont read FFont;

  published

    property Text : widestring read FText write SetText;

    property FontDesc : string read GetFontDesc write SetFontDesc;

    property Color : TpgfColor read FColor write SetColor;
    property BackgroundColor : TpgfColor read FBackgroundColor write SetBackgroundColor;
  end;

  TLabelClass = class of TwgLabel;

function CreateLabel(AOwner : TComponent; x, y : TpgfCoord; txt8 : String) : TwgLabel;

implementation

function CreateLabel(AOwner : TComponent; x, y : TpgfCoord; txt8 : String) : TwgLabel;
begin
  Result := TwgLabel.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := utf8(txt8);
  Result.Width := Result.Font.TextWidth(Result.Text);
end;

{ TwgLabel }

procedure TwgLabel.SetColor(const AValue: TpgfColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  RePaint;
end;

function TwgLabel.GetFontDesc: string;
begin
  result := FFont.FontDesc;
end;

procedure TwgLabel.SetBackgroundColor(const AValue: TpgfColor);
begin
  if FBackgroundColor = AValue then exit;
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TwgLabel.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := pgfGetFont(AValue);
  RePaint;
end;

procedure TwgLabel.SetText(const AValue : widestring);
begin
  if FText = AValue then exit;
  FText := AValue;
  RePaint;
end;

constructor TwgLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := utf8('Label');
  FFont := pgfGetFont('#Label1');
  FHeight := FFont.Height;
  FWidth := 80;
  FColor := clText1;
  FBackgroundColor := clWindowBackground;
end;

destructor TwgLabel.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgLabel.HandlePaint;
begin
  Canvas.BeginDraw;
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FColor);
  Canvas.DrawString(0,0, FText);
  Canvas.EndDraw;
end;

end.

