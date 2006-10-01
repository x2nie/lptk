{ $Id$ }

unit wglabel;

// Label Widget

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, lptk, ptkwidget;

type
  TwgLabel = class(TptkWidget)
  private
    FColor: TptkColor;
    function GetFontName: string;
    procedure SetColor(const AValue: TptkColor);
    procedure SetFontName(const AValue: string);
    procedure SetText(const AValue : widestring);
    procedure SetText8(const AValue: string);
    function GetText8: string;
  protected
    FText : widestring;
    FFont : TptkFont;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;
    property Font : TptkFont read FFont;
    property Text8 : string read GetText8 write SetText8;
                           
  published

    property Text : widestring read FText write SetText;

    property FontName : string read GetFontName write SetFontName;
    
    property Color : TptkColor read FColor write SetColor;
  end;

  TLabelClass = class of TwgLabel;

function CreateLabel(AOwner : TComponent; x, y : TptkCoord; txt8 : String) : TwgLabel;

implementation

function CreateLabel(AOwner : TComponent; x, y : TptkCoord; txt8 : String) : TwgLabel;
begin
  Result := TwgLabel.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := utf8(txt8);
  Result.Width := Result.Font.TextWidth(Result.Text);
end;

{ TwgLabel }

procedure TwgLabel.SetColor(const AValue: TptkColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  RePaint;
end;

function TwgLabel.GetFontName: string;
begin
  result := FFont.FontName;
end;

procedure TwgLabel.SetFontName(const AValue: string);
begin
  FFont.Free;
  FFont := ptkGetFont(AValue);
  if Windowed then RePaint;
end;

procedure TwgLabel.SetText(const AValue : widestring);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

function TwgLabel.GetText8: string;
begin
  result := wstoutf8(Text);
end;

procedure TwgLabel.SetText8(const AValue: string);
begin
  Text := utf8(AValue);
end;

constructor TwgLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := utf8('Label');
  FFont := ptkGetFont('#Label1');
  FHeight := FFont.Height;
  FWidth := 80;
  FColor := clText1;
end;

destructor TwgLabel.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TwgLabel.RePaint;
begin
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FColor);
  Canvas.DrawString(0,0, FText);
end;

end.

(*-------------------------------------------------
$Log$
Revision 1.2  2006/10/01 22:02:41  nvitya
style updates mostly

Revision 1.1.1.1  2006/09/23 23:21:27  nvitya
initial import

-------------------------------------------------*)

