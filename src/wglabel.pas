{ wglabel.pas: Label widget
  File maintainer: nvitya@freemail.hu

History:
}
unit wglabel;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, schar16, gfxbase, messagequeue, gfxwidget;

type
  TwgLabel = class(TWidget)
  private
    FColor: TGfxColor;
    function GetFontName: string;
    procedure SetColor(const AValue: TGfxColor);
    procedure SetFontName(const AValue: string);
    procedure SetText(const AValue : String16);
    procedure SetText8(const AValue: string);
    function GetText8: string;
  protected
    FText : String16;
    FFont : TGfxFont;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RePaint; override;
    property Font : TGfxFont read FFont;
    property Text8 : string read GetText8 write SetText8;
                           
  published

    property Text : String16 read FText write SetText;

    property FontName : string read GetFontName write SetFontName;
    
    property Color : TGfxColor read FColor write SetColor;
  end;

  TLabelClass = class of TwgLabel;

function CreateLabel(AOwner : TComponent; x, y : TGfxCoord; txt : String) : TwgLabel;

implementation

uses gfxstyle;

function CreateLabel(AOwner : TComponent; x, y : TGfxCoord; txt : String) : TwgLabel;
begin
  Result := TwgLabel.Create(AOwner);
  Result.Left := x;
  Result.Top := y;
  Result.Text := Str8To16(txt);
  Result.Width := Result.Font.TextWidth16(Result.Text);
end;

{ TwgLabel }

procedure TwgLabel.SetColor(const AValue: TGfxColor);
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
  FFont := GfxGetFont(AValue);
  if Windowed then RePaint;
end;

procedure TwgLabel.SetText(const AValue : String16);
begin
  if FText=AValue then exit;
  FText:=AValue;
  if FWinHandle > 0 then RePaint;
end;

function TwgLabel.GetText8: string;
begin
  result := str16to8(Text);
end;

procedure TwgLabel.SetText8(const AValue: string);
begin
  Text := str8to16(AValue);
end;

constructor TwgLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FText := u8('Label');
  FFont := GfxGetFont('#Label1');
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
//var
//  r : TGfxRect;
begin
  //inherited RePaint;
  //writeln('label paint');
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FColor);
  Canvas.DrawString16(0,0, FText);

//  r.SetRect(0,0,width,height);

  {
  r.SetRect(0,0,width div 2,height div 2);
  Canvas.SetClipRect(r);
  r.Left := r.left + r.width;
  r.Top  := r.Top + r.height;
  Canvas.AddClipRect(r);
  Canvas.ClearClipRect;
}

{
  canvas.SetColor(clText1);
  canvas.DrawRectangle(0,0,width,height);

  canvas.FillTriangle(0,0,0,10,5,5);
  canvas.DrawSelectionRectangle(0,0,width div 2,height);
  //canvas.DrawLine(0,0,10,0);
}
end;

end.

