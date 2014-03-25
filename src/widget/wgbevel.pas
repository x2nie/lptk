{ wglistbox.pas: Bevel widget for LPTK by nvitya@freemail.hu
History:
    10.09.2012  Translated to PasGF by Preben Bjorn Biermann Madsen
}

unit wgBevel;

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils, pgf_defs, pgf_main, pgf_utils, pgf_Widget;
  
type

  TBevelShape = ( bsBottomLine, bsBox, bsFrame, bsLeftLine,
                  bsRightLine, bsTopLine );
  TBevelStyle = ( bsLowered, bsRaised );

  TwgBevel = class(TpgfWidget)
  private
    FBevelShape : TBevelShape;
    FBevelStyle : TBevelStyle;
    procedure SetBevelShape(Value: TBevelShape);
    procedure SetBevelStyle(Value: TBevelStyle);
  protected
    FBackgroundColor : TpgfColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandlePaint; override;
  published
    property Shape: TBevelShape read FBevelShape write SetBevelShape;
    property Style: TBevelStyle read FBevelStyle write SetBevelStyle;
  end;

function CreateBevel(AOwner: TComponent;
           Left, Top, Width, Height: TpgfCoord;
           Shape: TBevelShape;
           Style: TBevelStyle) : TwgBevel;
  
implementation

function CreateBevel(AOwner: TComponent; Left, Top, Width, Height: TpgfCoord;
                     Shape: TBevelShape; Style: TBevelStyle) : TwgBevel;
begin
  Result := TwgBevel.Create(AOwner);
  Result.SetPosition(Left, Top, Width, Height);
  Result.Shape := Shape;
  Result.Style := Style;
end;

constructor TwgBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Shape := bsBox;
  Style := bsRaised;
  FWidth := 80;
  FHeight := 80;
  FBackgroundColor := clWindowBackground;  
end;

destructor TwgBevel.Destroy;
begin
  inherited Destroy; 
end;

procedure TwgBevel.HandlePaint;
begin
  Canvas.BeginDraw; 

  Canvas.Clear(FBackgroundColor);
  Canvas.SetLineStyle(2,lsSolid);
  Canvas.SetColor(FBackgroundColor);
  Canvas.DrawRectangle(1, 1, Width - 1, Height - 1);
  Canvas.SetLineStyle(1,lsSolid);

  if Style = bsRaised then Canvas.SetColor(clHilite2)
                      else Canvas.SetColor(clShadow2);
  if Shape in [bsBox, bsFrame, bsTopLine] then
    Canvas.DrawLine(0, 0, Width - 1, 0);
  if Shape in [bsBox, bsFrame, bsLeftLine] then
    Canvas.DrawLine(0, 1, 0, Height - 1);
  if Shape in [bsFrame, bsRightLine] then
    Canvas.DrawLine(Width - 2, 1, Width - 2, Height - 1); 
  if Shape in [bsFrame, bsBottomLine] then
    Canvas.DrawLine(1, Height - 2, Width - 1, Height - 2);  
  if Style = bsRaised then Canvas.SetColor(clShadow2)
                      else Canvas.SetColor(clHilite2);
  if Shape in [bsFrame, bsTopLine] then
    Canvas.DrawLine(1, 1, Width - 2, 1);
  if Shape in [bsFrame, bsLeftLine] then
    Canvas.DrawLine(1, 2, 1, Height - 2);
  if Shape in [bsBox, bsFrame, bsRightLine] then
    Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1); 
  if Shape in [bsBox, bsFrame, bsBottomLine] then
    Canvas.DrawLine(0, Height - 1, Width, Height - 1);
    
   Canvas.EndDraw;
end;

procedure TwgBevel.SetBevelShape(Value: TBevelShape);
begin
  FBevelShape := Value;
  if FWinHandle > 0 then RePaint;
end;

procedure TwgBevel.SetBevelStyle(Value: TBevelStyle);
begin
  FBevelStyle := Value;
  if FWinHandle > 0 then RePaint;
end;

end.

